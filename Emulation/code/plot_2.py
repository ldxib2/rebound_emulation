"""
Equivalent of plot_2.R — global and India emissions/capacity plots.
Dependencies: pandas, matplotlib, seaborn, scipy, numpy
"""

import os
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.lines import Line2D
import seaborn as sns
from scipy.stats import gaussian_kde

#%% ── Paths ─────────────────────────────────────────────────────────────────────

script_dir = os.path.dirname(os.path.abspath(__file__))
wd = os.path.dirname(os.path.dirname(script_dir))
os.chdir(wd)

config_path = "Emulation/code/config/config.json"
with open(config_path) as f:
    config = json.load(f)

data_path = "Emulation/data/"
output_dir = os.path.join(data_path, "predictions")
figures_dir = os.path.join(data_path, "figures")
os.makedirs(figures_dir, exist_ok=True)

gbl_preds = "preds_combined_fac.csv"
df_gbl = pd.read_csv(os.path.join(output_dir, gbl_preds))

# list cols where rebound vars are obsolete due to no rebound switch
cols = ['trans_rebound', 'heat_rebound']
df_gbl.loc[df_gbl['emulator'].str.contains('No_RB', na=False), cols] = np.nan

df_gbl[df_gbl['emulator'].str.contains('No_RB', na=False)].head()


# Remove mixed scenarios (where one turnover is high and the other low) to reduce size
df = df_gbl[df_gbl['combined'].str.contains('Mixed', na=False) == False]



#%% ── Policy factor helper ───────────────────────────────────────────────────────

# def add_policy_factors(df: pd.DataFrame) -> pd.DataFrame:
#     df = df.copy()

#     # reb_level_tri
#     conditions = [
#         df["reb_switch"] == 0,
#         (df["reb_switch"] == 1) & (df["heat_rebound"] == 0) & (df["trans_rebound"] == 0),
#         (df["reb_switch"] == 1) & (df["heat_rebound"] == 1) & (df["trans_rebound"] == 1),
#     ]
#     choices = ["no rebound", "low rebound", "high rebound"]
#     df["reb_level_tri"] = pd.Categorical(
#         np.select(conditions, choices, default=np.nan),
#         categories=["no rebound", "low rebound", "high rebound"],
#         ordered=True,
#     )

#     # reb_active
#     df["reb_active"] = pd.Categorical(
#         np.where(df["reb_switch"] == 0, "no rebound", "rebound"),
#         categories=["no rebound", "rebound"],
#         ordered=True,
#     )

#     # turn_level_bi
#     cond_turn_bi = [
#         (df["trans_turnover"] == 0) & (df["heat_turnover"] == 0),
#         (df["trans_turnover"] == 1) & (df["heat_turnover"] == 1),
#     ]
#     df["turn_level_bi"] = pd.Categorical(
#         np.select(cond_turn_bi, ["low turnover", "high turnover"], default=np.nan),
#         categories=["low turnover", "high turnover"],
#         ordered=True,
#     )

#     # leads
#     cond_leads = [
#         (df["lead_solar"] < 0.5) & (df["lead_onshore"] < 0.5),
#         (df["lead_solar"] >= 0.5) & (df["lead_onshore"] >= 0.5),
#     ]
#     df["leads"] = pd.Categorical(
#         np.select(cond_leads, ["Short tech lead", "Long tech lead"], default=np.nan),
#         categories=["Short tech lead", "Long tech lead"],
#         ordered=True,
#     )

#     # cr_total
#     cond_cr = [
#         (df["cr_wind"] < 0.5) & (df["cr_solar"] < 0.5),
#         (df["cr_solar"] >= 0.5) & (df["cr_wind"] >= 0.5),
#     ]
#     df["cr_total"] = pd.Categorical(
#         np.select(cond_cr, ["Low Cannibalisation", "High Cannibalisation"], default=np.nan),
#         categories=["Low Cannibalisation", "High Cannibalisation"],
#         ordered=True,
#     )

#     # turn_level
#     cond_turn = [
#         (df["trans_turnover"] < 0.5) & (df["heat_turnover"] < 0.5),
#         (df["trans_turnover"] >= 0.5) & (df["heat_turnover"] >= 0.5),
#     ]
#     df["turn_level"] = pd.Categorical(
#         np.select(cond_turn, ["low turnover", "high turnover"], default=np.nan),
#         categories=["low turnover", "high turnover"],
#         ordered=True,
#     )

#     # reb_level
#     cond_reb = [
#         (df["trans_rebound"] < 0.5) & (df["heat_rebound"] < 0.5),
#         (df["trans_rebound"] >= 0.5) & (df["heat_rebound"] >= 0.5),
#     ]
#     df["reb_level"] = pd.Categorical(
#         np.select(cond_reb, ["low rebound", "high rebound"], default=np.nan),
#         categories=["low rebound", "high rebound"],
#         ordered=True,
#     )

#     # power_transition (depends on cr_total and leads already set)
#     cond_pt = [
#         (df["cr_total"] == "High Cannibalisation") & (df["leads"] == "Long tech lead"),
#         (df["cr_total"] == "Low Cannibalisation") & (df["leads"] == "Short tech lead"),
#     ]
#     df["power_transition"] = pd.Categorical(
#         np.select(cond_pt, ["slow transition", "fast transition"], default=np.nan),
#         categories=["slow transition", "fast transition"],
#         ordered=True,
#     )

#     return df

# # Apply
# df_gbl = add_policy_factors(df_gbl)

#%% ── Apply theme ─────────────────────────────────────────────────────────────

THEME = dict(
    title_fontsize=18,
    axis_title_fontsize=14,
    axis_tick_fontsize=12,
    strip_fontsize=13,
    legend_fontsize=12,
)


def apply_common_theme(ax):
    ax.set_xlabel(ax.get_xlabel(), fontsize=THEME["axis_title_fontsize"], fontweight="bold")
    ax.set_ylabel(ax.get_ylabel(), fontsize=THEME["axis_title_fontsize"], fontweight="bold")
    for label in ax.get_xticklabels():
        label.set_fontsize(THEME["axis_tick_fontsize"])
        label.set_fontweight("bold")
        label.set_rotation(45)
        label.set_ha("right")
    for label in ax.get_yticklabels():
        label.set_fontsize(THEME["axis_tick_fontsize"])


#%% ── Fig 1: Emissions — high/low rebound vs no rebound ─────────────────────────

palette_viridis = sns.color_palette("magma", n_colors=2)

summary_stats = (
    df.groupby(["rebound", "turnover"], observed=True)["prediction"]
    .mean()
    .reset_index()
    .rename(columns={"prediction": "mean_pred"})
)
#%%


#%%
turn_levels = df["turnover"].unique().tolist()
fig, axes = plt.subplots(1, len(turn_levels), figsize=(10, 8), sharey=False)
if len(turn_levels) == 1:
    axes = [axes]

for ax, tl in zip(axes, turn_levels):
    subset = df[df["turnover"] == tl]
    for i, rl in enumerate(df["rebound"].unique().tolist()):
        grp = subset[subset["rebound"] == rl]["prediction"].dropna()
        if grp.empty:
            continue
        grp.plot.kde(ax=ax, label=rl, color=palette_viridis[i], alpha=0.5)
        ax.fill_between(
            np.linspace(grp.min(), grp.max(), 200),
            gaussian_kde(grp)(np.linspace(grp.min(), grp.max(), 200)),
            alpha=0.3,
            color=palette_viridis[i],
        )
        vline_val = summary_stats.loc[
            (summary_stats["rebound"] == rl) & (summary_stats["turnover"] == tl),
            "mean_pred",
        ]
        if not vline_val.empty:
            ax.axvline(vline_val.values[0], color=palette_viridis[i], linestyle="--", linewidth=0.8)

    ax.set_title(tl, fontsize=THEME["strip_fontsize"])
    ax.set_xlabel(r"Emissions Levels (GtCO$_2$ cumulative)", fontsize=THEME["axis_title_fontsize"], fontweight="bold")
    ax.set_ylabel("Density", fontsize=THEME["axis_title_fontsize"], fontweight="bold")
    apply_common_theme(ax)

handles = [
    mpatches.Patch(color=palette_viridis[i], label=lbl, alpha=0.5)
    for i, lbl in enumerate(df["rebound"].cat.categories)
]
fig.legend(handles=handles, loc="lower center", ncol=2, fontsize=THEME["legend_fontsize"],
           title="Rebound level", title_fontsize=THEME["legend_fontsize"], bbox_to_anchor=(0.5, 0))
fig.tight_layout(rect=[0, 0.06, 1, 1])
fig.savefig(os.path.join(figures_dir, "emiss_gbl.png"), dpi=100)
plt.close(fig)
print("Saved emiss_gbl.png")

#%%
# ── Fig 2: Emissions under power system scenarios ─────────────────────────────

summary_stats = (
    df_gbl.groupby(["reb_switch", "leads", "cr_total"], observed=True)["prediction"]
    .mean()
    .reset_index()
    .rename(columns={"prediction": "median_pred"})
    .dropna()
)

plot_df2 = df_gbl.dropna(subset=["leads", "cr_total"])
leads_levels = plot_df2["leads"].cat.categories.tolist()
cr_levels = plot_df2["cr_total"].cat.categories.tolist()

fig, axes = plt.subplots(len(leads_levels), len(cr_levels), figsize=(10, 8), squeeze=False)

for r, lead in enumerate(leads_levels):
    for c, cr in enumerate(cr_levels):
        ax = axes[r][c]
        subset = plot_df2[(plot_df2["leads"] == lead) & (plot_df2["cr_total"] == cr)]
        for i, ra in enumerate(df["rebound"].cat.categories):
            grp = subset[subset["rebound"] == ra]["prediction"].dropna()
            if not grp.empty:
                grp.plot.kde(ax=ax, label=ra, alpha=0.5)
                vline_row = summary_stats[
                    (summary_stats["leads"] == lead) & (summary_stats["cr_total"] == cr)
                ]
                for _, row in vline_row.iterrows():
                    ax.axvline(row["mean_pred"], linestyle="--", linewidth=0.6, color="grey")

        if r == 0:
            ax.set_title(cr, fontsize=THEME["strip_fontsize"])
        if c == 0:
            ax.set_ylabel(lead, fontsize=THEME["strip_fontsize"])
        ax.set_xlabel(r"Emissions Levels (GtCO$_2$ cumulative)", fontsize=10)
        apply_common_theme(ax)

fig.tight_layout()
fig.savefig(os.path.join(figures_dir, "emiss_power_scens.png"), dpi=100)
plt.close(fig)
print("Saved emiss_power_scens.png")

#%%
# ── Fig 3: Emissions under set power scens with turnover ──────────────────────

summary_stats_pt = (
    df_gbl.groupby(["reb_level", "turn_level", "power_transition"], observed=True)["prediction"]
    .mean()
    .reset_index()
    .rename(columns={"prediction": "median_pred"})
    .dropna()
)

plot_df3 = df_gbl.dropna(subset=["power_transition", "turn_level", "reb_level"])
pt_levels = plot_df3["power_transition"].cat.categories.tolist()
tl_levels = plot_df3["turn_level"].cat.categories.tolist()
rl_levels = plot_df3["reb_level"].cat.categories.tolist()

n_rows = len(tl_levels) * len(rl_levels)
n_cols = len(pt_levels)

fig, axes = plt.subplots(n_rows, n_cols, figsize=(10, 8), squeeze=False)

row_combos = [(tl, rl) for tl in tl_levels for rl in rl_levels]

for r, (tl, rl) in enumerate(row_combos):
    for c, pt in enumerate(pt_levels):
        ax = axes[r][c]
        subset = plot_df3[
            (plot_df3["turn_level"] == tl)
            & (plot_df3["reb_level"] == rl)
            & (plot_df3["power_transition"] == pt)
        ]
        for grp_name, grp_df in subset.groupby("reb_active", observed=True):
            vals = grp_df["prediction"].dropna()
            if not vals.empty:
                vals.plot.kde(ax=ax, label=grp_name, alpha=0.5)

        ax.set_xlim(800, 880)
        vline_row = summary_stats_pt[
            (summary_stats_pt["turn_level"] == tl)
            & (summary_stats_pt["reb_level"] == rl)
            & (summary_stats_pt["power_transition"] == pt)
        ]
        for _, row in vline_row.iterrows():
            ax.axvline(row["median_pred"], linestyle="--", linewidth=0.6, color="grey")

        if r == 0:
            ax.set_title(pt, fontsize=THEME["strip_fontsize"])
        if c == 0:
            ax.set_ylabel(f"{tl}\n{rl}", fontsize=9)
        ax.set_xlabel(r"Emissions (GtCO$_2$ cum.)", fontsize=9)
        apply_common_theme(ax)

fig.tight_layout()
fig.savefig(os.path.join(figures_dir, "emiss_power_turnover.png"), dpi=100)
plt.close(fig)
print("Saved emiss_power_turnover.png")

# %%

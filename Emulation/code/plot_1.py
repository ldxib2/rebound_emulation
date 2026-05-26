"""
FTT Emulator Plots — Python/Matplotlib equivalents of the R/ggplot2 script.

Usage:
    python plots.py

Expects:
    data/predictions/bases_preds_lhc.csv   (columns include: sample_id, Year, Value,
        emulator, trans_turnover, heat_turnover, lead_solar, lead_onshore,
        cr_wind, cr_solar, trans_rebound, heat_rebound, prediction)
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from matplotlib.lines import Line2D
from pathlib import Path
import sys
import os

# Set root directory
root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.append(root_dir)
os.chdir(root_dir)


# ── Paths ──────────────────────────────────────────────────────────────────────
DATA_PATH   = Path("data/predictions")
FIGURES_DIR = Path("data/figures")
FIGURES_DIR.mkdir(parents=True, exist_ok=True)

PREDS_FILE  = DATA_PATH / "bases_preds_lhc.csv"

# ── Colour palette (matching R script intent) ──────────────────────────────────
COLOURS = {
    "energy_demand_no": "#d73027",   # no rebound  → warm red
    "energy_demand_rb": "#4575b4",   # rebound     → blue
    "no rebound":       "#d73027",
    "rebound":          "#4575b4",
}

# ── Shared theme helper ────────────────────────────────────────────────────────
def apply_theme(ax, xlabel="", ylabel="", fontsize=9):
    """Apply the clean, minimal style used throughout the reference script."""
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(labelsize=fontsize - 1)
    ax.yaxis.grid(True, linestyle="--", linewidth=0.4, alpha=0.4)
    ax.set_axisbelow(True)
    if xlabel:
        ax.set_xlabel(xlabel, fontsize=fontsize, fontweight="bold")
    if ylabel:
        ax.set_ylabel(ylabel, fontsize=fontsize, fontweight="bold")


# ── Policy-factor labelling ────────────────────────────────────────────────────
def add_policy_factors(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df["Value"] = df["Value"] / 1000  # TJ → PJ (same as /1000 in R)

    # Binary turnover
    conditions = [
        (df["trans_turnover"] == 0) & (df["heat_turnover"] == 0),
        (df["trans_turnover"] == 1) & (df["heat_turnover"] == 1),
    ]
    df["turn_level_bi"] = np.select(conditions,
                                     ["low turnover", "high turnover"],
                                     default=None)

    # Continuous-threshold turnover
    df["turn_level"] = np.where(
        (df["trans_turnover"] < 0.5) & (df["heat_turnover"] < 0.5),
        "low turnover",
        np.where(
            (df["trans_turnover"] >= 0.5) & (df["heat_turnover"] >= 0.5),
            "high turnover", None
        )
    )

    # Rebound level
    df["reb_level"] = np.where(
        (df["trans_rebound"] < 0.5) & (df["heat_rebound"] < 0.5),
        "low rebound",
        np.where(
            (df["trans_rebound"] >= 0.5) & (df["heat_rebound"] >= 0.5),
            "high rebound", None
        )
    )

    # # Rebound active flag
    # df["reb_active"] = np.where(df["reb_switch"] == 0, "no rebound", "rebound")

    # Tech leads
    df["leads"] = np.where(
        (df["lead_solar"] < 0.5) & (df["lead_onshore"] < 0.5),
        "Short tech lead",
        np.where(
            (df["lead_solar"] >= 0.5) & (df["lead_onshore"] >= 0.5),
            "Long tech lead", None
        )
    )

    # Cannibalisation rate
    df["cr_total"] = np.where(
        (df["cr_wind"] < 0.5) & (df["cr_solar"] < 0.5),
        "Low Cannibalisation",
        np.where(
            (df["cr_solar"] >= 0.5) & (df["cr_wind"] >= 0.5),
            "High Cannibalisation", None
        )
    )

    # Power transition composite
    df["power_transition"] = np.where(
        (df["cr_total"] == "High Cannibalisation") & (df["leads"] == "Long tech lead"),
        "slow transition",
        np.where(
            (df["cr_total"] == "Low Cannibalisation") & (df["leads"] == "Short tech lead"),
            "fast transition", None
        )
    )

    return df


# ══════════════════════════════════════════════════════════════════════════════
# PLOT 1 — Energy Demand: Low vs High Turnover, coloured by emulator
# ══════════════════════════════════════════════════════════════════════════════
def plot_energy_demand(df: pd.DataFrame, sample_prop: float = 0.1):
    """
    Spaghetti lines of energy demand faceted by turn_level_bi,
    coloured by emulator (no-rebound vs rebound).
    """
    # Sample a fraction of unique IDs to keep the plot readable
    rng  = np.random.default_rng(42)
    ids  = df["sample_id"].unique()
    keep = rng.choice(ids, size=max(1, int(len(ids) * sample_prop)), replace=False)
    sub  = df[df["sample_id"].isin(keep) & (df["Year"] >= 2020)].copy()

    facets = ["low turnover", "high turnover"]
    fig, axes = plt.subplots(1, 2, figsize=(10, 5), sharey=True)
    fig.subplots_adjust(wspace=0.15)

    emulator_labels = {
        "energy_demand_no": "No rebound",
        "energy_demand_rb": "Rebound",
    }

    for ax, facet in zip(axes, facets):
        fdata = sub[sub["turn_level_bi"] == facet]
        for (sid, emu), grp in fdata.groupby(["sample_id", "emulator"]):
            colour = COLOURS.get(emu, "#999999")
            ax.plot(grp["Year"], grp["Value"],
                    color=colour, alpha=0.05, linewidth=0.3)

        ax.set_title(facet, fontsize=10, fontweight="bold", pad=6)
        apply_theme(ax,
                    xlabel="Year",
                    ylabel="Energy Demand (PJ)" if ax is axes[0] else "",
                    fontsize=9)
        ax.tick_params(axis="x", rotation=45)

    # Legend
    legend_lines = [
        Line2D([0], [0], color=COLOURS["energy_demand_no"],
               lw=2, label=emulator_labels["energy_demand_no"]),
        Line2D([0], [0], color=COLOURS["energy_demand_rb"],
               lw=2, label=emulator_labels["energy_demand_rb"]),
    ]
    fig.legend(handles=legend_lines,
               title="Emulator", title_fontsize=9,
               fontsize=8, loc="lower center",
               ncol=2, frameon=False,
               bbox_to_anchor=(0.5, -0.04))

    fig.suptitle("Energy Demand by Turnover Scenario", fontsize=12,
                 fontweight="bold", y=1.02)
    plt.tight_layout()
    out = FIGURES_DIR / "energy_demand_turnover.png"
    fig.savefig(out, dpi=150, bbox_inches="tight")
    print(f"Saved → {out}")
    plt.show()



# ── Main ───────────────────────────────────────────────────────────────────────
def main():
    plt.style.use("default")

    print("Loading data …")
    df = pd.read_csv(PREDS_FILE)
    df = add_policy_factors(df)

    # ------------------------------------------------------------------
    # Plot 1: Energy demand spaghetti — needs Value column (time-series)
    # ------------------------------------------------------------------
    plot_energy_demand(df, sample_prop=0.1)

   

if __name__ == "__main__":
    main()
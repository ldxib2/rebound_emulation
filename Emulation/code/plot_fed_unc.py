import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from src.data_loader import load_csv
from fed_utils import plot_iqr_uncertainty, plot_spaghetti, plot_full_envelope

def main():

    plt.style.use("default")

    plt.rcParams.update({
        "font.size": 8,
        "axes.labelsize": 8,
        "axes.titlesize": 9,
        "xtick.labelsize": 7,
        "ytick.labelsize": 7,
        "legend.fontsize": 7,
        "axes.edgecolor": "black",
        "axes.linewidth": 0.8
    })

    df = load_csv("FR0_PJ.csv")

    df.set_index("YEAR", inplace=True)

    df = df / 1000  # PJ → EJ

    years = df.index.astype(int)

    rb = df.loc[:, df.columns.str.startswith("RB_")]
    norb = df.loc[:, df.columns.str.startswith("No_RB_")]

    colours = {
        "NORB": "#2F6C8E",
        "RB": "#43BF71",
    }

    fig, ax = plt.subplots(figsize=(7.1, 4.5))

    plot_full_envelope(
        ax=ax,
        x=years,
        df_subset=norb,
        color=colours["NORB"],
        linestyle="-",
        label="No rebound"
    )

    plot_full_envelope(
        ax=ax,
        x=years,
        df_subset=rb,
        color=colours["RB"],
        linestyle="--",
        label="With rebound"
    )
    ax.set_xlabel("Year")
    ax.set_ylabel("Final energy demand (EJ)")

    ax.set_xlim(years.min(), years.max())

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.grid(True, linestyle="--", linewidth=0.4, alpha=0.25)
    ax.set_axisbelow(True)

    custom_lines = [
        Line2D([0], [0], lw=1.6, color=colours["NORB"], linestyle="-",
            label="Median (no rebound)"),
        Line2D([0], [0], lw=1.6, color=colours["RB"], linestyle="--",
            label="Median (with rebound)"),
        Line2D([0], [0], lw=6, color="grey", alpha=0.25,
            label="Model range (min–max)"),
    ]

    ax.legend(handles=custom_lines, frameon=False, loc="upper left")

    plt.tight_layout()
    plt.savefig("plots/Figure 6.png", dpi=300, bbox_inches="tight")
    plt.show()
    plt.close()


if __name__ == "__main__":
    main()


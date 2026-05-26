import pandas as pd
import numpy as np

def plot_iqr_uncertainty(ax, x, df_subset, color, linestyle, label):

    q0 = df_subset.quantile(0.00, axis=1)
    q25 = df_subset.quantile(0.25, axis=1)
    q50 = df_subset.quantile(0.50, axis=1)
    q75 = df_subset.quantile(0.75, axis=1)
    q100 = df_subset.quantile(1.00, axis=1)

    ax.fill_between(x, q0, q25, color=color, alpha=0.12, linewidth=0)
    ax.fill_between(x, q75, q100, color=color, alpha=0.12, linewidth=0)

    ax.fill_between(x, q25, q75, color=color, alpha=0.35, linewidth=0)

    ax.plot(
        x,
        q50,
        color=color,
        linewidth=1.6,
        linestyle=linestyle,
        label=label
    )

def plot_spaghetti(ax, x, df_subset, color, linestyle, label):

    for col in df_subset.columns:
        ax.plot(
            x,
            df_subset[col],
            color=color,
            alpha=0.08,
            linewidth=0.8
        )

    median = df_subset.median(axis=1)

    ax.plot(
        x,
        median,
        color=color,
        linewidth=1.8,
        linestyle=linestyle,
        label=label
    )

def plot_full_envelope(ax, x, df_subset, color, linestyle, label):

    lower = df_subset.min(axis=1)
    upper = df_subset.max(axis=1)

    ax.fill_between(
        x,
        lower,
        upper,
        color=color,
        alpha=0.25,
        linewidth=0
    )

    median = df_subset.median(axis=1)

    ax.plot(
        x,
        median,
        color=color,
        linewidth=1.6,
        linestyle=linestyle,
        label=label
    )
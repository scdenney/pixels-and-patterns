#!/usr/bin/env python3
"""Render Table 1 (the two estimands side by side) as an on-brand PNG.

Reads summary_structural.csv (3 countries) and writes figures/fig_table.png.
Reproducible companion to 03_present.R; no data logic here, presentation only.
House palette (Obangsaek): terracotta Korea, cobalt Germany, ochre Taiwan text;
structural weight in cobalt; light-cobalt header on warm paper.
"""
import csv
import pathlib
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

HERE = pathlib.Path("/Users/scdenney/Documents/github/research/substack/drafts/coethnic-cross-national")
CSV = HERE / "analysis" / "summary_structural.csv"
OUT = HERE / "figures" / "fig_table.png"

# palette
PAPER = "#F3EFE3"; INK = "#17171A"; COBALT = "#1F4E9B"; TERRA = "#C25B39"; OCHRE = "#9A6B14"
HEADER_BG = "#E5ECF7"; BORDER = "#D9CDB0"; ROW_ALT = "#FBFAF4"
NAME_COLOR = {"South Korea": TERRA, "Germany": COBALT, "Taiwan": OCHRE}
ORDER = ["South Korea", "Germany", "Taiwan"]

rows = {r["Country"]: r for r in csv.DictReader(open(CSV))}

def f3(x):  # +0.082
    return f"+{float(x):.3f}"

def fmt_row(r):
    w = f'{float(r["weight"]):.2f}'
    wci = f'[{float(r["w_lo"]):.2f}, {float(r["w_hi"]):.2f}]'
    pr = f'{round(float(r["prefer"]))} / {round(float(r["reject"]))}'
    mod = f'{round(float(r["moderate"]))} [{round(float(r["mod_lo"]))}, {round(float(r["mod_hi"]))}]'
    strong = f'{round(float(r["strong"]))} [{round(float(r["strong_lo"]))}, {round(float(r["strong_hi"]))}]'
    n = f'{int(float(r["n"])):,}'
    return n, f3(r["AMCE"]), w, wci, pr, mod, strong

headers = ["Country", "n", "AMCE", "Structural weight", "Prefer /\nreject", "Moderate\n(>0.5)", "Strong\n(>1)"]
# column left edges + widths (sum ~1)
widths = [0.155, 0.075, 0.095, 0.225, 0.115, 0.165, 0.170]
lefts = [sum(widths[:i]) for i in range(len(widths))]
centers = [lefts[i] + widths[i] / 2 for i in range(len(widths))]

nrow = len(ORDER) + 1
rh = 1.0 / nrow

fig = plt.figure(figsize=(10.6, 2.35), dpi=150)
ax = fig.add_axes([0.012, 0.04, 0.976, 0.92]); ax.set_xlim(0, 1); ax.set_ylim(0, 1); ax.axis("off")
fig.patch.set_facecolor(PAPER)

def ytop(i):  # row i from top (0 = header)
    return 1.0 - i * rh

# header band
ax.add_patch(Rectangle((0, ytop(1)), 1, rh, facecolor=HEADER_BG, edgecolor="none", zorder=0))
for i, h in enumerate(headers):
    ha = "left" if i == 0 else "center"
    x = lefts[0] + 0.012 if i == 0 else centers[i]
    ax.text(x, ytop(1) + rh / 2, h, ha=ha, va="center", fontsize=11.5, fontweight="bold",
            color=INK, family="DejaVu Sans", linespacing=0.95)

# body rows
for ri, country in enumerate(ORDER, start=1):
    y0 = ytop(ri + 1)
    if ri % 2 == 0:
        ax.add_patch(Rectangle((0, y0), 1, rh, facecolor=ROW_ALT, edgecolor="none", zorder=0))
    n, amce, w, wci, pr, mod, strong = fmt_row(rows[country])
    yc = y0 + rh / 2
    ax.text(lefts[0] + 0.012, yc, country, ha="left", va="center", fontsize=11.5,
            fontweight="bold", color=NAME_COLOR[country], family="DejaVu Sans")
    ax.text(centers[1], yc, n, ha="center", va="center", fontsize=11, color=INK)
    ax.text(centers[2], yc, amce, ha="center", va="center", fontsize=11, color=INK)
    # structural weight: bold cobalt point + normal ink CI
    ax.text(lefts[3] + 0.018, yc, w, ha="left", va="center", fontsize=11.5, fontweight="bold", color=COBALT)
    ax.text(lefts[3] + 0.075, yc, wci, ha="left", va="center", fontsize=11, color=INK)
    ax.text(centers[4], yc, pr, ha="center", va="center", fontsize=11, color=INK)
    ax.text(centers[5], yc, mod, ha="center", va="center", fontsize=11, color=INK)
    ax.text(centers[6], yc, strong, ha="center", va="center", fontsize=11, color=INK)

# horizontal rules
for i in range(nrow + 1):
    lw = 1.4 if i in (0, 1, nrow) else 0.7
    ax.plot([0, 1], [ytop(i)] * 2, color=BORDER, lw=lw, zorder=1)

fig.savefig(OUT, facecolor=PAPER, bbox_inches="tight", pad_inches=0.06)
print("wrote", OUT)

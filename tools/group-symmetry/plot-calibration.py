"""Render selected held-out comparisons. Intervals describe simulation precision."""
import json
import sys
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

rows = json.loads(Path(sys.argv[1]).read_text())
methods = ["PM-mKH", "EqualSubjects", "FixedInverseVariance"]
labels = ["PM / modified KH", "Sign flip · equal subjects", "Sign flip · fixed precision"]
colors = ["#5261A3", "#127F78", "#B16A2D"]
plt.rcParams.update({"font.family": "DejaVu Sans", "font.size": 11,
                     "axes.spines.top": False, "axes.spines.right": False,
                     "axes.spines.left": False, "axes.edgecolor": "#CED3D6",
                     "text.color": "#213239", "axes.labelcolor": "#213239",
                     "xtick.color": "#53636A", "ytick.color": "#213239"})
fig, axes = plt.subplots(2, 2, figsize=(11, 6.7), gridspec_kw={"wspace": .52, "hspace": .6})
fig.patch.set_facecolor("#FAFBFA")
for row, df in enumerate(["0", "8"]):
    for col, question in enumerate(["null", "power"]):
        ax = axes[row, col]
        ax.set_facecolor("#FAFBFA")
        for j, method in enumerate(methods):
            r = next(x for x in rows if x["method"] == method and x["n"] == "8"
                     and x["df"] == df and x["tau"] == "0.2" and x["errors"] == "normal"
                     and x["precision"] == "spread" and x["question"] == question)
            y = 2-j
            ax.plot([100*r["lo95"], 100*r["hi95"]], [y,y], color=colors[j], lw=2.1)
            ax.scatter(100*r["rate"], y, color=colors[j], s=60, zorder=3,
                       marker=["o", "s", "D"][j], edgecolor="#FAFBFA", linewidth=.9)
            ax.text(100*r["hi95"] + (.3 if col==0 else .55), y, f'{100*r["rate"]:.2f}%', va="center", fontsize=10)
        ax.set_yticks([2,1,0], labels if col==0 else ["", "", ""])
        ax.tick_params(axis="y", length=0, pad=12)
        ax.set_ylim(-.5,2.7)
        ax.set_axisbelow(True)
        ax.grid(axis="x", color="#E3E7E6", linewidth=.7)
        ax.set_xlim(0,11 if col==0 else 30)
        ax.set_xticks([0,5,10] if col==0 else [0,10,20,30])
        ax.set_xlabel("False positives (%)" if col==0 else "Power for mean = 0.35 (%)", labelpad=9)
        if col==0:
            ax.axvline(5,color="#68797E",lw=1.2,ls=(0,(3,3)))
            ax.text(5,2.55,"nominal 5%",ha="center",fontsize=9,color="#53636A")
            ax.set_title("Known first-level variances" if row==0 else "Estimated variances · 8 residual df",loc="left",fontweight="bold",pad=18,fontsize=12)
        else:
            ax.set_title("Power on matched data",loc="left",fontweight="bold",pad=18,fontsize=12)
fig.suptitle("One-sample sign flips improve calibration",x=.025,ha="left",y=.985,fontsize=19,fontweight="bold")
fig.text(.025,.918,"8 independent subjects  ·  unequal precision (25:1)  ·  heterogeneity τ² = 0.2",fontsize=12)
fig.text(.025,.033,"Selected held-out Gaussian settings: 10,000 studies per row. Bars: 95% simulation intervals.\nSign flips enumerate all 256 actions; inference is pointwise and assumes symmetric subject errors.",fontsize=10,color="#53636A",linespacing=1.6)
fig.subplots_adjust(left=.25,right=.985,top=.82,bottom=.17)
out=Path(__file__).with_name("calibration.png")
fig.savefig(out,dpi=180,facecolor=fig.get_facecolor())
print(out)

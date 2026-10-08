"""Graphical abstract and research flow diagram (vector PDF + PNG), with every number read from outputs/.
Writes docs/Graphical_Abstract_EN.pdf/.png and docs/Research_Flow_Diagram_EN.pdf/.png."""
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

ROOT = Path(__file__).resolve().parents[1]
OUT, DOCS = ROOT / "outputs", ROOT / "docs"
plt.rcParams.update({"font.family": "DejaVu Sans", "pdf.fonttype": 42})
PW = ["SL", "S1", "S2", "S3", "S4", "S5"]
NAME = {"SL": "Sanitary landfill + flare", "S1": "Waste-to-energy", "S2": "RDF to cement kiln", "S3": "Anaerobic digestion",
        "S4": "PHB from landfill gas", "S5": "RDF + AD"}
COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}
INK, MUTED, LINE = "#1f2a36", "#55606e", "#c9d2dc"
BLUE, BLUE_L, GREEN_L, SAND_L, ROSE_L, GREY_L = "#2f5d8a", "#e8f0f8", "#e9f5ea", "#f6efe3", "#fbe9e7", "#f2f4f6"

# ------------------------------------------------------------------ numbers from the model outputs
pb = pd.read_csv(OUT / "15_p_best_fixed_carbon_value.csv"); pb = pb[pb.scenario == "market"]
PMED = pb.groupby(["carbon_value", "pathway"]).p_best.median().unstack()[PW]
PMED = PMED.div(PMED.sum(axis=1), axis=0)                         # medians renormalised for a stacked display
det = pd.read_csv(OUT / "03_deterministic_results.csv")
MAC_S5 = det[(det.scenario == "market") & (det.baseline == "SL") & (det.pathway == "S5")].MAC.median()
best = pd.read_csv(OUT / "04_best_pathway_by_carbon_value.csv"); best = best[best.scenario == "market"]
N_S5_100 = int((best.best_at_100 == "S5").sum()); N_SL_50 = int((best.best_at_50 == "SL").sum())
EVPI = pd.read_csv(OUT / "voi_evpi_by_location.csv").groupby("carbon_value").evpi.median()
PJ = pd.read_csv(OUT / "proj2045_comparison_central.csv"); PJ = PJ[PJ.baseline == "OD"].groupby("pathway").G_change.median()
NVAL = int((pd.read_csv(OUT / "validation_report.csv").result == "PASS").sum())
NVER = len(pd.read_csv(ROOT / "data" / "secondary_data_verification.csv"))
ACC = __import__("json").load(open(OUT / "discovery_cart_accuracy.json"))["acc_test"]


def box(ax, x, y, w, h, text="", fc=GREY_L, ec=LINE, fs=8, weight="normal", color=INK, ha="center", lw=0.8, r=0.012,
        va="center", pad=0.0):
    ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle=f"round,pad={pad},rounding_size={r}", fc=fc, ec=ec, lw=lw,
                                transform=ax.transAxes, zorder=2))
    if text:
        tx = x + w / 2 if ha == "center" else x + 0.008
        ax.text(tx, y + h / 2, text, ha=ha, va=va, fontsize=fs, weight=weight, color=color, transform=ax.transAxes,
                zorder=3, linespacing=1.25, wrap=False)


def arrow(ax, x1, y1, x2, y2, color=MUTED, lw=1.4, style="-|>", ms=12):
    ax.add_patch(FancyArrowPatch((x1, y1), (x2, y2), arrowstyle=style, mutation_scale=ms, color=color, lw=lw,
                                 transform=ax.transAxes, zorder=4, shrinkA=0, shrinkB=0))


# ================================================================== graphical abstract
def graphical_abstract():
    fig = plt.figure(figsize=(14, 7.4)); ax = fig.add_axes([0, 0, 1, 1]); ax.axis("off")
    ax.add_patch(plt.Rectangle((0, 0), 1, 1, fc="white", transform=ax.transAxes, zorder=0))
    # title band
    box(ax, 0.012, 0.885, 0.976, 0.1, fc=BLUE, ec=BLUE, r=0.01)
    ax.text(0.5, 0.95, "When does resource recovery beat a sanitary landfill?", ha="center", va="center", fontsize=17,
            weight="bold", color="white", transform=ax.transAxes, zorder=3)
    ax.text(0.5, 0.908, "Ex-ante screening of six MSW pathways for 21 Indonesian cities and regencies  |  per tonne of "
            "mixed MSW at the facility gate, 2025 baseline", ha="center", va="center", fontsize=10.2, color="#dfe9f3",
            transform=ax.transAxes, zorder=3)

    # column headers
    for x, w, t in ((0.012, 0.25, "1  DATA (2025 baseline)"), (0.285, 0.33, "2  MODEL"), (0.638, 0.35, "3  RESULTS")):
        ax.text(x + 0.004, 0.855, t, ha="left", va="center", fontsize=11, weight="bold", color=BLUE, transform=ax.transAxes)
        ax.plot([x, x + w], [0.838, 0.838], color=BLUE, lw=1.2, transform=ax.transAxes)

    # column 1: data
    items = [("21 cities / regencies", "RIPS waste master plans:\ncomposition + tonnage", SAND_L),
             ("2025 baseline", "Tonnage projected once,\nQ2025 = Qt (1+g)^(2025-t)", SAND_L),
             ("Provenance", f"Every value labelled; {NVER} secondary\nvalues re-verified against sources", SAND_L),
             ("Parameters", "IPCC 2006/2019 defaults, Indonesian\ncosts, grid factors (ESDM 2018)", SAND_L)]
    y = 0.705
    for head, body, fc in items:
        box(ax, 0.012, y, 0.25, 0.115, fc=fc, ec="#d8c7a6")
        ax.text(0.022, y + 0.083, head, fontsize=9.5, weight="bold", color=INK, transform=ax.transAxes, va="center", zorder=3)
        ax.text(0.022, y + 0.038, body, fontsize=8.3, color=MUTED, transform=ax.transAxes, va="center", zorder=3, linespacing=1.2)
        y -= 0.13
    arrow(ax, 0.266, 0.52, 0.283, 0.52, color=BLUE, lw=2.2, ms=18)

    # column 2: model
    box(ax, 0.285, 0.565, 0.33, 0.26, fc=BLUE_L, ec="#b9cde2")
    ax.text(0.45, 0.8, "Six options manage the whole tonne (one LCA/TEA boundary)", ha="center", fontsize=8.8,
            weight="bold", color=INK, transform=ax.transAxes, zorder=3)
    for n, k in enumerate(PW):
        cx = 0.297 + (n % 3) * 0.106; cy = 0.71 - (n // 3) * 0.075
        box(ax, cx, cy, 0.1, 0.06, f"{k}\n{NAME[k]}", fc=COL[k], ec=COL[k], fs=7.6, color="white", weight="bold", r=0.01)
    ax.text(0.45, 0.59, "Climate (GWP100)  ·  net cost  ·  fossil energy  ·  land", ha="center", fontsize=8.6,
            color=MUTED, transform=ax.transAxes, zorder=3)
    steps = [("Feasibility gates", "LHV, scale, RDF quality, kiln distance, PHB scale"),
             ("Decision", "carbon-inclusive cost C + p·G/1000,\nfixed p = 0, 2, 25, 50, 100 USD/t CO2e"),
             ("Uncertainty", "Monte Carlo 4,000 draws × 21 locations,\nSobol indices, stress tests"),
             ("Decision support", "scenario discovery (CART/PRIM), break-even\ntargets, value of information"),
             ("Future", "2045 projection: grid to net zero in 2060,\nreal cost escalation")]
    y = 0.49
    for head, body in steps:
        box(ax, 0.285, y, 0.33, 0.062, fc="white", ec="#b9cde2")
        ax.text(0.294, y + 0.031, head, fontsize=8.6, weight="bold", color=BLUE, transform=ax.transAxes, va="center", zorder=3)
        ax.text(0.39, y + 0.031, body, fontsize=7.6, linespacing=1.15, color=INK, transform=ax.transAxes, va="center", zorder=3)
        y -= 0.07
    arrow(ax, 0.619, 0.52, 0.636, 0.52, color=BLUE, lw=2.2, ms=18)

    # column 3: chart of P(best) by carbon value
    cax = fig.add_axes([0.675, 0.565, 0.305, 0.235])
    x = np.arange(len(PMED.index)); bottom = np.zeros(len(x))
    for k in PW:
        cax.bar(x, PMED[k].values, bottom=bottom, color=COL[k], width=0.72, edgecolor="white", linewidth=0.8)
        bottom += PMED[k].values
    cax.set_xticks(x, [str(int(v)) for v in PMED.index], fontsize=8.5)
    cax.set_yticks([0, 0.5, 1], ["0", "0.5", "1"], fontsize=8)
    cax.set_xlabel("Carbon value, USD per t CO2e  (2 = current Indonesian carbon tax)", fontsize=8, color=INK)
    cax.set_ylabel("P(best), median", fontsize=8.5, color=INK)
    for s in ("top", "right"): cax.spines[s].set_visible(False)
    cax.spines["left"].set_color(LINE); cax.spines["bottom"].set_color(LINE)
    cax.set_title("Which option is preferred? (21 locations)", fontsize=9.2, weight="bold", color=INK, loc="left")
    # key results
    res = [(f"Up to 50 USD/t CO2e", f"Landfill + flare is cheapest in all {N_SL_50} locations", "#5f5f5f"),
           (f"At 100 USD/t CO2e", f"RDF + AD wins in {N_S5_100} of 21 (abatement ~{MAC_S5:.0f} USD/t CO2e)", COL["S5"]),
           ("Measure first", f"waste moisture/composition & landfill-gas capture (EVPI {EVPI[100]:.1f} USD/t)", BLUE),
           ("In 2045", f"cleaner grid: WtE GHG +{PJ['S1']:.0f} kg CO2e/t; RDF + AD unchanged", "#6b4f9e")]
    y = 0.425
    for head, body, c in res:
        box(ax, 0.638, y, 0.35, 0.062, fc="white", ec=LINE)
        ax.add_patch(plt.Rectangle((0.638, y), 0.006, 0.062, fc=c, ec=c, transform=ax.transAxes, zorder=3))
        ax.text(0.652, y + 0.043, head, fontsize=8.6, weight="bold", color=c, transform=ax.transAxes, va="center", zorder=3)
        ax.text(0.652, y + 0.017, body, fontsize=7.9, color=INK, transform=ax.transAxes, va="center", zorder=3)
        y -= 0.07

    # bottom band: decision rules = answer to the research question
    box(ax, 0.012, 0.03, 0.976, 0.12, fc=GREEN_L, ec="#b6d8b9")
    ax.text(0.022, 0.125, "Answer to the research question: conditions under which each pathway becomes preferable",
            fontsize=9.5, weight="bold", color="#2e6b34", transform=ax.transAxes, va="center", zorder=3)
    rules = [("SL", "Landfill + flare", "carbon value ≤ ~50 USD/t,\nor no cement kiln in reach"),
             ("S5", "RDF + AD", f"carbon value near {MAC_S5:.0f}-100 USD/t,\nkiln ≤ 300 km, waste not too wet"),
             ("S1", "Waste-to-energy", "Perpres 109/2025 tariff +\n≥ 1,000 t/day + LHV ≥ 7 MJ/kg"),
             ("S3", "Anaerobic digestion", "food separated at source +\ncarbon value > ~30-45 USD/t")]
    for n, (k, head, body) in enumerate(rules):
        x0 = 0.022 + n * 0.241
        ax.add_patch(plt.Circle((x0 + 0.009, 0.072), 0.008, color=COL[k], transform=ax.transAxes, zorder=3))
        ax.text(x0 + 0.024, 0.088, head, fontsize=8.8, weight="bold", color=INK, transform=ax.transAxes, va="center", zorder=3)
        ax.text(x0 + 0.024, 0.056, body, fontsize=7.6, color=MUTED, transform=ax.transAxes, va="center", zorder=3, linespacing=1.15)
    ax.text(0.988, 0.008, f"Open Colab decision tool (mswpath v3.2) · {NVAL} verification checks · tree accuracy "
            f"{100 * ACC:.0f}%", ha="right", va="bottom", fontsize=7, color=MUTED, transform=ax.transAxes)
    for ext in ("pdf", "png"):
        fig.savefig(DOCS / f"Graphical_Abstract_EN.{ext}", dpi=300)
    plt.close(fig)


# ================================================================== research flow diagram
def flow_diagram():
    fig = plt.figure(figsize=(8.27, 11.69)); ax = fig.add_axes([0, 0, 1, 1]); ax.axis("off")
    ax.text(0.5, 0.972, "Research flow", ha="center", fontsize=16, weight="bold", color=INK, transform=ax.transAxes)
    ax.text(0.5, 0.952, "Ex-ante screening of MSW recovery pathways for 21 Indonesian cities and regencies",
            ha="center", fontsize=9.5, color=MUTED, transform=ax.transAxes)
    phases = [  # (phase label, colour, [(main box title, detail)], side note)
        ("Phase 1\nFraming", "#5b6f86", [("Problem and research question",
          "Under what Indonesian city and waste-system conditions does\neach recovery pathway become preferable? (no facility exists yet)")],
         "Functional unit: 1 t mixed MSW at the\nfacility gate, 2025; six options + 2 baselines"),
        ("Phase 2\nData", "#a07a3c", [("Data collection",
          "RIPS of 21 locations (composition, tonnage, managed share);\nIPCC 2006/2019 defaults; Indonesian costs; grid factors"),
          ("Secondary-data verification and provenance",
           f"{NVER} values re-checked; status V/L/A; blank = unknown, never zero")],
         "build_database_v3.py -> data/*.csv\n(source + locator + status per value)"),
        ("Phase 3\nModel", "#2f5d8a", [("Harmonisation to 2025",
          "Tonnage projected once; composition normalised per stream and\nweighted by 2025 tonnage; rules L/W"),
          ("Characterisation", "Moisture, LHV, fossil carbon, landfill methane potential"),
          ("LCA + TEA of six options", "SL, WtE, RDF, AD, PHB, RDF + AD: GHG, net cost, fossil CED,\nland; residues to landfill; gates G1-G6")],
         "mswpath/core.py\n(45 equations, see Mathematical Model PDF)"),
        ("Phase 4\nDecision &\nuncertainty", "#3b8a43", [("Decision analysis",
          "Carbon-inclusive cost C + pG/1000 at p = 0, 2, 25, 50, 100;\nPareto screening; P(best) with standard error"),
          ("Uncertainty and sensitivity", "Monte Carlo (4,000 draws per location), scenarios, stress tests,\nSpearman and Sobol indices")],
         "Common random numbers:\nscenario differences are paired"),
        ("Phase 5\nDecision\nsupport", "#7a4fa0", [("Scenario discovery", "40,000 condition sets; CART tree and PRIM boxes -> decision rules"),
          ("Break-even targets and value of information", "What an option must reach to beat a landfill;\nEVPI/EVPPI: which data to measure first"),
          ("Projection to 2045", "Grid to net zero in 2060 (delta = 1/35), real escalation 2/1/2%,\ntonnage growth; change by driver")],
         "mswpath/discovery.py, voi.py,\nthresholds.py, projection.py"),
        ("Phase 6\nOutputs", "#b5523b", [("Answer and tools for stakeholders",
          "Conditions per pathway; Colab tool (one city, manual template);\nmethodology, manuscript, worked example, 2045 report")],
         "Local governments, Bappeda, KLHK,\nPLN / cement offtakers, funders"),
    ]
    top, bottom = 0.925, 0.06
    nbox = sum(len(p[2]) for p in phases)
    gap_phase, gap_box = 0.016, 0.009
    h = (top - bottom - gap_phase * (len(phases) - 1) - gap_box * (nbox - len(phases))) / nbox
    x_main, w_main = 0.2, 0.47
    x_side, w_side = 0.705, 0.27
    y = top; centers = []
    for label, colr, boxes, note in phases:
        y0 = y
        for title, detail in boxes:
            yb = y - h
            box(ax, x_main, yb, w_main, h, fc="white", ec=colr, lw=1.2)
            ax.text(x_main + 0.012, yb + h * 0.72, title, fontsize=8.8, weight="bold", color=colr, transform=ax.transAxes, va="center", zorder=3)
            ax.text(x_main + 0.012, yb + h * 0.33, detail, fontsize=7.1, color=INK, transform=ax.transAxes, va="center", zorder=3, linespacing=1.2)
            centers.append((yb + h, yb))
            y = yb - gap_box
        y = y + gap_box
        # phase band
        box(ax, 0.02, y, 0.14, y0 - y, label, fc=colr, ec=colr, fs=8.6, color="white", weight="bold", r=0.008)
        # side note
        box(ax, x_side, y + (y0 - y) / 2 - 0.028, w_side, 0.056, note, fc=GREY_L, ec=LINE, fs=7, color=MUTED, r=0.008)
        ax.plot([x_main + w_main, x_side], [y + (y0 - y) / 2] * 2, color=LINE, lw=0.8, ls=":", transform=ax.transAxes)
        y = y - gap_phase
    for (t1, b1), (t2, b2) in zip(centers[:-1], centers[1:]):
        arrow(ax, x_main + w_main / 2, b1, x_main + w_main / 2, t2, color=MUTED, lw=1.1, ms=9)
    # validation bar spanning phases 3-5
    t_val = centers[3][0]; b_val = centers[-2][1]
    box(ax, 0.68, b_val, 0.012, t_val - b_val, fc="#d9a441", ec="#d9a441", r=0.004)
    ax.text(0.686, (t_val + b_val) / 2, f"Verification & validation: {NVAL} automated checks, benchmark comparison, stress "
            "tests, field-validation protocol", rotation=90, ha="center", va="center", fontsize=7.2, color="white",
            weight="bold", transform=ax.transAxes, zorder=3)
    # feedback: value of information tells which data to collect first (VOI box -> data collection box)
    yv = centers[-3][1] + h / 2; yd = centers[1][1] + h / 2; xr = 0.186
    ax.plot([x_main, xr, xr], [yv, yv, yd], color="#7a4fa0", lw=0.9, ls="--", transform=ax.transAxes, zorder=1)
    arrow(ax, xr, yd, x_main, yd, color="#7a4fa0", lw=0.9, ms=9)
    ax.text(xr - 0.006, (yv + yd) / 2, "VOI: data to collect first", rotation=90, ha="center", va="center", fontsize=6.4,
            color="#7a4fa0", transform=ax.transAxes, bbox=dict(fc="white", ec="none", pad=0.6), zorder=5)
    ax.text(0.5, 0.025, "mswpath v3.2  ·  all numbers and files reproducible from the repository (MSW_pathway_model_v3)",
            ha="center", fontsize=7, color=MUTED, transform=ax.transAxes)
    for ext in ("pdf", "png"):
        fig.savefig(DOCS / f"Research_Flow_Diagram_EN.{ext}", dpi=300)
    plt.close(fig)


graphical_abstract()
flow_diagram()
print("wrote Graphical_Abstract_EN.pdf/.png and Research_Flow_Diagram_EN.pdf/.png")

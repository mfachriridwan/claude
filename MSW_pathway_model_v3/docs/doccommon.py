"""Shared helpers for the v3 PDF documents: styles, tables, equations, figures and result statistics."""
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
from reportlab.lib import colors
from reportlab.lib.enums import TA_JUSTIFY, TA_CENTER
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.lib.units import cm
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.platypus import Paragraph, Table, TableStyle, Image, Spacer, KeepTogether

ROOT = Path(__file__).resolve().parents[1]
import sys as _sys
if str(ROOT) not in _sys.path: _sys.path.insert(0, str(ROOT))
DATA, OUT, DOCS = ROOT / "data", ROOT / "outputs", ROOT / "docs"
FIG = DOCS / "figures"
FIG.mkdir(exist_ok=True)

F = "/usr/share/fonts/truetype/liberation/"
pdfmetrics.registerFont(TTFont("Serif", F + "LiberationSerif-Regular.ttf"))
pdfmetrics.registerFont(TTFont("Serif-B", F + "LiberationSerif-Bold.ttf"))
pdfmetrics.registerFont(TTFont("Serif-I", F + "LiberationSerif-Italic.ttf"))
pdfmetrics.registerFont(TTFont("Serif-BI", F + "LiberationSerif-BoldItalic.ttf"))
pdfmetrics.registerFont(TTFont("Sans", F + "LiberationSans-Regular.ttf"))
pdfmetrics.registerFont(TTFont("Sans-B", F + "LiberationSans-Bold.ttf"))
from reportlab.pdfbase.pdfmetrics import registerFontFamily
registerFontFamily("Serif", normal="Serif", bold="Serif-B", italic="Serif-I", boldItalic="Serif-BI")
registerFontFamily("Sans", normal="Sans", bold="Sans-B", italic="Sans", boldItalic="Sans-B")

ss = getSampleStyleSheet()
S = {
    "title": ParagraphStyle("t", fontName="Serif-B", fontSize=17, leading=21, alignment=TA_CENTER, spaceAfter=6),
    "subtitle": ParagraphStyle("st", fontName="Serif", fontSize=11, leading=14, alignment=TA_CENTER, spaceAfter=4),
    "h1": ParagraphStyle("h1", fontName="Serif-B", fontSize=13, leading=16, spaceBefore=10, spaceAfter=5),
    "h2": ParagraphStyle("h2", fontName="Serif-B", fontSize=11, leading=14, spaceBefore=7, spaceAfter=3),
    "body": ParagraphStyle("b", fontName="Serif", fontSize=10, leading=13.2, alignment=TA_JUSTIFY, spaceAfter=5),
    "small": ParagraphStyle("s", fontName="Serif", fontSize=8.6, leading=10.6, alignment=TA_JUSTIFY, spaceAfter=3),
    "cap": ParagraphStyle("c", fontName="Serif-I", fontSize=8.8, leading=11, alignment=TA_JUSTIFY, spaceBefore=2, spaceAfter=7),
    "cell": ParagraphStyle("cell", fontName="Sans", fontSize=7.2, leading=8.6),
    "cellb": ParagraphStyle("cellb", fontName="Sans-B", fontSize=7.2, leading=8.6),
    "box": ParagraphStyle("box", fontName="Serif", fontSize=9.4, leading=12, alignment=TA_JUSTIFY),
    "ref": ParagraphStyle("r", fontName="Serif", fontSize=8.4, leading=10.2, leftIndent=12, firstLineIndent=-12, spaceAfter=2),
}


def P(t, st="body"):
    return Paragraph(t, S[st])


def bullets(items, st="body"):
    return [Paragraph("&bull;&nbsp;" + t, ParagraphStyle("bl", parent=S[st], leftIndent=10, firstLineIndent=-8))
            for t in items]


def table(rows, widths, header=1, zebra=True, font=None):
    data = [[c if not isinstance(c, str) else Paragraph(c, S["cellb"] if i < header else S["cell"]) for c in r]
            for i, r in enumerate(rows)]
    t = Table(data, colWidths=[w * cm for w in widths], repeatRows=header)
    st = [("VALIGN", (0, 0), (-1, -1), "TOP"), ("LINEABOVE", (0, 0), (-1, 0), 0.8, colors.black),
          ("LINEBELOW", (0, header - 1), (-1, header - 1), 0.5, colors.black),
          ("LINEBELOW", (0, -1), (-1, -1), 0.8, colors.black),
          ("TOPPADDING", (0, 0), (-1, -1), 1.6), ("BOTTOMPADDING", (0, 0), (-1, -1), 1.6),
          ("LEFTPADDING", (0, 0), (-1, -1), 2.5), ("RIGHTPADDING", (0, 0), (-1, -1), 2.5)]
    if zebra:
        for i in range(header, len(rows)):
            if (i - header) % 2:
                st.append(("BACKGROUND", (0, i), (-1, i), colors.HexColor("#f2f2f2")))
    t.setStyle(TableStyle(st))
    return t


def boxed(flow, width=17.0, bg="#eef3f8"):
    t = Table([[flow]], colWidths=[width * cm])
    t.setStyle(TableStyle([("BACKGROUND", (0, 0), (-1, -1), colors.HexColor(bg)),
                           ("BOX", (0, 0), (-1, -1), 0.5, colors.HexColor("#7f9fbf")),
                           ("LEFTPADDING", (0, 0), (-1, -1), 6), ("RIGHTPADDING", (0, 0), (-1, -1), 6),
                           ("TOPPADDING", (0, 0), (-1, -1), 5), ("BOTTOMPADDING", (0, 0), (-1, -1), 5)]))
    return t


def img(path, width_cm):
    from PIL import Image as PI
    w, h = PI.open(path).size
    return Image(str(path), width=width_cm * cm, height=width_cm * cm * h / w)


_eq_n = [0]


def eq(tex, name, width_cm=None, fs=13):
    """Render a mathtext equation to PNG and return an Image flowable."""
    path = FIG / f"eq_{name}.png"
    fig = plt.figure(figsize=(0.01, 0.01))
    fig.text(0, 0, f"${tex}$", fontsize=fs)
    fig.savefig(path, dpi=300, bbox_inches="tight", pad_inches=0.04, transparent=False, facecolor="white")
    plt.close(fig)
    from PIL import Image as PI
    w, h = PI.open(path).size
    wc = min(w / 300 * 2.54 * 0.92, 15.0) if width_cm is None else width_cm
    return Image(str(path), width=wc * cm, height=wc * cm * h / w)


def eqrow(tex, name, label, fs=13):
    im = eq(tex, name, fs=fs)
    t = Table([[im, Paragraph(f"({label})", S["body"])]], colWidths=[15.4 * cm, 1.6 * cm])
    t.setStyle(TableStyle([("VALIGN", (0, 0), (-1, -1), "MIDDLE"), ("ALIGN", (0, 0), (0, 0), "CENTER")]))
    return t


def figure(path, width_cm, caption):
    """Figure and caption kept on the same page."""
    return KeepTogether([img(path, width_cm), Paragraph(caption, S["cap"])])


def num(v):
    return f"{v/1e6:g} M" if abs(v) >= 1e5 else f"{v:g}"


def page_deco(title):
    def deco(canvas, doc):
        canvas.saveState()
        canvas.setFont("Serif-I", 8)
        canvas.drawString(2 * cm, A4[1] - 1.2 * cm, title)
        canvas.drawRightString(A4[0] - 2 * cm, A4[1] - 1.2 * cm, str(doc.page))
        canvas.setLineWidth(0.3); canvas.line(2 * cm, A4[1] - 1.35 * cm, A4[0] - 2 * cm, A4[1] - 1.35 * cm)
        canvas.restoreState()
    return deco


# ---------------------------------------------------------------------------------------------
# Data and statistics used in both documents (numbers are read, never typed)
# ---------------------------------------------------------------------------------------------
def load():
    d = dict(
        inp=pd.read_csv(DATA / "input_kota_2025_updated.csv"),
        t2=pd.read_csv(DATA / "table2_model_fraction_parameters.csv"),
        t5=pd.read_csv(DATA / "table5_qmanaged_2025.csv"),
        reg=pd.read_csv(DATA / "assumption_register.csv"),
        cross=pd.read_csv(DATA / "taxonomy_crosswalk.csv", dtype={"code": str}),
        fp=pd.read_csv(DATA / "fraction_parameters_L1_L2_L3.csv", dtype={"code": str}),
        bm=pd.read_csv(DATA / "composition_benchmarks_conditional.csv", dtype={"code": str}),
        term=pd.read_csv(DATA / "composition_terminal_partition_2025.csv", dtype={"code": str}),
        char=pd.read_csv(OUT / "02_characterisation_and_gates.csv"),
        det=pd.read_csv(OUT / "03_deterministic_results.csv"),
        best=pd.read_csv(OUT / "04_best_pathway_by_carbon_value.csv"),
        mc=pd.read_csv(OUT / "05_montecarlo_results.csv"),
        sens=pd.read_csv(OUT / "06_sensitivity_spearman.csv", header=[0, 1], index_col=0),
        bench=pd.read_csv(OUT / "09_validation_benchmarks.csv", index_col=0),
        usplit=pd.read_csv(OUT / "10_uncertainty_split.csv", header=[0, 1], index_col=0),
        cover=pd.read_csv(OUT / "11_parameter_coverage_L2L3.csv"),
        rob=pd.read_csv(OUT / "12_most_probable_by_scenario.csv", index_col=0),
        val=pd.read_csv(OUT / "validation_report.csv"),
        lcia=pd.read_csv(DATA / "lcia_factors.csv"),
        rules=pd.read_csv(OUT / "discovery_cart_rules.csv"),
        imp=pd.read_csv(OUT / "discovery_cart_importance.csv", index_col=0).iloc[:, 0],
        prim=pd.read_csv(OUT / "discovery_prim_boxes.csv"),
        sobol=pd.read_csv(OUT / "discovery_sobol_mean.csv"),
        cart_acc=__import__("json").load(open(OUT / "discovery_cart_accuracy.json")),
        disc=pd.read_csv(OUT / "discovery_samples.csv.gz"),
        meta=__import__("json").load(open(OUT / "discovery_meta.json")),
    )
    return d


PW = ["SL", "S1", "S2", "S3", "S4", "S5"]
NAMES = {"SL": "SL landfill + flare", "S1": "S1 WtE", "S2": "S2 RDF", "S3": "S3 AD", "S4": "S4 PHB", "S5": "S5 RDF + AD"}
SCN = {"market": "Market", "perpres109": "Perpres 109", "food_separated_at_source": "Food sep.",
       "residues_to_open_dump": "Residues to OD", "managed_M1_status_index": "Managed M1",
       "managed_M2_local_first": "Managed M2", "GWP20": "GWP20", "moisture_IPCC_default": "IPCC moisture",
       "tonnage_overview_projected": "Overview projected", "docf_rubber_0.5": "Rubber DOCf 0.5"}


def fmt(v, f=".0f"):
    return "n.a." if pd.isna(v) else format(v, f)


def lcia_table(d, scen="market"):
    det = d["det"]; x = det[(det.scenario == scen) & (det.baseline == "OD")]
    rows = []
    for k in PW:
        q = x[x.pathway == k]
        rows.append([NAMES[k], f"{q.CED.median()/1e3:.2f} ({q.CED.min()/1e3:.2f} to {q.CED.max()/1e3:.2f})",
                     f"{q.dCED.median()/1e3:.2f}", f"{q.LU.median():.3f} ({q.LU.min():.3f} to {q.LU.max():.3f})",
                     f"{q.dLU.median():.3f}"])
    return rows


def rules_table(d, min_share=0.03):
    r = d["rules"].sort_values("share_of_draws", ascending=False)
    r = r[r.share_of_draws >= min_share]
    return [[x.rule.replace(" AND ", " and ").replace("_", " "), NAMES[x.predicted], f"{x.purity:.2f}", f"{100*x.share_of_draws:.0f}%"]
            for x in r.itertuples()]


def pathway_table(d, scen="market"):
    det = d["det"]; x = det[det.scenario == scen]
    rows = []
    for k in PW:
        o, s = x[(x.baseline == "OD") & (x.pathway == k)], x[(x.baseline == "SL") & (x.pathway == k)]
        r = lambda q, c, f=".0f": f"{q[c].median():{f}} ({q[c].min():{f}} to {q[c].max():{f}})"
        rows.append([NAMES[k], r(o, "G"), r(o, "dG"), "-" if k == "SL" else r(s, "dG"), r(o, "C"), r(o, "MAC"),
                     "-" if k == "SL" else r(s, "MAC"), str(int(o.feasible.sum()))])
    return rows


def stats(d):
    det, best, mc, rob = d["det"], d["best"], d["mc"], d["rob"]
    N = {}
    b = best[best.scenario == "market"]
    for pc in (0, 25, 50, 100):
        N[f"best{pc}"] = b[f"best_at_{pc}"].value_counts().to_dict()
    w = mc[mc.scenario == "market"].pivot(index="city", columns="pathway", values="p_best")
    srt = np.sort(w[PW].to_numpy(), axis=1)
    N["n_ties"] = int((srt[:, -1] - srt[:, -2] < 0.05).sum())
    N["mp"] = rob.market.value_counts().to_dict()
    N["mp_all"] = {c: rob[c].value_counts().to_dict() for c in rob.columns}
    m = mc[mc.scenario == "market"].groupby("pathway")[["p_feasible", "p_front", "p_best_pc0_10", "p_best_pc10_50",
                                                       "p_best_pc50_100"]].mean()
    N["avg"] = m
    ch = d["char"]
    N["lhv"] = (ch.LHV.min(), ch.LHV.max(), ch.LHV.median())
    N["g1"] = int(ch.gate_LHV.sum()); N["g12"] = int((ch.gate_LHV & ch.gate_WtE_scale).sum())
    N["psel_gen"] = int(ch.gate_PSEL_generated.sum())
    N["psel_m1"] = int(ch.gate_PSEL_M1.fillna(False).astype(bool).sum())
    N["psel_m2"] = int(ch.gate_PSEL_M2.fillna(False).astype(bool).sum())
    x = det[(det.scenario == "market")]
    s5 = x[(x.baseline == "SL") & (x.pathway == "S5")].MAC
    N["s5_mac"] = (s5.median(), s5.min(), s5.max())
    N["valid"] = (int((d["val"].result == "PASS").sum()), len(d["val"]))
    ds = d["disc"]; mix = (ds.food_separated == 0) & (ds.tariff == 0)
    N["disc_share"] = ds.best.value_counts(normalize=True).to_dict()
    N["disc_mix"] = ds[mix].best.value_counts(normalize=True).to_dict()
    sb = d["sobol"]
    N["sobol_gap"] = sb[sb.output == "SCgap_S5_SL"].sort_values("ST", ascending=False).head(6)
    N["sobol_gsl"] = sb[sb.output == "G_SL"].sort_values("ST", ascending=False).head(4)
    N["sobol_gs1"] = sb[sb.output == "G_S1"].sort_values("ST", ascending=False).head(4)
    x = det[(det.scenario == "market") & (det.baseline == "OD")]
    N["ced_med"] = x.groupby("pathway").CED.median().to_dict(); N["lu_med"] = x.groupby("pathway").LU.median().to_dict()
    return N


# ---------------------------------------------------------------------------------------------
# Figures produced for the documents
# ---------------------------------------------------------------------------------------------
def fig_flow():
    path = FIG / "fig_flow.png"
    fig, ax = plt.subplots(figsize=(11, 6.2)); ax.set_xlim(0, 11); ax.set_ylim(0, 6.2); ax.axis("off")

    def box(x, y, w, h, t, fc="#e8f0f8", ec="#46719e", fs=8.2, bold=False):
        ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.03,rounding_size=0.08", fc=fc, ec=ec, lw=1))
        ax.text(x + w / 2, y + h / 2, t, ha="center", va="center", fontsize=fs, weight="bold" if bold else "normal", wrap=True)

    def arr(x1, y1, x2, y2):
        ax.annotate("", (x2, y2), (x1, y1), arrowprops=dict(arrowstyle="-|>", color="#333", lw=0.9))

    ax.text(0.1, 6.0, "A. Data layer (build_database_v3.py)", fontsize=9.5, weight="bold")
    src = [("RIPS extracts\n(21 locations)", 5.0), ("Qmanaged\nsource audit", 4.1), ("IPCC 2006 / 2019\nTable 2.4 / 3.0", 3.2),
           ("Edjabou et al. 2015\ntaxonomy + Tables 3-4", 2.3), ("Legacy v2\nregister / LHV / tau", 1.4)]
    for t, y in src:
        box(0.1, y, 1.9, 0.7, t, fc="#f6efe3", ec="#a07a3c", fs=7.6)
        arr(2.0, y + 0.35, 2.55, 3.2)
    box(2.55, 2.55, 1.6, 1.3, "Rules +\nstatus labels\n(measured / proxy /\nassumption / gap)", fc="#fbe9e7", ec="#b5523b", fs=7.4)
    outs = [("input_kota_2025_updated", 5.15), ("table5_qmanaged_2025", 4.45), ("table2 + register\n+ constants", 3.65),
            ("fraction_parameters\nL1/L2/L3 + crosswalk", 2.8), ("composition L1/L2/\nterminal (78 leaves)", 1.95),
            ("benchmarks\n(conditional)", 1.15)]
    for t, y in outs:
        box(4.6, y, 1.85, 0.62, t, fs=7.2)
        arr(4.15, 3.2, 4.6, y + 0.31)
    ax.text(6.85, 6.0, "B. Model layer (msw_pathway_model_v3.py)", fontsize=9.5, weight="bold")
    steps = [("Parser: rejects v2 file;\nreads Q2025, no re-projection", 5.05), ("Harmonise: normalise streams,\nrules L / W, 2025 mass weights", 4.2),
             ("Characterise: M, LHV, E_fos,\nCH4 (DOC dry x DOCf)", 3.35), ("Six options S0-S5:\nLCA (kg CO2e/t) + TEA (USD/t)", 2.5),
             ("Gates G1-G6 -> Pareto ->\nP(best) over carbon value", 1.65), ("Monte Carlo + scenarios\n+ Spearman + split", 0.8)]
    for i, (t, y) in enumerate(steps):
        box(7.0, y, 2.6, 0.68, t, fc="#e9f5ea", ec="#3b8a43", fs=7.4)
        if i: arr(8.3, y + 0.68 + 0.17, 8.3, y + 0.68)
    for t, y in outs[:4]:
        arr(6.45, y + 0.31, 7.0, 5.0 - 0.0 * y)
    box(9.85, 2.3, 1.05, 1.9, "validate_v3\n49 checks\n\noutputs/\n01-12 CSV\n+ figures", fc="#eeeeee", ec="#666", fs=7.2)
    arr(9.6, 3.2, 9.85, 3.2)
    fig.savefig(path, dpi=220, bbox_inches="tight"); plt.close(fig)
    return path


def fig_metamodel():
    path = FIG / "fig_metamodel.png"
    fig, ax = plt.subplots(figsize=(11, 4.6)); ax.set_xlim(0, 11); ax.set_ylim(0, 4.6); ax.axis("off")

    def box(x, y, w, h, t, fc, ec, fs=7.6):
        ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.03,rounding_size=0.08", fc=fc, ec=ec, lw=1))
        ax.text(x + w / 2, y + h / 2, t, ha="center", va="center", fontsize=fs)

    def arr(x1, y1, x2, y2, t="", ls="-"):
        ax.annotate("", (x2, y2), (x1, y1), arrowprops=dict(arrowstyle="-|>", color="#333", lw=0.9, ls=ls))
        if t: ax.text((x1 + x2) / 2, max(y1, y2) + 0.12, t, fontsize=6.4, ha="center", style="italic")

    box(0.1, 3.3, 2.1, 1.0, "RIPS categories (12 + glass)\nwet-mass %, sampling year t_c\nstatus: local measurement", "#f6efe3", "#a07a3c")
    box(3.0, 3.3, 2.2, 1.0, "10 model fractions\n(IPCC-aligned)\nused for computation", "#e9f5ea", "#3b8a43")
    box(6.0, 3.3, 2.3, 1.0, "Table 2 parameters\nIPCC defaults + legacy LHV/tau\n+ gap scenarios", "#e8f0f8", "#46719e")
    box(9.0, 3.3, 1.9, 1.0, "LCA / TEA\nper tonne\n(model results)", "#eeeeee", "#666")
    arr(2.2, 3.8, 3.0, 3.8, "H1, L/W")
    arr(5.2, 3.8, 6.0, 3.8, "join")
    arr(8.3, 3.8, 9.0, 3.8)
    box(0.1, 0.4, 2.1, 1.6, "Edjabou hierarchy\nLevel I (10)\nLevel II (36)\nLevel III (56, provisional)", "#f6efe3", "#a07a3c")
    box(3.0, 0.4, 2.2, 1.6, "Allocated composition\nL1 / L2 / terminal\n(78 leaves; priors A01-A10)\nstatus: model estimate", "#fbe9e7", "#b5523b")
    box(6.0, 0.4, 2.3, 1.6, "Material parameters\nper (level, code)\nproxy / missing / gap\nDOC_wet = DOC_dry(1-w)", "#e8f0f8", "#46719e")
    box(9.0, 0.4, 1.9, 1.6, "Coverage report\n(% wet mass with\nknown parameter)\nnot used in results", "#eeeeee", "#666")
    arr(2.2, 1.2, 3.0, 1.2, "priors")
    arr(5.2, 1.2, 6.0, 1.2, "(level, code)")
    arr(8.3, 1.2, 9.0, 1.2)
    arr(1.15, 3.3, 1.15, 2.0, "")
    ax.text(1.25, 2.6, "RIPS aggregate\nsplit by priors", fontsize=6.6, style="italic")
    arr(4.1, 2.0, 4.1, 3.3, "", ls="--")
    ax.text(4.2, 2.45, "crosswalk to model fractions:\nreporting and consistency\ncheck only (dashed)", fontsize=6.4, style="italic")
    ax.text(8.4, 2.7, "Rule: model results never depend on Level II/III priors", fontsize=8, ha="center", weight="bold", color="#b5523b")
    fig.savefig(path, dpi=220, bbox_inches="tight"); plt.close(fig)
    return path


def fig_tonnage(d):
    path = FIG / "fig_tonnage.png"
    inp = d["inp"].sort_values("Q2025_tpd")
    fig, ax = plt.subplots(figsize=(8.5, 5.4))
    y = np.arange(len(inp))
    col = inp.tonnage_year_status.map({"stated_in_source_table": "#46719e", "overview_index_year_unverified": "#d9822b",
                                       "RIPS_projection_for_2025": "#7b4ea3"})
    ax.barh(y, inp.Q2025_tpd, color=col, height=0.7)
    alt = inp.Q2025_alt_tpd - inp.Q2025_tpd
    ax.barh(y, alt, left=inp.Q2025_tpd, color="none", edgecolor="#d9822b", hatch="///", height=0.7, lw=0.6)
    ax.scatter(inp.tonnage_source_value_tpd, y, marker="|", color="k", s=60, zorder=3)
    ax.set_yticks(y); ax.set_yticklabels(inp.city_name, fontsize=7.5); ax.set_xscale("log")
    ax.set_xlabel("2025 tonnage, t/day (log scale)")
    from matplotlib.patches import Patch
    from matplotlib.lines import Line2D
    ax.legend(handles=[Patch(color="#46719e", label="source-table year, projected once"),
                       Patch(color="#d9822b", label="overview total, year unverified (used as reported)"),
                       Patch(fc="none", ec="#d9822b", hatch="///", label="sensitivity: projected from composition year"),
                       Patch(color="#7b4ea3", label="RIPS projection for 2025"),
                       Line2D([], [], marker="|", ls="", color="k", label="reported source value Qt")],
              fontsize=7, frameon=False, loc="lower right")
    fig.tight_layout(); fig.savefig(path, dpi=200); plt.close(fig)
    return path


def fig_qmanaged(d):
    path = FIG / "fig_qmanaged.png"
    t = d["t5"].copy()
    t = t.sort_values("Q2025_tpd")
    fig, ax = plt.subplots(figsize=(8.5, 5.4)); y = np.arange(len(t))
    ax.scatter(100 * t.status_index_share, y, marker="o", color="#46719e", label="status index (M1)", zorder=3)
    ax.scatter(100 * t.local_share, y, marker="s", facecolor="none", edgecolor="#d9822b", label="local indicator", zorder=3)
    ax.scatter(100 * t.local_share_lower, y, marker="<", color="#b5523b", label="Padang: TPA-only lower bound", zorder=3)
    ax.scatter(100 * t.target_scenario_share, y, marker="x", color="#7b4ea3", label="Serang 2025 target (scenario)", zorder=3)
    for i, r in enumerate(t.itertuples()):
        v = [x for x in (r.status_index_share, r.local_share) if pd.notna(x)]
        if len(v) == 2: ax.plot([100 * v[0], 100 * v[1]], [i, i], color="#bbbbbb", lw=1, zorder=1)
    ax.set_yticks(y); ax.set_yticklabels(t.city_name, fontsize=7.5); ax.set_xlabel("Managed share of 2025 waste, %")
    ax.set_xlim(0, 100); ax.legend(fontsize=7, frameon=False, loc="lower right"); ax.grid(axis="x", alpha=0.3)
    fig.tight_layout(); fig.savefig(path, dpi=200); plt.close(fig)
    return path


def fig_robust(d):
    path = FIG / "fig_robust.png"
    rob = d["rob"].rename(columns=SCN)
    mc = d["mc"]
    pmax = {}
    for s, lab in SCN.items():
        w = mc[mc.scenario == s].pivot(index="city", columns="pathway", values="p_best")
        pmax[lab] = w.max(axis=1)
    pm = pd.DataFrame(pmax).reindex(rob.index)
    code = {"SL": 0, "S1": 1, "S2": 2, "S3": 3, "S4": 4, "S5": 5}
    cols = ["#8c8c8c", "#d62728", "#ff7f0e", "#2ca02c", "#9467bd", "#1f77b4"]
    from matplotlib.colors import ListedColormap
    M = rob.apply(lambda c: c.map(code)).to_numpy(float)
    fig, ax = plt.subplots(figsize=(9.5, 6.4))
    ax.imshow(np.ma.masked_invalid(M), cmap=ListedColormap(cols), vmin=-0.5, vmax=5.5, aspect="auto")
    for i in range(M.shape[0]):
        for j in range(M.shape[1]):
            v = pm.iloc[i, j]
            if pd.notna(v):
                ax.text(j, i, f"{v:.2f}", ha="center", va="center", fontsize=6.3, color="white" if M[i, j] in (1, 5) else "black")
            else:
                ax.text(j, i, "n.a.", ha="center", va="center", fontsize=6.3)
    ax.set_xticks(range(M.shape[1])); ax.set_xticklabels(rob.columns, rotation=40, ha="right", fontsize=7.5)
    ax.set_yticks(range(M.shape[0])); ax.set_yticklabels(rob.index, fontsize=7.5)
    from matplotlib.patches import Patch
    ax.legend(handles=[Patch(color=c, label=NAMES[k]) for k, c in zip(PW, cols)], fontsize=7, frameon=False,
              loc="upper left", bbox_to_anchor=(1.01, 1))
    fig.tight_layout(); fig.savefig(path, dpi=200); plt.close(fig)
    return path


def fig_hierarchy(d):
    path = FIG / "fig_hierarchy.png"
    fp, cr = d["fp"], d["cross"]
    st = fp.parameter_status.fillna("")
    cat = np.select([st.str.startswith("IPCC"), st.str.contains("proxy;"), st.str.contains("fibre portion"),
                     st.str.contains("missing")], ["IPCC category default", "parent-category proxy",
                                                    "missing (composite; fibre proxy only)", "missing"], "other")
    t = pd.crosstab(fp.level, cat)
    bm = d["bm"]
    obs = bm.groupby("level").equal_housing_mean_pct_total_wet.apply(lambda s: s.notna().sum())
    fig, ax = plt.subplots(1, 2, figsize=(10, 4.2))
    cols = {"IPCC category default": "#46719e", "parent-category proxy": "#8fb3d9",
            "missing (composite; fibre proxy only)": "#e6b06e", "missing": "#d9822b", "other": "#cccccc"}
    t = t[[c for c in cols if c in t.columns]]
    t.plot.barh(stacked=True, ax=ax[0], color=[cols[c] for c in t.columns], width=0.6)
    ax[0].set_yticklabels(["Level I (10)", "Level II (36)", "Level III (56)"]); ax[0].set_ylabel("")
    ax[0].set_xlabel("number of fractions"); ax[0].set_title("Material-parameter status", fontsize=9)
    ax[0].legend(fontsize=6.5, frameon=False, loc="upper center", bbox_to_anchor=(0.5, -0.2), ncol=2); ax[0].invert_yaxis()
    n = cr.groupby("level").size()
    ax[1].barh(["Level II (36)", "Level III (56)"], [n[2], n[3]], color="#dddddd", height=0.6, label="catalogued")
    ax[1].barh(["Level II (36)", "Level III (56)"], [obs.get(2, 0), obs.get(3, 0)], color="#3b8a43", height=0.6,
               label="with a Danish benchmark value")
    cs = bm[bm.sibling_set_complete].groupby("level").size()
    ax[1].barh(["Level II (36)", "Level III (56)"], [cs.get(2, 0), cs.get(3, 0)], color="#1f5f28", height=0.3,
               label="in a complete sibling set")
    ax[1].invert_yaxis(); ax[1].set_xlabel("number of fractions"); ax[1].legend(fontsize=6.5, frameon=False, loc="upper center", bbox_to_anchor=(0.5, -0.2), ncol=2)
    ax[1].set_title("Composition benchmarks (Edjabou et al. 2015)", fontsize=9)
    fig.tight_layout(); fig.savefig(path, dpi=200); plt.close(fig)
    return path

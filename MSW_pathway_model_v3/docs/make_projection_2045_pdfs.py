"""Build the two 2045 projection reports from outputs/proj2045_*:
docs/Projection_2045_Environmental_EN.pdf and docs/Projection_2045_Economic_EN.pdf. Run after run_projection_2045.py."""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from reportlab.platypus import SimpleDocTemplate, Spacer, PageBreak
from doccommon import *

COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}
CMP = pd.read_csv(OUT / "proj2045_comparison_central.csv")
DEC = pd.read_csv(OUT / "proj2045_decomposition.csv")
DIST = pd.read_csv(OUT / "proj2045_change_distribution.csv")
PATH = pd.read_csv(OUT / "proj2045_year_path.csv")
PB = pd.read_csv(OUT / "proj2045_p_best_fixed_carbon.csv")
BEST = pd.read_csv(OUT / "proj2045_best_central.csv", header=[0, 1], index_col=0)
GATES = pd.read_csv(OUT / "proj2045_scale_and_gates.csv", header=[0, 1], index_col=0)
FAC = pd.read_csv(OUT / "proj2045_factors.csv", index_col=0).value
PRM = pd.read_csv(DATA / "projection_2045_parameters.csv").set_index("key")
GRID = pd.read_csv(DATA / "grid_emission_factors.csv").set_index("grid_region").ef_kgCO2_per_kWh
gf = float(FAC["grid_factor"]); Y = int(float(PRM.loc["target_year", "value"]))
o = CMP[CMP.baseline == "OD"]; sl = CMP[CMP.baseline == "SL"]
med = lambda df, c, k: df[df.pathway == k][c].median()
rng = lambda df, c, k, f=".0f": f"{df[df.pathway == k][c].median():{f}} ({df[df.pathway == k][c].min():{f}} to {df[df.pathway == k][c].max():{f}})"
dmed = lambda ind, col, k: DEC[(DEC.indicator == ind) & (DEC.baseline == "OD") & (DEC.pathway == k)][col].median()


def common_method(A, focus):
    A(P("1 What is projected and how", "h1"))
    A(P(f"The six options are evaluated per tonne of mixed MSW at the facility gate for the existing condition of 2025 and for "
        f"{Y}, for the same 21 locations, with the same model, parameters and composition. Three drivers change between the two "
        "years (Table 1); everything else is held constant so that the difference isolates their effect. Both years use the same "
        "Monte Carlo random numbers, so changes are computed draw by draw (paired)."))
    A(eqrow(r"EF_{grid}(t)=EF_{grid}(2025)\,\max\left[0,\;1-\delta\,(t-2025)\right],\qquad \delta=\frac{1}{35}\ \mathrm{yr^{-1}}"
            r"\quad\Rightarrow\quad EF_{grid}(2045)=%.3f\,EF_{grid}(2025)" % gf, "p_grid", "1"))
    A(eqrow(r"x(t)=x(2025)\,(1+e_x)^{\,t-2025},\qquad Q(t)=Q(2025)\,(1+g)^{\,t-2025}", "p_esc", "2"))
    rows = [["Driver", "Rule", f"Factor 2025 to {Y}", "Affects"],
            ["Grid emission factor", "linear to zero in 2060 (&delta; = 1/35 per year)", f"&times; {gf:.3f}",
             "electricity credits of WtE and AD, electricity use of RDF and front-end sorting, fossil CED of grid electricity"],
            ["Electricity value", "real escalation 2%/yr", f"&times; {FAC['f_el']:.3f}", "revenue of WtE and AD"],
            ["Labour-driven O&amp;M", "real escalation 1%/yr", f"&times; {FAC['f_lab']:.3f}", "O&amp;M of WtE, RDF and AD plants"],
            ["Landfill cost", "real escalation 2%/yr (third rate, see note)", f"&times; {FAC['f_lf']:.3f}",
             "sanitary landfill and open-dump cost, including residue landfills of every option"],
            ["Tonnage", "each location's growth rate; composition constant", f"median &times; {FAC['tonnage_factor_median']:.2f}",
             "plant scale (economies of scale) and scale gates"]]
    A(table(rows, [3.2, 4.6, 2.6, 6.6]))
    A(P("Table 1: Drivers of the projection (data/projection_2045_parameters.csv). CAPEX is constant in real terms. The third "
        "escalation rate of the 2/1/2% set is applied to landfill cost, an interpretation to be confirmed. Grid factors in 2045: "
        + ", ".join(f"{k} {v:.3f} &rarr; {v * gf:.3f}" for k, v in GRID.items()) + " kg CO<sub>2</sub>/kWh.", "cap"))
    if focus in ("env", "both"):
        A(P("<b>Environmental indicators.</b> Only the grid driver changes greenhouse gases and fossil energy per tonne: escalation changes prices, not physical "
            "flows, and per-tonne emissions do not depend on plant size in this model. Landfill methane, fossil carbon of "
            "plastics and coal displaced in cement kilns are unchanged; a cleaner grid makes electricity exported by WtE and AD "
            "displace less CO<sub>2</sub>, and makes electricity consumed by RDF lines cleaner."))
    if focus in ("econ", "both"):
        A(P("<b>Economic indicators.</b> All three drivers change costs: escalation raises O&amp;M, landfill cost and electricity revenue; tonnage growth "
            "lowers the unit cost of capital through economies of scale (exponent b = 0.7) and of landfilling (elasticity "
            "e<sub>SL</sub>); the grid affects cost only indirectly, through the ranking by carbon-inclusive cost."))


# ===================================================================================== environmental
def fig_grid():
    yrs = np.arange(2025, 2061)
    fig, ax = plt.subplots(figsize=(7.5, 3.2))
    for k, v in GRID.items():
        ax.plot(yrs, v * np.maximum(0, 1 - (yrs - 2025) / 35), label=k)
    ax.axvline(Y, c="k", lw=0.6, ls=":"); ax.set_ylabel("kg CO2 per kWh"); ax.set_xlabel("Year"); ax.legend(frameon=False, fontsize=8)
    ax.set_title("Grid emission factor, linear path to net zero in 2060", fontsize=9)
    fig.tight_layout(); p = FIG / "pj_grid.png"; fig.savefig(p, dpi=200); plt.close(fig); return p


def fig_bars(c25, c45, ylab, name, scale=1.0):
    fig, ax = plt.subplots(figsize=(8.5, 3.6)); x = np.arange(len(PW))
    for dx, c, lab, al in ((-0.2, c25, "2025", 0.55), (0.2, c45, str(Y), 1.0)):
        m = [o[o.pathway == k][c].median() / scale for k in PW]
        lo = [m[i] - o[o.pathway == k][c].min() / scale for i, k in enumerate(PW)]
        hi = [o[o.pathway == k][c].max() / scale - m[i] for i, k in enumerate(PW)]
        ax.bar(x + dx, m, 0.38, yerr=[lo, hi], capsize=2, color=[COL[k] for k in PW], alpha=al, label=lab,
               edgecolor="k", linewidth=0.3)
    ax.set_xticks(x, [NAMES[k] for k in PW], fontsize=8); ax.axhline(0, c="k", lw=0.5); ax.set_ylabel(ylab)
    ax.set_title(f"Median over 21 locations (bars) and range (whiskers); light = 2025, dark = {Y}", fontsize=9)
    fig.tight_layout(); p = FIG / name; fig.savefig(p, dpi=200); plt.close(fig); return p


def fig_path(col, ylab, name, title):
    fig, ax = plt.subplots(1, 2, figsize=(11, 3.6))
    for a, loc in zip(ax, ("median", "Kota Padang")):
        q = PATH[PATH.location == loc]
        for k in PW:
            v = q[q.pathway == k]; a.plot(v.year, v[col], "-o", ms=3, color=COL[k], label=NAMES[k])
        a.axvline(Y, c="k", lw=0.6, ls=":"); a.set_xlabel("Year"); a.set_ylabel(ylab)
        a.set_title(("Median of 21 locations" if loc == "median" else loc) + f": {title}", fontsize=9)
    ax[0].legend(fontsize=7, frameon=False); fig.tight_layout(); p = FIG / name; fig.savefig(p, dpi=200); plt.close(fig); return p


def env_findings(A):
    sp = sl.pivot(index="city", columns="pathway")
    n45 = int((sp["dG_2045"]["S5"] > sp["dG_2045"]["S1"]).sum()); n25 = int((sp["dG_2025"]["S5"] > sp["dG_2025"]["S1"]).sum())
    dS1 = med(o, "G_change", "S1"); dS2 = med(o, "G_change", "S2"); dS3 = med(o, "G_change", "S3"); dS5 = med(o, "G_change", "S5")
    box = [P("<b>Key findings: environmental performance</b>", "box")] + bullets([
        f"By {Y} the grid emits {100 * (1 - gf):.0f}% less per kWh. This is the only driver that changes emissions per tonne.",
        f"<b>WtE loses most</b>: its GHG rises by {dS1:.0f} kg CO<sub>2</sub>e/t (median), from {med(o, 'G_2025', 'S1'):.0f} to "
        f"{med(o, 'G_2045', 'S1'):.0f}, because each exported kWh displaces less coal-based power; its GHG avoided against the "
        f"landfill falls from {med(sl, 'dG_2025', 'S1'):.0f} to {med(sl, 'dG_2045', 'S1'):.0f} kg CO<sub>2</sub>e/t.",
        f"<b>AD alone</b> also loses credit (+{dS3:.0f} kg/t). <b>RDF improves</b> ({dS2:.0f} kg/t) because its electricity use "
        f"becomes cleaner while the coal it displaces in kilns does not change; <b>RDF + AD</b> is almost unchanged ({dS5:+.0f} kg/t).",
        "The <b>landfill</b> and <b>PHB</b> do not change: their emissions do not involve grid electricity.",
        f"RDF + AD avoids more GHG against the landfill than WtE in {n45} of 21 locations in {Y}, against {n25} of 21 in 2025. "
        "Climate benefits of energy recovery that rely on displacing grid power shrink as the grid decarbonises; benefits that "
        "rely on keeping organics out of landfills and displacing kiln coal persist.",
        f"Fossil-energy savings follow the same pattern: WtE saves {-med(o, 'CED_2045', 'S1') / 1e3:.1f} GJ/t in {Y} against "
        f"{-med(o, 'CED_2025', 'S1') / 1e3:.1f} GJ/t in 2025."], "box")
    A(boxed(box)); A(Spacer(1, 6))


def env_body(A):
    A(PageBreak()); A(P("Part A &mdash; Environmental performance", "h1"))
    A(figure(fig_grid(), 13, "Figure 1: Grid emission factors of the three Indonesian systems on the assumed path (ESDM 2018 "
             "values in 2025)."))
    A(P("2 Greenhouse gases per tonne, 2025 and " + str(Y), "h1"))
    rows = [["Option", "G 2025", f"G {Y}", "Change", "Change %", "Avoided vs OD 2025", f"Avoided vs OD {Y}",
             "Avoided vs SL 2025", f"Avoided vs SL {Y}"]]
    for k in PW:
        rows.append([NAMES[k], rng(o, "G_2025", k), rng(o, "G_2045", k), f"{med(o, 'G_change', k):+.0f}",
                     f"{med(o, 'G_change_pct', k):+.0f}%", f"{med(o, 'dG_2025', k):.0f}", f"{med(o, 'dG_2045', k):.0f}",
                     "-" if k == "SL" else f"{med(sl, 'dG_2025', k):.0f}", "-" if k == "SL" else f"{med(sl, 'dG_2045', k):.0f}"])
    A(table(rows, [2.8, 2.5, 2.5, 1.3, 1.4, 1.7, 1.7, 1.6, 1.6]))
    A(P("Table 2: GHG per tonne of MSW (kg CO<sub>2</sub>e/t, GWP100), central values, market case: median (minimum to "
        "maximum) over 21 locations; avoided = baseline minus option (medians). Change and change % are medians of the "
        "per-location changes.", "cap"))
    A(figure(fig_bars("G_2025", "G_2045", "kg CO2e per t MSW", "pj_ghg.png"), 15.5,
             f"Figure 2: GHG per tonne in 2025 and {Y}."))
    rows = [["Option", "Grid driver", "Cost drivers", "Tonnage", "Total change"]]
    for k in PW:
        rows.append([NAMES[k]] + [f"{dmed('G', c, k):+.1f}" for c in ("grid only", "costs only", "tonnage only", "2045")])
    A(table(rows, [3.4, 3.2, 3.2, 3.2, 3.2]))
    A(P("Table 3: Decomposition of the change in GHG (kg CO<sub>2</sub>e/t, median over locations), each driver applied alone. "
        "Escalation and tonnage do not change emissions per tonne.", "cap"))
    dd = DIST[DIST.indicator == "G"]
    rows = [["Option", "Change p5", "Change median", "Change p95", "Locations where GHG rises (median change > 0)"]]
    for k in PW:
        q = dd[dd.pathway == k]
        rows.append([NAMES[k], f"{q.change_p5.median():+.0f}", f"{q.change_p50.median():+.0f}", f"{q.change_p95.median():+.0f}",
                     str(int((q.change_p50 > 0.5).sum()))])
    A(table(rows, [3.4, 2.4, 2.6, 2.4, 6.2]))
    A(P("Table 4: Uncertainty of the change (paired Monte Carlo, 4,000 draws per location): 5th, 50th and 95th percentile "
        "of G(" + str(Y) + ") &minus; G(2025), median over locations, kg CO<sub>2</sub>e/t. The spread comes from the grid "
        "multiplier, efficiencies and composition.", "cap"))
    A(figure(fig_path("G", "kg CO2e per t MSW", "pj_ghg_path.png", "GHG per tonne"), 16,
             "Figure 3: GHG per tonne from 2025 to 2060 on the assumed grid path (central values, all drivers)."))
    A(P("3 Fossil cumulative energy demand", "h1"))
    rows = [["Option", "CED 2025 (GJ/t)", f"CED {Y} (GJ/t)", "Change (GJ/t)"]]
    for k in PW:
        rows.append([NAMES[k], f"{med(o, 'CED_2025', k) / 1e3:.2f}", f"{med(o, 'CED_2045', k) / 1e3:.2f}",
                     f"{(med(o, 'CED_2045', k) - med(o, 'CED_2025', k)) / 1e3:+.2f}"])
    A(table(rows, [4.0, 4.0, 4.0, 4.0]))
    A(P("Table 5: Fossil CED per tonne (negative = saving), medians over 21 locations. The fossil primary energy of grid "
        "electricity is scaled with the same grid path, so savings from exported electricity shrink.", "cap"))
    A(figure(fig_bars("CED_2025", "CED_2045", "GJ fossil per t MSW", "pj_ced.png", 1e3), 15.5,
             f"Figure 4: Fossil CED per tonne in 2025 and {Y}."))
    A(P("4 Per-location environmental results", "h1"))
    rows = [["Location"] + [f"{k} 2025 / {Y}" for k in ("S1", "S2", "S3", "S5")] + ["Avoided vs SL: S5 &minus; S1, " + str(Y)]]
    for city in dict.fromkeys(o.city):
        q = o[o.city == city].set_index("pathway"); s_ = sl[sl.city == city].set_index("pathway")
        rows.append([city] + [f"{q.loc[k, 'G_2025']:.0f} / {q.loc[k, 'G_2045']:.0f}" for k in ("S1", "S2", "S3", "S5")]
                    + [f"{s_.loc['S5', 'dG_2045'] - s_.loc['S1', 'dG_2045']:+.0f}"])
    A(table(rows, [3.6, 2.6, 2.6, 2.6, 2.6, 3.0]))
    A(P("Table 6: GHG per tonne (kg CO<sub>2</sub>e/t) for the grid-dependent options in each location, and the difference in "
        f"GHG avoided against the landfill between RDF + AD and WtE in {Y} (positive = RDF + AD avoids more).", "cap"))
    A(P("5 Environmental interpretation and limits", "h1"))
    for t in bullets([
        "Energy recovery evaluated with today's grid factor overstates its long-term climate benefit. A WtE plant commissioned "
        "around 2030 operates through the decarbonisation period, so its lifetime-average credit is closer to the 2045 value "
        "than to the 2025 value.",
        "Options whose benefit comes from avoiding landfill methane and displacing kiln coal (RDF, RDF + AD) keep their benefit; "
        "this is a robust argument for diversion of organics and RDF where a kiln is in reach.",
        "The landfill result assumes the same gas collection in both years; better collection or methane regulation would "
        "lower it, and kiln decarbonisation (less coal) would reduce the RDF credit. Neither is part of this projection.",
        "Composition is held at today's proxy; a richer, more plastic-intensive waste in 2045 would raise WtE emissions further.",
        "Screening-level results; all drivers are user-specified scenario assumptions, not forecasts."]):
        A(t)


# ===================================================================================== economic
def fig_dec_cost():
    fig, ax = plt.subplots(figsize=(8.5, 3.6)); x = np.arange(len(PW)); w = 0.2
    for i, (c, lab, colr) in enumerate((("costs only", "escalation", "#d9822b"), ("tonnage only", "tonnage growth (scale)", "#46719e"),
                                        ("interaction", "interaction", "#999999"), ("2045", "total", "#222222"))):
        ax.bar(x + (i - 1.5) * w, [dmed("C", c, k) for k in PW], w, label=lab, color=colr)
    ax.axhline(0, c="k", lw=0.5); ax.set_xticks(x, [NAMES[k] for k in PW], fontsize=8); ax.set_ylabel("USD per t MSW")
    ax.legend(fontsize=7, frameon=False); ax.set_title(f"Change in net cost 2025 to {Y} by driver (median over locations)", fontsize=9)
    fig.tight_layout(); p = FIG / "pj_cost_dec.png"; fig.savefig(p, dpi=200); plt.close(fig); return p


def _pb():
    t25 = PB[PB.year == 2025] if PB.year.dtype != object else PB[PB.year == "2025"]
    t45 = PB[PB.year == Y] if PB.year.dtype != object else PB[PB.year == str(Y)]
    mp = lambda t, pc: t[t.carbon_value == pc].sort_values("p_best", ascending=False).groupby("city").head(1).pathway.value_counts().to_dict()
    return t25, t45, mp


def econ_findings(A):
    t25, t45, mp = _pb()
    psel25 = int(GATES[("2025", "gate_PSEL_generated")].sum()); psel45 = int(GATES[(str(Y), "gate_PSEL_generated")].sum())
    box = [P("<b>Key findings: economic performance</b>", "box")] + bullets([
        f"Net cost of the <b>landfill</b> rises from {med(o, 'C_2025', 'SL'):.1f} to {med(o, 'C_2045', 'SL'):.1f} USD/t (median): "
        "landfill escalation outweighs the economies of scale of larger sites.",
        f"<b>WtE becomes cheaper</b> ({med(o, 'C_2025', 'S1'):.1f} &rarr; {med(o, 'C_2045', 'S1'):.1f} USD/t): electricity "
        f"revenue grows by {100 * (FAC['f_el'] - 1):.0f}% and larger plants are cheaper per tonne. Locations reaching 1,000 t/day "
        f"(Perpres 109/2025 tariff) rise from {psel25} to {psel45}.",
        f"<b>RDF, AD and RDF + AD</b> become dearer by about 5&ndash;8 USD/t, mainly through O&amp;M and residue-landfill escalation.",
        f"The <b>abatement cost</b> of RDF + AD against the landfill stays near {med(sl, 'MAC_2045', 'S5'):.0f} USD/t CO<sub>2</sub>e "
        f"(2025: {med(sl, 'MAC_2025', 'S5'):.0f}); that of WtE stays near {med(sl, 'MAC_2045', 'S1'):.0f} because its lower cost is "
        "offset by its smaller climate benefit.",
        f"<b>The preferred option hardly changes</b>: landfill up to 50 USD/t in both years; at 100 USD/t RDF + AD is most probable "
        f"in {mp(t45, 100).get('S5', 0)} locations in {Y} (2025: {mp(t25, 100).get('S5', 0)}). The decision rules of the main "
        "study remain valid under these drivers."], "box")
    A(boxed(box))


def econ_body(A):
    t25, t45, mp = _pb()
    A(PageBreak()); A(P("Part B &mdash; Economic performance", "h1"))
    A(P("6 Net cost per tonne, 2025 and " + str(Y), "h1"))
    rows = [["Option", "C 2025", f"C {Y}", "Change", "Change %", "Escalation", "Tonnage scale", "Interaction"]]
    for k in PW:
        rows.append([NAMES[k], rng(o, "C_2025", k, ".1f"), rng(o, "C_2045", k, ".1f"), f"{med(o, 'C_change', k):+.1f}",
                     f"{med(o, 'C_change_pct', k):+.0f}%", f"{dmed('C', 'costs only', k):+.1f}", f"{dmed('C', 'tonnage only', k):+.1f}",
                     f"{dmed('C', 'interaction', k):+.1f}"])
    A(table(rows, [2.9, 2.9, 2.9, 1.4, 1.4, 1.8, 2.0, 1.7]))
    A(P("Table 7: Net levelised cost per tonne (USD/t, real 2025 USD), central values, market case: median (range) over 21 "
        "locations, and the median change attributed to each driver applied alone.", "cap"))
    A(figure(fig_bars("C_2025", "C_2045", "USD per t MSW", "pj_cost.png"), 15.5, f"Figure 5: Net cost per tonne in 2025 and {Y}."))
    A(figure(fig_dec_cost(), 15.5, "Figure 6: Decomposition of the change in net cost."))
    dd = DIST[DIST.indicator == "C"]
    rows = [["Option", "Change p5", "Change median", "Change p95"]]
    for k in PW:
        q = dd[dd.pathway == k]
        rows.append([NAMES[k], f"{q.change_p5.median():+.1f}", f"{q.change_p50.median():+.1f}", f"{q.change_p95.median():+.1f}"])
    A(table(rows, [4.4, 3.8, 3.8, 3.8]))
    A(P("Table 8: Uncertainty of the change in net cost (paired Monte Carlo, USD/t, median over locations).", "cap"))
    A(P("7 Abatement cost and carbon-inclusive cost", "h1"))
    rows = [["Option", "MAC vs SL 2025", f"MAC vs SL {Y}", "CIC at 50: 2025", f"CIC at 50: {Y}", "CIC at 100: 2025", f"CIC at 100: {Y}"]]
    for k in PW:
        rows.append([NAMES[k], "-" if k == "SL" else f"{med(sl, 'MAC_2025', k):.0f}", "-" if k == "SL" else f"{med(sl, 'MAC_2045', k):.0f}",
                     f"{med(o, 'CIC50_2025', k):.1f}", f"{med(o, 'CIC50_2045', k):.1f}", f"{med(o, 'CIC100_2025', k):.1f}",
                     f"{med(o, 'CIC100_2045', k):.1f}"])
    A(table(rows, [3.0, 2.3, 2.3, 2.4, 2.4, 2.3, 2.3]))
    A(P("Table 9: Abatement cost against the landfill (USD/t CO<sub>2</sub>e) and carbon-inclusive cost C + pG/1000 (USD/t) "
        "at 50 and 100 USD/t CO<sub>2</sub>e, medians over 21 locations.", "cap"))
    A(figure(fig_path("CIC100", "USD per t MSW", "pj_cic_path.png", "carbon-inclusive cost at 100 USD/t"), 16,
             "Figure 7: Carbon-inclusive cost at 100 USD/t CO<sub>2</sub>e from 2025 to 2060 (central values, all drivers)."))
    A(P("8 Preferred option at fixed carbon values", "h1"))
    rows = [["Carbon value (USD/t CO2e)", "Most probable 2025 (locations)", f"Most probable {Y} (locations)",
             "Central best 2025", f"Central best {Y}"]]
    for pc in (0, 2, 25, 50, 100):
        f_ = lambda d: ", ".join(f"{k} {v}" for k, v in sorted(d.items()))
        cb = lambda yr: f_(BEST[(yr, f"best_at_{pc}")].value_counts().to_dict()) if (yr, f"best_at_{pc}") in BEST.columns else "as at 0"
        rows.append([str(pc), f_(mp(t25, pc)), f_(mp(t45, pc)), cb("2025"), cb(str(Y))])
    A(table(rows, [3.0, 3.6, 3.6, 3.4, 3.4]))
    A(P("Table 10: Preferred option, 21 locations, market case (Monte Carlo 4,000 draws per location and carbon value; central "
        "values for the last two columns).", "cap"))
    rows = [["Location", "Q 2025 t/d", f"Q {Y} t/d", "P(S5) at 100: 2025", f"P(S5) at 100: {Y}", "P(SL) at 100: 2025",
             f"P(SL) at 100: {Y}", f"Central best at 100: 2025 / {Y}"]]
    for city in dict.fromkeys(o.city):
        g = lambda t, k: t[(t.city == city) & (t.carbon_value == 100) & (t.pathway == k)].p_best.iloc[0]
        rows.append([city, f"{GATES.loc[city, ('2025', 'Q_2025')]:,.0f}", f"{GATES.loc[city, (str(Y), 'Q_2025')]:,.0f}",
                     f"{g(t25, 'S5'):.2f}", f"{g(t45, 'S5'):.2f}", f"{g(t25, 'SL'):.2f}", f"{g(t45, 'SL'):.2f}",
                     f"{BEST.loc[city, ('2025', 'best_at_100')]} / {BEST.loc[city, (str(Y), 'best_at_100')]}"])
    A(table(rows, [3.4, 1.6, 1.6, 1.9, 1.9, 1.9, 1.9, 2.8]))
    A(P("Table 11: Per-location results at 100 USD/t CO<sub>2</sub>e (standard error of each probability &le; 0.008).", "cap"))
    A(P("9 Economic interpretation and limits", "h1"))
    for t in bullets([
        "Two effects cancel for WtE: higher electricity revenue and larger plants make it cheaper, while the cleaner grid "
        "removes much of its climate credit. Its position relative to RDF + AD therefore changes little, and it remains "
        "dependent on the tariff and on scale.",
        "Rising landfill costs make every option that reduces landfilling relatively more attractive, but by 2045 the effect is "
        "a few USD per tonne, smaller than the gap that the carbon value has to close.",
        "The 1,000 t/day gate of Perpres 109/2025 is reached by more locations as waste grows; whether that waste is collected is "
        "a separate question (managed shares are far lower today).",
        "CAPEX is constant in real terms and technology learning is not modelled; learning would lower the cost of the newer "
        "options (AD, RDF + AD, PHB). The Perpres tariff is held constant in real terms.",
        "Escalation rates and the grid path are user-specified scenario assumptions; the third rate of the 2/1/2% set is applied "
        "to landfill cost (to be confirmed). The parameters are in data/projection_2045_parameters.csv and the analysis reruns "
        "with python run_projection_2045.py."]):
        A(t)


def build():
    story = []; A = story.append
    A(P(f"Projection to {Y}: Environmental and Economic Performance of MSW Recovery Pathways", "title"))
    A(P(f"Six options for 21 Indonesian cities and regencies in {Y} against the existing condition of 2025: greenhouse "
        "gases, fossil energy, net cost, abatement cost and the preferred option, with a grid decarbonising linearly to net "
        "zero in 2060 and real escalation of electricity (2%/yr), labour-driven O&amp;M (1%/yr) and landfill cost (2%/yr) "
        "&mdash; MSW pathway model v3.2", "subtitle"))
    A(P("Muhammad Fachri Ridwan &mdash; The University of Queensland &mdash; October 2026 &mdash; companion to "
        "MSW_Methodology_v3_EN.pdf and MSW_Mathematical_Model_EN.pdf", "subtitle"))
    A(Spacer(1, 6))
    env_findings(A); econ_findings(A)
    common_method(A, "both")
    env_body(A); econ_body(A)
    A(P("10 Joint reading: what changes for the decision", "h1"))
    t25, t45, mp = _pb()
    for x in bullets([
        "Environmentally, the cleaner grid erodes the credit of options that export electricity (WtE, AD) and slightly "
        "improves options that consume it (RDF); landfill-based options do not change.",
        "Economically, escalation makes landfilling dearer and electricity more valuable, and growing tonnage brings "
        "economies of scale; WtE becomes cheaper while the other recovery options become slightly dearer.",
        f"The two effects largely offset each other for WtE, whose abatement cost stays near {med(sl, 'MAC_2045', 'S1'):.0f} "
        f"USD/t CO<sub>2</sub>e, while RDF + AD keeps both its climate benefit and its abatement cost (about "
        f"{med(sl, 'MAC_2045', 'S5'):.0f} USD/t CO<sub>2</sub>e).",
        f"The preferred option is stable: landfill up to 50 USD/t CO<sub>2</sub>e in both years; at 100 USD/t RDF + AD is most "
        f"probable in {mp(t45, 100).get('S5', 0)} of 21 locations in {Y} (2025: {mp(t25, 100).get('S5', 0)}). The decision "
        "rules derived for 2025 therefore remain valid for plants that operate into the 2040s.",
        "For stakeholders, a WtE business case should be evaluated with a declining grid factor over the plant's life, not "
        "with today's value; the climate case for RDF + AD and for source-separated AD does not depend on the grid."]):
        A(x)
    doc = SimpleDocTemplate(str(DOCS / "Projection_2045_EN.pdf"), pagesize=A4, leftMargin=2 * cm, rightMargin=2 * cm,
                            topMargin=1.8 * cm, bottomMargin=1.6 * cm, title=f"Projection to {Y}",
                            author="Muhammad Fachri Ridwan")
    deco = page_deco(f"Projection to {Y}: environmental and economic performance")
    doc.build(story, onFirstPage=deco, onLaterPages=deco)
    print("wrote", DOCS / "Projection_2045_EN.pdf")


build()

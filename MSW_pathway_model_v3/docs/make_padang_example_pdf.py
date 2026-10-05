"""Build docs/Worked_Example_Kota_Padang_EN.pdf: a step-by-step calculation for one location, with its uncertainty
and sensitivity analysis. Every number is computed by padang_calc.py / padang_uncertainty.py and checked against the
model."""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from reportlab.platypus import SimpleDocTemplate, Spacer, PageBreak
from doccommon import *
from padang_calc import calc
from padang_uncertainty import run

o = calc(DATA); u = run(DATA)
res, PR, ctx, tn, nm, bk = o["res"], o["P"], o["ctx"], o["tonnage"], o["norm"], o["bulk"]
FR = o["M"].FR
COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}
f0 = lambda x, d=1: f"{x:,.{d}f}"
story = []; A = story.append
RUN = "Worked example: Kota Padang (MSW pathway model v3.1)"

# ------------------------------------------------------------------------------------------------ figures
def fig_comp():
    fr = o["frac"]; x = np.arange(len(FR))
    fig, ax = plt.subplots(figsize=(9, 3.4))
    ax.bar(x - 0.27, 100 * fr.s_dom, 0.27, label=f"domestic ({100*nm['w_dom']:.1f}% of mass)", color="#46719e")
    ax.bar(x, 100 * fr.s_nd, 0.27, label=f"non-domestic ({100*nm['w_nd']:.1f}%)", color="#d9822b")
    ax.bar(x + 0.27, 100 * fr.s, 0.27, label="combined (used)", color="#3b8a43")
    ax.set_xticks(x, FR, fontsize=8); ax.set_ylabel("% of wet mass"); ax.legend(frameon=False, fontsize=8)
    fig.tight_layout(); p = FIG / "pd_comp.png"; fig.savefig(p, dpi=200); plt.close(fig); return p


def fig_contrib():
    t = o["char"]
    fig, ax = plt.subplots(1, 4, figsize=(11, 3.3), sharey=True)
    for a, col, lab, c in zip(ax, ("dry", "CH4_MCF1", "CO2_fossil", "LHV_contrib"),
                              ("Dry matter (t/t)", "CH4 potential (kg/t)", "Fossil CO2 if burned (kg/t)", "LHV contribution (MJ/kg)"),
                              ("#7f7f7f", "#2ca02c", "#d62728", "#ff7f0e")):
        a.barh(FR[::-1], t[col].values[::-1], color=c); a.set_title(lab, fontsize=8.5); a.axvline(0, c="k", lw=0.5)
        a.tick_params(labelsize=7.5)
    fig.tight_layout(); p = FIG / "pd_contrib.png"; fig.savefig(p, dpi=200); plt.close(fig); return p


def fig_components(key, unit, fname, title):
    keys = ["OD"] + PW if key == "parts" else PW
    fig, ax = plt.subplots(figsize=(10, 4.2))
    cmap = plt.get_cmap("tab20")
    labels = {}
    for i, k in enumerate(keys):
        comp = res[k].get(key, {"total": res[k]["G" if key == "parts" else "C"]})
        pos = neg = 0
        for name, val in comp.items():
            base = name.replace("AD_", "").replace("RDF_", "")
            if base not in labels: labels[base] = cmap(len(labels) % 20)
            if val >= 0:
                ax.bar(i, val, bottom=pos, color=labels[base], edgecolor="white", lw=0.4); pos += val
            else:
                ax.bar(i, val, bottom=neg, color=labels[base], edgecolor="white", lw=0.4, hatch="//"); neg += val
        tot = res[k]["G" if key == "parts" else "C"]
        ax.plot(i, tot, marker="D", color="k", ms=6, zorder=5); ax.text(i + 0.12, tot, f"{tot:,.0f}" if key == "parts" else f"{tot:,.1f}", fontsize=8, va="center")
    ax.axhline(0, c="k", lw=0.6)
    ax.set_xticks(range(len(keys)), [("OD open dump" if k == "OD" else NAMES[k]) for k in keys], fontsize=8)
    ax.set_ylabel(unit); ax.set_title(title, fontsize=9.5)
    from matplotlib.patches import Patch
    ax.legend(handles=[Patch(color=c, label=n.replace("_", " ")) for n, c in labels.items()], fontsize=7, frameon=False,
              loc="upper left", bbox_to_anchor=(1, 1))
    fig.tight_layout(); p = FIG / fname; fig.savefig(p, dpi=200); plt.close(fig); return p


def fig_mc():
    R = u["base"]["R"]
    fig, ax = plt.subplots(1, 3, figsize=(11, 3.6))
    for a, (arr, lab) in zip(ax, ((R["G"], "GHG, kg CO2e/t"), (MSWModel_delta(R, "OD"), "GHG avoided vs open dump, kg CO2e/t"),
                                  (R["C"], "Net cost, USD/t"))):
        bp = a.boxplot([arr[:, j] for j in range(6)], whis=(5, 95), showfliers=False, patch_artist=True)
        for patch, k in zip(bp["boxes"], PW): patch.set_facecolor(COL[k]); patch.set_alpha(0.6)
        a.set_xticks(range(1, 7), PW); a.set_title(lab, fontsize=9); a.axhline(0, c="k", lw=0.4)
    fig.tight_layout(); p = FIG / "pd_mc.png"; fig.savefig(p, dpi=200); plt.close(fig); return p


def MSWModel_delta(R, bg):
    return R["G_od"][:, None] - R["G"] if bg == "OD" else R["G"][:, [0]] - R["G"]


def fig_decision():
    fig, ax = plt.subplots(1, 2, figsize=(11, 3.8))
    sc = u["sc_curve"]
    for k in PW:
        ls = "-" if u["feasible_central"][k] else ":"
        ax[0].plot(sc.index, sc[k], ls, color=COL[k], label=NAMES[k] + ("" if u["feasible_central"][k] else " (fails a gate)"))
    ax[0].set_xlabel("Carbon value, USD/t CO2e"); ax[0].set_ylabel("Social cost C + pG, USD/t"); ax[0].legend(fontsize=7, frameon=False)
    ax[0].set_title("Central values", fontsize=9)
    pb = u["p_by_pc"][PW]
    x = np.arange(len(pb)); bot = np.zeros(len(pb))
    for k in PW:
        ax[1].bar(x, pb[k], bottom=bot, color=COL[k], width=0.85); bot += pb[k].values
    ax[1].set_xticks(x, [f"{int(i.left)+ (i.left>0)}-{int(i.right)}" for i in pb.index], fontsize=7, rotation=30)
    ax[1].set_xlabel("Carbon value bin, USD/t CO2e"); ax[1].set_ylabel("Probability of being best"); ax[1].set_ylim(0, 1)
    ax[1].set_title("Monte Carlo (4,000 draws)", fontsize=9)
    fig.tight_layout(); p = FIG / "pd_decision.png"; fig.savefig(p, dpi=200); plt.close(fig); return p


def fig_tornado():
    t = u["tornado"]; b = u["tornado_base"]
    fig, ax = plt.subplots(1, 2, figsize=(11, 4.6))
    for a, out_, lab in zip(ax, ("gap_S5_SL", "G_SL"), ("Social-cost gap RDF + AD minus landfill at 50 USD/t (USD/t)",
                                                        "GHG of sanitary landfill (kg CO2e/t)")):
        q = t[t.output == out_].sort_values("swing", ascending=False).head(12).iloc[::-1]
        y = np.arange(len(q))
        a.barh(y, q.at_low - b[out_], left=b[out_], color="#8fb3d9", label="input at low value")
        a.barh(y, q.at_high - b[out_], left=b[out_], color="#d9822b", label="input at high value")
        a.set_yticks(y, q.input, fontsize=7.5); a.axvline(b[out_], c="k", lw=0.8)
        if out_ == "gap_S5_SL": a.axvline(0, c="r", lw=0.8, ls="--")
        a.set_title(lab, fontsize=8.5); a.legend(fontsize=7, frameon=False)
    fig.tight_layout(); p = FIG / "pd_tornado.png"; fig.savefig(p, dpi=200); plt.close(fig); return p


def fig_sobol():
    s = u["sobol"]
    fig, ax = plt.subplots(1, 3, figsize=(11, 3.8))
    for a, out_, lab in zip(ax, ("SCgap_S5_SL", "G_SL", "C_S5"), ("Social-cost gap S5 - SL at 50 USD/t", "GHG of landfill", "Cost of RDF + AD")):
        q = s[s.output == out_].sort_values("ST", ascending=False).head(8).iloc[::-1]
        a.barh(q.input, q.ST, color="#c9d6e8", label="total ST"); a.barh(q.input, q.S1, color="#46719e", height=0.45, label="first order S1")
        a.set_title(lab, fontsize=9); a.tick_params(labelsize=7.5); a.legend(fontsize=7, frameon=False)
    fig.tight_layout(); p = FIG / "pd_sobol.png"; fig.savefig(p, dpi=200); plt.close(fig); return p


# ------------------------------------------------------------------------------------------------ document
r = o["row"]
A(P("Worked Example: Kota Padang", "title"))
A(P("Step-by-step calculation of climate impact, cost, fossil energy and land take per tonne of MSW, with uncertainty "
    "and sensitivity analysis &mdash; MSW pathway model v3.1, baseline year 2025", "subtitle"))
A(P("Muhammad Fachri Ridwan &mdash; The University of Queensland &mdash; October 2026 &mdash; companion to "
    "MSW_Methodology_v3_EN.pdf", "subtitle"))
A(Spacer(1, 6))
pbest = lambda k: u["dist"].query("indicator == 'p_best' and option == @k").p50.iloc[0]
sobST = lambda o_, i_: u["sobol"].query("output == @o_ and input == @i_").ST.iloc[0]
pstar = 1e3 * (res["S5"]["C"] - res["SL"]["C"]) / (res["SL"]["G"] - res["S5"]["G"])
best50 = min((k for k in PW if u["feasible_central"][k]), key=lambda k: res[k]["C"] + 50 * res[k]["G"] / 1e3)
box = [P("<b>Summary for Kota Padang (central values unless stated)</b>", "box")] + bullets([
    f"2025 tonnage {tn['Q2025']:,.1f} t/day; composition sampled in {int(r.composition_sampling_year)} and used as a 2025 "
    f"proxy; bulk moisture {bk['moisture']:.2f}; LHV {bk['LHV']:.2f} MJ/kg; methane potential {bk['L0']:.1f} kg CH<sub>4</sub>/t "
    f"(MCF = 1); fossil CO<sub>2</sub> if burned {bk['fossil']:.0f} kg/t.",
    f"GHG per tonne: open dump {res['OD']['G']:.0f}, sanitary landfill {res['SL']['G']:.0f}, WtE {res['S1']['G']:.0f}, "
    f"RDF {res['S2']['G']:.0f}, AD {res['S3']['G']:.0f}, PHB {res['S4']['G']:.0f}, RDF + AD {res['S5']['G']:.0f} "
    "kg CO<sub>2</sub>e/t.",
    f"Net cost per tonne: landfill {res['SL']['C']:.1f}, WtE {res['S1']['C']:.1f}, RDF {res['S2']['C']:.1f}, AD "
    f"{res['S3']['C']:.1f}, PHB {res['S4']['C']:.1f}, RDF + AD {res['S5']['C']:.1f} USD/t.",
    f"At a carbon value of 50 USD/t the lowest social cost is {NAMES[best50]}; RDF + AD overtakes the landfill at "
    f"{pstar:.0f} USD/t CO<sub>2</sub>e.",
    f"Monte Carlo (4,000 draws, carbon value 0&ndash;100 USD/t): RDF + AD is best in "
    f"{100*pbest('S5'):.0f}% of draws and the landfill in {100*pbest('SL'):.0f}%: a statistical tie.",
    "Sensitivity: as-received moisture and landfill-gas collection efficiency dominate the comparison between landfill "
    "and RDF + AD (Sobol total indices "
    f"{sobST('SCgap_S5_SL', 'wetness'):.2f} and {sobST('SCgap_S5_SL', 'cap'):.2f})."], "box")
A(boxed(box))
A(P("<b>How this document was produced.</b> <i>docs/padang_calc.py</i> writes out every equation of the model again "
    "with named intermediate quantities and stops if any result differs from the model output (GHG, cost, CED and land "
    "take of all options agree to 10<super>&minus;6</super>). <i>docs/padang_uncertainty.py</i> runs the Monte Carlo with "
    "the same random numbers as the 21-location analysis, so the probabilities are identical to Table 7 of the "
    "methodology. Symbols follow the methodology (Section 4.1)."))

# ---- 1 inputs
A(P("1 Inputs from the RIPS", "h1"))
A(P(f"Source: {r.source_pdf}; {r.source_pages}. Composition sampled in {int(r.composition_sampling_year)} (wet mass, "
    "domestic and non-domestic reported separately). Glass is not reported for Padang: the cell is blank and treated "
    "as not reported, not as zero. Data-quality flags: " + str(r.data_quality_flags).replace(";", "; ")))
rips = o["rips"]
rows = [["RIPS category", "Domestic %", "Non-domestic %", "Model fraction"]]
inv = {c: f for f, cs in __import__("mswpath.core", fromlist=["MAP"]).MAP.items() for c in cs}
for k, rw in rips.iterrows():
    rows.append([k, "blank" if pd.isna(rw.dom_pct) else f"{rw.dom_pct:.2f}", "blank" if pd.isna(rw.nd_pct) else f"{rw.nd_pct:.2f}", inv[k]])
rows.append(["Sum", f"{nm['dom_sum']:.2f}", f"{nm['nd_sum']:.2f}", ""])
A(table(rows, [4.0, 3.0, 3.0, 4.0]))
A(P("Table 1: RIPS composition of Kota Padang and its mapping to the ten model fractions.", "cap"))
A(P("<b>Tonnage to 2025.</b> The tonnage comes from the RIPS table of the stated year, so it is projected once with the "
    "Padang growth rate:"))
A(eqrow(rf"Q_{{2025}}=Q_t(1+g)^{{2025-t}}={tn['Qt']:.2f}\times(1+{tn['g']:.4f})^{{{tn['expo']}}}={tn['Q2025']:.2f}\ \mathrm{{t/day}}", "pd_q", "1"))
A(P(f"Domestic {tn['Md_t']:.2f} &rarr; {tn['Md']:.2f} t/day and non-domestic {tn['Mn_t']:.2f} &rarr; {tn['Mn']:.2f} t/day "
    f"(same factor {tn['factor']:.6f}). The tonnage does not enter the GHG per tonne; it enters plant cost (economies of "
    "scale) and the scale gates."))

# ---- 2 composition
A(P("2 Composition per functional unit", "h1"))
A(P(f"Each stream is normalised to 1 (domestic sum {nm['dom_sum']:.2f}%, factor {nm['f_dom']:.5f}; non-domestic "
    f"{nm['nd_sum']:.2f}%, factor {nm['f_nd']:.5f}) and the two are weighted by their 2025 masses "
    f"({100*nm['w_dom']:.2f}% / {100*nm['w_nd']:.2f}%):"))
A(eqrow(r"s_j=\frac{M_d\,s^{(d)}_j+M_n\,s^{(n)}_j}{M_d+M_n}\quad\Rightarrow\quad \sum_j s_j=1\ \mathrm{t\ per\ t\ MSW}", "pd_s", "2"))
fr = o["frac"]
rows = [["Fraction", "Domestic (mapped %)", "Non-domestic (mapped %)", "s domestic", "s non-domestic", "s used (t/t)"]]
for k, rw in fr.iterrows():
    rows.append([k, f"{rw.dom_pct_mapped:.2f}", f"{rw.nd_pct_mapped:.2f}", f"{rw.s_dom:.4f}", f"{rw.s_nd:.4f}", f"{rw.s:.4f}"])
A(table(rows, [2.6, 3.0, 3.2, 2.6, 2.8, 2.8]))
A(P("Table 2: Composition per tonne of MSW. Rules L and W are not triggered for Padang (organics reported separately; "
    "dry leaves present).", "cap"))
A(figure(fig_comp(), 14.5, "Figure 1: Domestic, non-domestic and combined composition of Kota Padang."))

# ---- 3 characterisation
A(P("3 Characterisation: dry matter, methane potential, fossil carbon, heating value", "h1"))
A(P("Every property multiplies the same dry mass s<sub>j</sub>(1 &minus; w<sub>j</sub>) (moisture as received, main "
    f"case). Parameters: Table 2 of the methodology; DOC&times;DOCf multiplier k<sub>D</sub> = {PR['doc_k']:.2f}, methane "
    f"fraction F = {PR['F']:.2f}, heating-value factor k<sub>h</sub> = {PR['h_k']:.2f} (legacy fractions; plastic uses its own "
    "value), rubber DOCf = 0 (gap scenario)."))
A(eqrow(r"\mathrm{CH_4}=10^3\sum_j s_j(1-w_j)\,DOC_j\,DOCf_j\,k_D\,F\,\frac{16}{12};\quad E_{fos}=10^3\sum_j s_j(1-w_j)C_j\varphi_j\frac{44}{12};\quad H=\sum_j s_j(1-w_j)h_j-\lambda\sum_j s_jw_j", "pd_char", "3"))
t = o["char"]
rows = [["Fraction", "s", "w", "dry s(1-w)", "DOC", "DOCf", "C", "&phi;", "h MJ/kg", "CH4 kg/t", "CO2 fossil kg/t", "LHV MJ/kg"]]
for k, rw in t.iterrows():
    rows.append([k, f"{rw.s:.4f}", f"{rw.w:.2f}", f"{rw.dry:.4f}", f"{rw.DOC:.2f}", f"{rw.DOCf:.2f}", f"{rw.C:.2f}",
                 f"{rw.phi:.2f}", f"{rw.h:.2f}", f"{rw.CH4_MCF1:.2f}", f"{rw.CO2_fossil:.2f}", f"{rw.LHV_contrib:.3f}"])
rows.append(["Total", "1.0000", f"M = {bk['moisture']:.3f}", f"{bk['dry']:.4f}", "", "", "", "", "", f"{bk['L0']:.2f}",
             f"{bk['fossil']:.2f}", f"{bk['LHV']:.3f}"])
A(table(rows, [1.6, 1.2, 1.4, 1.5, 1.0, 1.0, 0.9, 0.9, 1.3, 1.5, 1.8, 1.5]))
A(P("Table 3: Characterisation per tonne of MSW. The LHV column is each fraction's contribution s(1&minus;w)h &minus; "
    "&lambda;sw (water makes it negative for fractions without heating value).", "cap"))
A(figure(fig_contrib(), 16, "Figure 2: Contribution of each fraction to dry matter, methane potential, fossil CO2 and "
         "heating value."))
A(P(f"Food is {100*t.loc['food','s']:.0f}% of the wet mass but, being wet (w = {t.loc['food','w']:.2f}), only "
    f"{t.loc['food','dry']:.3f} t/t of dry matter; garden waste ({100*t.loc['garden','s']:.0f}% of the mass, w = "
    f"{t.loc['garden','w']:.2f}, DOC {t.loc['garden','DOC']:.2f}) contributes almost as much methane "
    f"({t.loc['garden','CH4_MCF1']:.1f} vs {t.loc['food','CH4_MCF1']:.1f} kg/t). Plastic supplies "
    f"{100*t.loc['plastic','CO2_fossil']/bk['fossil']:.0f}% of the fossil CO<sub>2</sub> and "
    f"{t.loc['plastic','LHV_contrib']:.2f} of the {bk['LHV']:.2f} MJ/kg. The dry-matter HHV implied is "
    f"{bk['HHV_dry']:.1f} MJ/kg, inside the 15.7&ndash;19.2 MJ/kg measured for Indonesian MSW (Prabowo et al. 2019)."))

# ---- 4 baselines
A(P("4 Baselines: open dump and sanitary landfill", "h1"))
sl = res["SL"]["lf"]
A(P(f"<b>Open dump</b> (MCF = {PR['mcf_od']:.1f}, no gas collection, no oxidation): CH<sub>4</sub> = {bk['L0']:.2f} &times; "
    f"{PR['mcf_od']:.1f} = {res['OD']['ch4']:.2f} kg/t; G<sub>OD</sub> = {res['OD']['ch4']:.2f} &times; {ctx['gwp_ch4']:.0f} + "
    f"{PR['anc_od']:.0f} (diesel) = <b>{res['OD']['G']:.1f} kg CO<sub>2</sub>e/t</b>; cost {res['OD']['C']:.1f} USD/t."))
A(P(f"<b>Sanitary landfill</b> (MCF = 1, lifetime gas collection &eta; = {PR['cap']:.2f}, flare, cover oxidation "
    f"{PR['ox']:.2f}): CH<sub>4</sub> generated {sl['ch4']:.2f} kg/t, captured and flared {sl['capt']:.2f}, emitted "
    f"({sl['ch4']:.2f} &minus; {sl['capt']:.2f}) &times; (1 &minus; {PR['ox']:.2f}) = {sl['emit']:.2f} kg/t; "
    f"G<sub>SL</sub> = {sl['emit']:.2f} &times; {ctx['gwp_ch4']:.0f} + {PR['anc_sl']:.0f} = <b>{sl['G']:.1f} kg "
    f"CO<sub>2</sub>e/t</b>. Cost: c<sub>SL</sub>(Q/500)<super>&minus;e</super> = {PR['c_sl']:.0f} &times; "
    f"({o['Q']:.1f}/500)<super>&minus;{PR['e_sl']:.1f}</super> = <b>{sl['unit']:.2f} USD/t</b>."))
A(P(f"The landfill already avoids {res['OD']['G']-sl['G']:.0f} kg CO<sub>2</sub>e/t relative to the open dump, for "
    f"{sl['C']-res['OD']['C']:.1f} USD/t more."))

# ---- 5 pathways
A(P("5 The five recovery options, step by step", "h1"))
A(P(f"Common values: grid {ctx['grid']} EF = {ctx['ef_grid']:.2f} kg CO<sub>2</sub>/kWh; nearest kiln {ctx['kiln']} at "
    f"{ctx['road']:.1f} km by road; CRF = r(1+r)<super>n</super>/((1+r)<super>n</super>&minus;1) = {ctx['crf']:.4f} "
    f"(r = {PR['r']:.2f}, n = {PR['n']:.0f}); CAPEX per tonne = K<sub>ref</sub>(q/0.85/q<sub>ref</sub>)<super>b</super> "
    f"CRF/(365q) with b = {PR['b']:.2f}."))
s1 = res["S1"]
A(P("5.1 S1 Waste-to-energy", "h2"))
A(P(f"Electricity E = H &times; 1000/3.6 &times; &eta;<sub>W</sub> = {bk['LHV']:.3f} &times; 277.8 &times; {PR['eta_wte']:.2f} "
    f"= {s1['E']:.1f} kWh/t."))
def comp_table(k, unit_g="kg CO2e/t", unit_c="USD/t"):
    pr, cs = res[k]["parts"], res[k]["cost"]
    rows = [["GHG component", unit_g, "Cost component", unit_c]]
    pk, ck = list(pr.items()), list(cs.items())
    for j in range(max(len(pk), len(ck))):
        a = pk[j] if j < len(pk) else ("", None); b = ck[j] if j < len(ck) else ("", None)
        rows.append([a[0].replace("_", " "), "" if a[1] is None else f"{a[1]:,.2f}", b[0].replace("_", " "), "" if b[1] is None else f"{b[1]:,.2f}"])
    rows.append(["<b>Total G</b>", f"<b>{res[k]['G']:,.2f}</b>", "<b>Total C</b>", f"<b>{res[k]['C']:,.2f}</b>"])
    return table(rows, [4.6, 3.2, 4.6, 3.2])
A(comp_table("S1"))
A(P(f"Table 4: WtE. Fossil CO<sub>2</sub> from Table 3; N<sub>2</sub>O {PR['n2o_wte']:.2f} kg/t &times; 273; ash "
    f"{PR['ash']:.2f} t/t to landfill (inert); electricity credited at the grid factor and sold at {PR['p_el']:.2f} USD/kWh "
    f"(market case). Gate G1 (LHV &ge; 7 MJ/kg): {'passed' if bk['LHV']>=7 else 'failed'}; PSEL tariff (&ge; 1,000 t/day): "
    f"{'eligible' if o['Q']>=1000 else 'not eligible'}.", "cap"))
L2 = res["S2"]["line"]
A(P("5.2 S2 RDF to cement kiln", "h2"))
rows = [["Fraction", "s (t/t)", "&tau; (x k)", "to RDF r (t/t)", "rejected (t/t)"]]
for j, k in enumerate(FR):
    rows.append([k, f"{o['frac'].s.iloc[j]:.4f}", f"{min(o['M'].TAU[j]*PR['tau_k'],1):.2f}", f"{L2['r'][j]:.4f}", f"{L2['rej'][j]:.4f}"])
A(table(rows, [3.0, 2.6, 2.4, 3.0, 3.0]))
A(P("Table 5: Sorting of each fraction into RDF (legacy transfer coefficients).", "cap"))
A(P(f"RDF line: mass in {L2['m_in']:.4f} t/t with {L2['water']:.4f} t water and {L2['dry']:.4f} t dry; dried to "
    f"&omega; = {PR['omega']:.2f}: m<sub>out</sub> = min({L2['m_in']:.4f}, {L2['dry']:.4f}/(1&minus;{PR['omega']:.2f})) = "
    f"{L2['m_out']:.4f} t/t. Energy in dried RDF E = &Sigma;r(1&minus;w)h &minus; &lambda;(m<sub>out</sub>&minus;dry) = "
    f"{L2['e']:.3f} GJ/t; dryer heat ({PR['q_dry']:.1f} GJ/t water &times; {L2['m_in']-L2['m_out']:.4f} t) leaves "
    f"E<sub>net</sub> = {L2['e_net']:.3f} GJ/t and m<sub>del</sub> = {L2['m_del']:.4f} t RDF/t MSW with NCV = "
    f"{L2['ncv']:.2f} MJ/kg (gate G4 &ge; 12.56: {'passed' if L2['ncv']>=12.56 else 'failed'}). Coal displaced: "
    f"&psi;E<sub>net</sub> = {PR['psi']:.2f} &times; {L2['e_net']:.3f} GJ at {PR['ef_coal']:.1f} kg CO<sub>2</sub>/GJ. The "
    f"rejects ({L2['rej'].sum():.4f} t/t, mostly wet food) are landfilled and generate "
    f"{res['S2']['lf']['ch4']:.2f} kg CH<sub>4</sub>/t."))
A(comp_table("S2"))
A(P("Table 6: RDF. The methane from rejected wet organics is the largest positive term.", "cap"))
A3 = res["S3"]["ad"]; lf3 = res["S3"]["lf"]
A(P("5.3 S3 Anaerobic digestion of food waste", "h2"))
A(P(f"Food captured a = &kappa;s<sub>food</sub> = {PR['kappa']:.2f} &times; {o['frac'].s['food']:.4f} = {A3['a']:.4f} t/t. "
    f"Methane V = a &times; 1000 &times; (1&minus;w<sub>food</sub>) &times; VS/TS &times; y<sub>CH4</sub> = {A3['a']:.4f} "
    f"&times; 1000 &times; {1-o['char'].w['food']:.2f} &times; {PR['vs_ts']:.2f} &times; {PR['y_ch4']:.2f} = {A3['V']:.2f} "
    f"Nm<super>3</super>/t; fugitive {PR['fug_ad']:.2f}; electricity exported {A3['el']:.1f} kWh/t "
    f"(&eta;<sub>CHP</sub> {PR['eta_chp']:.2f}, own use {PR['par_ad']:.2f}). The rest of the waste "
    f"({lf3['t']-A3['a']*PR['dig']:.4f} t) and the digestate ({A3['a']*PR['dig']:.4f} t) go to the landfill "
    f"({lf3['ch4']:.2f} kg CH<sub>4</sub>/t generated). Front-end separation of food from mixed waste is charged as "
    f"&pi; = {PR['pre']:.2f} of the RDF line."))
A(comp_table("S3"))
A(P("Table 7: AD of food waste separated from mixed waste.", "cap"))
s4 = res["S4"]
A(P("5.4 S4 PHB from landfill gas", "h2"))
A(P(f"Captured methane {sl['capt']:.2f} kg/t &divide; R = {PR['r_phb']:.1f} t CH<sub>4</sub>/t PHB gives "
    f"{s4['phb']:.2f} kg PHB/t, i.e. {s4['tpa']:,.0f} t PHB/yr at {o['Q']:.0f} t/day (gate G6 &ge; 500 t/yr: "
    f"{'passed' if s4['tpa']>=500 else 'failed'}). Production cost c<sub>500</sub>(P/500)<super>&minus;e</super> = "
    f"{s4['phb_cost']:.2f} USD/kg against a price of {PR['p_phb']:.1f} USD/kg."))
A(comp_table("S4"))
A(P("Table 8: PHB. Against the landfill it is built on, the only climate gain is the displaced polypropylene.", "cap"))
L5 = res["S5"]["line"]
A(P("5.5 S5 Integrated RDF + AD", "h2"))
A(P(f"The whole tonne passes the RDF line, which also separates the food: {A3['a']:.4f} t to the digester (as in S3), "
    f"the remaining {1-A3['a']:.4f} t sorted as in S2 (RDF {L5['m_del']:.4f} t/t, NCV {L5['ncv']:.2f} MJ/kg); only the "
    f"rejects ({L5['rej'].sum():.4f} t/t) and digestate go to the landfill ({res['S5']['lf']['ch4']:.2f} kg CH<sub>4</sub>/t)."))
A(comp_table("S5"))
A(P("Table 9: RDF + AD. No separate front-end step is charged because the RDF line separates the food.", "cap"))
A(figure(fig_components("parts", "kg CO2e per tonne MSW", "pd_ghg.png", "GHG components per tonne (hatched = credits); diamonds = net"),
         16, "Figure 3: GHG components of each option for Kota Padang."))
A(figure(fig_components("cost", "USD per tonne MSW", "pd_cost.png", "Cost components per tonne (hatched = revenues); diamonds = net"),
         16, "Figure 4: Cost components of each option for Kota Padang."))

# ---- 6 summary & decision
A(P("6 Summary of central results and decision", "h1"))
rows = [["Option", "Gates", "G kg CO2e/t", "&Delta;G vs OD", "&Delta;G vs SL", "C USD/t", "MAC vs SL USD/t", "CED MJ/t", "Land m2/t"]]
for k in ["OD"] + PW:
    if k == "OD":
        rows.append(["Open dump (baseline)", "-", f"{res['OD']['G']:.1f}", "-", "-", f"{res['OD']['C']:.1f}", "-", "-", "-"]); continue
    dG, dGs = res["OD"]["G"] - res[k]["G"], res["SL"]["G"] - res[k]["G"]
    mac = 1e3 * (res[k]["C"] - res["SL"]["C"]) / dGs if dGs > 1e-9 else np.nan
    rows.append([NAMES[k], "pass" if u["feasible_central"][k] else "fail", f"{res[k]['G']:.1f}", f"{dG:.1f}",
                 "-" if k == "SL" else f"{dGs:.1f}", f"{res[k]['C']:.2f}", fmt(mac, ".0f") if k != "SL" else "-",
                 f"{res[k]['CED']:,.0f}", f"{res[k]['LU']:.4f}"])
A(table(rows, [3.6, 1.1, 1.9, 1.7, 1.7, 1.5, 2.1, 1.7, 1.7]))
A(P("Table 10: Central results for Kota Padang (market case, GWP100).", "cap"))
A(P("The best option minimises the social cost SC<sub>k</sub> = C<sub>k</sub> + p G<sub>k</sub>/1000 among the options "
    "that pass their gates. Two options with straight lines cross at p* = 1000(C<sub>b</sub>&minus;C<sub>a</sub>)/"
    "(G<sub>a</sub>&minus;G<sub>b</sub>): for RDF + AD against the landfill p* = "
    f"1000 &times; ({res['S5']['C']:.2f} &minus; {res['SL']['C']:.2f}) / ({res['SL']['G']:.1f} &minus; {res['S5']['G']:.1f}) = "
    f"{1e3*(res['S5']['C']-res['SL']['C'])/(res['SL']['G']-res['S5']['G']):.1f} USD/t CO<sub>2</sub>e."))
A(figure(fig_decision(), 16, "Figure 5: Left: social cost of each option against the carbon value (central values). "
         "Right: probability of being best within each carbon-value bin (Monte Carlo)."))

# ---- 7 uncertainty
A(PageBreak())
A(P("7 Uncertainty analysis", "h1"))
A(P(f"4,000 Monte Carlo draws. Composition: Dirichlet around the RIPS proxy with concentration "
    f"&alpha;<sub>0</sub> = {u['alpha0']:.0f} (heuristic; unflagged data). Parameters: triangular distributions of the "
    "register (58 inputs) and the CED/land-use factors; one common wetness draw moves all moistures; the carbon value is "
    "uniform on 0&ndash;100 USD/t."))
cp = u["comp"]
rows = [["Fraction", "Central", "p5", "median", "p95"]] + [[k, f"{rw.central:.3f}", f"{rw.p5:.3f}", f"{rw.p50:.3f}", f"{rw.p95:.3f}"] for k, rw in cp.iterrows()]
A(table(rows, [3.0, 2.4, 2.4, 2.4, 2.4]))
A(P(f"Table 11: Sampled composition (share of wet mass). Bulk moisture p5&ndash;p95 {u['moist'][0]:.2f}&ndash;"
    f"{u['moist'][2]:.2f}; LHV {u['lhv'][0]:.1f}&ndash;{u['lhv'][2]:.1f} MJ/kg; WtE heating-value gate passed in "
    f"{100*u['p_lhv_gate']:.0f}% of draws.", "cap"))
d = u["dist"]
rows = [["Option", "G p5 / p50 / p95", "&Delta;G vs OD p50 (p5-p95)", "C p5 / p50 / p95", "CED p50 MJ/t", "P(feasible)", "P(Pareto)", "P(best)"]]
for k in PW:
    g = d[(d.option == k) & (d.indicator == "G")].iloc[0]; cc = d[(d.option == k) & (d.indicator == "C")].iloc[0]
    dg = d[(d.option == k) & (d.indicator == "dG_OD")].iloc[0]; ce = d[(d.option == k) & (d.indicator == "CED")].iloc[0]
    pv = lambda ind: d[(d.option == k) & (d.indicator == ind)].p50.iloc[0]
    rows.append([NAMES[k], f"{g.p5:.0f} / {g.p50:.0f} / {g.p95:.0f}", f"{dg.p50:.0f} ({dg.p5:.0f}-{dg.p95:.0f})",
                 f"{cc.p5:.1f} / {cc.p50:.1f} / {cc.p95:.1f}", f"{ce.p50:,.0f}", f"{pv('p_feasible'):.2f}",
                 f"{pv('p_front'):.2f}", f"<b>{pv('p_best'):.3f}</b>"])
A(table(rows, [3.2, 2.7, 2.9, 2.7, 1.8, 1.3, 1.3, 1.2]))
A(P("Table 12: Monte Carlo results for Kota Padang (market case). P(best) is evaluated over the sampled carbon value.", "cap"))
A(figure(fig_mc(), 16, "Figure 6: Distributions of GHG, GHG avoided against the open dump and net cost (boxes: "
         "interquartile range; whiskers: 5th-95th percentiles)."))
sp = u["split"]
rows = [["Indicator", "Option", "composition only", "parameters only", "both"]]
for (ind, k), rw in sp.iterrows():
    rows.append([ind, NAMES[k], f"{rw['composition only']:.1f}", f"{rw['parameters only']:.1f}", f"{rw['both']:.1f}"])
A(table(rows, [2.0, 4.0, 3.0, 3.0, 2.4]))
A(P("Table 13: Width of the 90% interval (p95 &minus; p5) when only one source of uncertainty is sampled. G in kg "
    "CO<sub>2</sub>e/t, C in USD/t.", "cap"))
A(P(f"Composition uncertainty matters most for WtE (through the plastic share: "
    f"{sp.loc[('G','S1'),'composition only']:.0f} kg CO<sub>2</sub>e/t of interval width) and much less for the cost of "
    "any option; parameter uncertainty dominates the landfill-based options, mainly through gas collection."))

# ---- 8 sensitivity
A(P("8 Sensitivity analysis", "h1"))
A(P("<b>8.1 One-at-a-time (tornado).</b> Each input of the register is set to its low and its high value with all others "
    "at their central values. The decisive output is the social-cost gap between RDF + AD and the landfill at 50 USD/t "
    f"(base value {u['tornado_base']['gap_S5_SL']:.2f} USD/t; negative = RDF + AD cheaper in social terms)."))
A(figure(fig_tornado(), 16, "Figure 7: Tornado diagrams for Kota Padang. Left: social-cost gap S5 minus SL at 50 USD/t "
         "(red dashed line: break-even). Right: GHG of the sanitary landfill."))
tt = u["tornado"]; q = tt[tt.output == "gap_S5_SL"].sort_values("swing", ascending=False).head(10)
rows = [["Input", "Meaning", "Range", "Gap at low", "Gap at high", "Crosses zero?"]]
for rw in q.itertuples():
    rows.append([rw.input, rw.meaning, f"{num(rw.low)} to {num(rw.high)}", f"{rw.at_low:.2f}", f"{rw.at_high:.2f}",
                 "yes" if np.sign(rw.at_low) != np.sign(rw.at_high) else "no"])
A(table(rows, [1.8, 6.6, 2.4, 1.8, 1.8, 1.8]))
A(P("Table 14: The ten inputs with the largest swing of the S5 &minus; SL social-cost gap. 'Crosses zero' means that the "
    "input alone, within its range, can reverse the choice between the two options.", "cap"))
A(P("<b>8.2 Variance-based (Sobol).</b> First-order (S1) and total (ST) indices from 1,024 Saltelli base samples (all "
    "register inputs plus the wetness draw varied together; composition fixed at the Padang values)."))
A(figure(fig_sobol(), 16, "Figure 8: Sobol indices for Kota Padang."))
sb = u["sobol"]
rows = [["Output", "Most influential inputs: ST (S1)"]]
for outn, lab in (("SCgap_S5_SL", "Social-cost gap S5 - SL at 50 USD/t"), ("G_SL", "GHG landfill"), ("G_S1", "GHG WtE"),
                  ("G_S5", "GHG RDF + AD"), ("C_S1", "Cost WtE"), ("C_S5", "Cost RDF + AD")):
    qq = sb[sb.output == outn].sort_values("ST", ascending=False).head(5)
    rows.append([lab, "; ".join(f"{x.input} {x.ST:.2f} ({x.S1:.2f})" for x in qq.itertuples())])
A(table(rows, [4.4, 12.6]))
A(P("Table 15: Sobol indices for Kota Padang.", "cap"))
spm = u["spearman"]
rows = [["Option", "GHG: top inputs |rho|", "Cost: top inputs |rho|"]]
for k in PW:
    gi = spm[(k, "G")].nlargest(4); ci = spm[(k, "C")].nlargest(4)
    rows.append([NAMES[k], "; ".join(f"{a} {b:.2f}" for a, b in gi.items()), "; ".join(f"{a} {b:.2f}" for a, b in ci.items())])
A(table(rows, [3.2, 6.9, 6.9]))
A(P("Table 16: Spearman rank correlations in the Monte Carlo sample (composition shares included as s_...).", "cap"))

# ---- 9 scenarios
A(P("9 Scenarios", "h1"))
sc = u["scen"]
rows = [["Scenario", "Q t/d", "Best at 0", "25", "50", "100"] + [f"P({k})" for k in PW]]
for rw in sc.itertuples():
    if isinstance(getattr(rw, "note", None), str):
        rows.append([SCN.get(rw.scenario, rw.scenario), "n.a.", "", "", "", ""] + [""] * 6 ); continue
    pv = [getattr(rw, f"p_{k}") for k in PW]; mx = max(pv)
    rows.append([SCN.get(rw.scenario, rw.scenario), f"{rw.Q:,.0f}", rw.best_0, rw.best_25, rw.best_50, rw.best_100] +
                [(f"<b>{x:.2f}</b>" if x == mx else f"{x:.2f}") for x in pv])
A(table(rows, [3.0, 1.2, 1.3, 1.0, 1.0, 1.0] + [1.25] * 6))
A(P("Table 17: Scenarios for Kota Padang. Managed M1 is n.a. because Padang has no status-index value; M2 uses the "
    "TPA-delivery lower bound (71.8%).", "cap"))
A(P(f"Interpretation for Padang. Landfill with flare and RDF + AD are statistically tied over the carbon range; the "
    f"choice turns at about {pstar:.0f} USD/t CO<sub>2</sub>e at central values. A cement kiln {ctx['road']:.0f} km away "
    f"makes RDF cheap to deliver, and the tonnage ({o['Q']:.0f} t/day) is below the Perpres 109 threshold, so the WtE "
    "tariff does not apply. Separating food at source "
    "makes AD alone the most probable option; with IPCC default moisture, RDF + AD becomes clearly preferred; with "
    "GWP20, WtE becomes the cheapest at 50&ndash;100 USD/t at central values, though RDF + AD remains most probable over "
    "the sampled range. The quantities to measure first are the as-received moisture of Padang's waste and the gas "
    "collection that a new landfill would actually achieve."))
A(P("<b>Caveats.</b> Composition from 2023 used as a 2025 proxy; moisture as received, dry heating values and RDF "
    "transfer coefficients are assumptions; costs are class-5 estimates; the Padang managed share (93.7%) is provisional; "
    "land-use and several CED factors are assumptions.", "small"))

doc = SimpleDocTemplate(str(DOCS / "Worked_Example_Kota_Padang_EN.pdf"), pagesize=A4, leftMargin=2 * cm, rightMargin=2 * cm,
                        topMargin=1.8 * cm, bottomMargin=1.6 * cm, title="Worked example: Kota Padang",
                        author="Muhammad Fachri Ridwan")
doc.build(story, onFirstPage=page_deco(RUN), onLaterPages=page_deco(RUN))
print("wrote", DOCS / "Worked_Example_Kota_Padang_EN.pdf")

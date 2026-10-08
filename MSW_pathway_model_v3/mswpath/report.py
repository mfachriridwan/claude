"""Analysis bundle and downloadable reports (Excel and HTML) for any set of cities, in English or Indonesian."""
import base64, io
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
from .core import PW, NAMES, SCENARIOS, CARBON_VALUES

COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}

T = {
    "en": dict(title="MSW recovery pathways: screening results", central="Central results per tonne of MSW (market case)",
               prob="Probability of being the best feasible option at a fixed carbon value (USD/t CO2e), with Monte Carlo standard error",
               bestpc="Best feasible option at central values, by carbon value (USD/t CO2e)",
               gates="Waste characteristics and feasibility gates", caveat="How to read these results",
               option="Option", city="City", feasible="Passes gates", G="GHG (kg CO2e/t)",
               dG_OD="GHG avoided vs open dump", dG_SL="GHG avoided vs landfill", C="Net cost (USD/t)",
               MAC="Abatement cost vs landfill (USD/t CO2e)", CED="Fossil CED (MJ/t)", LU="Land take (m2/t)",
               scenario="Scenario", warnings="Input warnings",
               caveats=["Screening LCA/TEA (class-5 costs). Results are per tonne of mixed MSW at the facility gate in 2025.",
                        "Composition is the sampling-year distribution used as a 2025 proxy; it is not a 2025 measurement.",
                        "Moisture as received, dry heating values and RDF transfer coefficients are assumptions; the moisture "
                        "assumption can reverse the ranking between landfill and RDF + AD.",
                        "CED and land-use factors are partly analyst assumptions (see data/lcia_factors.csv).",
                        "The best option has the lowest carbon-inclusive cost C + pG/1000 (USD/t) for a chosen carbon value p. "
                        "It is not a full social cost: health and local pollution are not valued.",
                        "A small lead in probability means the choice depends on inputs that are still uncertain (decision "
                        "uncertainty), not on Monte Carlo noise: the standard error is reported and is small.",
                        "The facilities do not exist yet: results are ex-ante screening to guide which options to study, "
                        "tender or pilot, and which data to collect first."]),
    "id": dict(title="Jalur pengolahan sampah kota: hasil penapisan", central="Hasil pusat per ton sampah (kasus pasar)",
               prob="Peluang menjadi opsi layak terbaik pada nilai karbon tetap (USD/t CO2e), dengan galat baku Monte Carlo",
               bestpc="Opsi layak terbaik pada nilai pusat, menurut harga karbon (USD/t CO2e)",
               gates="Karakteristik sampah dan syarat kelayakan", caveat="Cara membaca hasil",
               option="Opsi", city="Kota", feasible="Lolos syarat", G="GRK (kg CO2e/t)",
               dG_OD="GRK terhindar vs open dump", dG_SL="GRK terhindar vs landfill", C="Biaya bersih (USD/t)",
               MAC="Biaya abatemen vs landfill (USD/t CO2e)", CED="CED fosil (MJ/t)", LU="Kebutuhan lahan (m2/t)",
               scenario="Skenario", warnings="Peringatan input",
               caveats=["LCA/TEA tingkat penapisan (biaya kelas 5). Hasil per ton sampah tercampur di gerbang fasilitas, tahun 2025.",
                        "Komposisi adalah data tahun sampling yang dipakai sebagai proksi 2025, bukan pengukuran 2025.",
                        "Kadar air saat diterima, nilai kalor kering, dan koefisien transfer RDF adalah asumsi; asumsi kadar "
                        "air dapat membalik peringkat landfill dan RDF + AD.",
                        "Faktor CED dan lahan sebagian merupakan asumsi analis (lihat data/lcia_factors.csv).",
                        "Opsi terbaik adalah yang biaya dengan valuasi karbonnya, C + pG/1000 (USD/t), paling rendah untuk "
                        "nilai karbon p yang dipilih. Ini bukan biaya sosial penuh: dampak kesehatan dan polusi lokal tidak dinilai.",
                        "Selisih peluang yang kecil berarti pilihan bergantung pada input yang masih tidak pasti "
                        "(ketidakpastian keputusan), bukan derau Monte Carlo; galat baku dilaporkan dan nilainya kecil.",
                        "Fasilitas belum ada: hasil ini penapisan ex-ante untuk menentukan opsi yang layak dikaji, "
                        "dilelang, atau dipilotkan, dan data mana yang perlu dikumpulkan lebih dulu."]),
}
SCN_LABEL = {"en": {"market": "Market", "perpres109": "Perpres 109/2025 tariff", "food_separated_at_source": "Food separated",
                    "GWP20": "GWP20", "moisture_IPCC_default": "IPCC moisture", "residues_to_open_dump": "Residues to open dump",
                    "managed_M1_status_index": "Managed (status index)", "managed_M2_local_first": "Managed (local indicator)",
                    "tonnage_overview_projected": "Overview tonnage projected", "docf_rubber_0.5": "Rubber DOCf 0.5",
                    "AD_feed_optimistic": "AD feed optimistic", "glass_imputed": "Glass imputed",
                    "dirichlet_pseudocount": "Dirichlet pseudocount", "moisture_high_bound": "Moisture high bound",
                    "rdf_ncv_stress": "RDF NCV stress", "phb_large_scale_cost": "PHB large-scale cost (what-if)"},
             "id": {"market": "Pasar", "perpres109": "Tarif Perpres 109/2025", "food_separated_at_source": "Makanan terpilah",
                    "GWP20": "GWP20", "moisture_IPCC_default": "Kadar air IPCC", "residues_to_open_dump": "Residu ke open dump",
                    "managed_M1_status_index": "Terkelola (indeks status)", "managed_M2_local_first": "Terkelola (indikator lokal)",
                    "tonnage_overview_projected": "Tonase overview diproyeksikan", "docf_rubber_0.5": "DOCf karet 0,5",
                    "AD_feed_optimistic": "Umpan AD optimistis", "glass_imputed": "Kaca diimputasi",
                    "dirichlet_pseudocount": "Pseudocount Dirichlet", "moisture_high_bound": "Kadar air batas atas",
                    "rdf_ncv_stress": "Uji tekan NCV RDF", "phb_large_scale_cost": "Biaya PHB skala besar (andaikan)"}}


def analyse(M, raw, N=2000, scenarios=("market", "perpres109", "food_separated_at_source", "GWP20", "moisture_IPCC_default"),
            warnings=None):
    """Run central and Monte Carlo analyses for the cities in `raw` (v3 format). Returns a dict of tables."""
    M.load_cities(raw)
    M.check_single_projection()
    sc = {k: SCENARIOS[k] for k in scenarios}
    det, char, best = M.run_central(sc)
    mcs = []
    for s in scenarios:
        df, _ = M.run_mc(s, N=N, scenarios=sc)
        mcs.append(df)
    mc = pd.concat(mcs, ignore_index=True)
    rows = []
    for (city, s), g in mc.groupby(["city", "scenario"], sort=False):
        g = g.set_index("pathway").reindex(PW)
        for pc in CARBON_VALUES:
            p = g[f"p_best_at_{pc}"].to_numpy(); se = g[f"p_best_at_{pc}_se"].to_numpy(); o = np.argsort(p)
            rows.append(dict(city=city, scenario=s, carbon_value=pc, **dict(zip(PW, p)), most_probable=PW[o[-1]],
                             p_most_probable=p[o[-1]], mc_se=se[o[-1]], lead=p[o[-1]] - p[o[-2]]))
    prob = pd.DataFrame(rows)
    return dict(det=det, char=char, best=best, mc=mc, prob=prob, warnings=warnings or [], N=N, scenarios=list(scenarios))


def central_table(A, lang="en"):
    L = T[lang]
    d = A["det"][A["det"].scenario == "market"]
    o, s = d[d.baseline == "OD"].set_index(["city", "pathway"]), d[d.baseline == "SL"].set_index(["city", "pathway"])
    t = pd.DataFrame({L["feasible"]: o.feasible, L["G"]: o.G.round(0), L["dG_OD"]: o.dG.round(0),
                      L["dG_SL"]: s.dG.round(0), L["C"]: o.C.round(1), L["MAC"]: s.MAC.round(0),
                      L["CED"]: o.CED.round(0), L["LU"]: o.LU.round(3)}).reset_index()
    t["pathway"] = t.pathway.map(NAMES)
    return t.rename(columns={"city": L["city"], "pathway": L["option"]})


def _fig_b64(fig):
    buf = io.BytesIO(); fig.savefig(buf, format="png", dpi=130, bbox_inches="tight"); plt.close(fig)
    return base64.b64encode(buf.getvalue()).decode()


def prob_figure(A, lang="en", pcs=(25, 50, 100)):
    p = A["prob"]; p = p[p.scenario == A["scenarios"][0]]; cities = list(dict.fromkeys(p.city))
    fig, axs = plt.subplots(1, len(pcs), figsize=(3.4 * len(pcs), 0.42 * len(cities) + 1.6), sharey=True, squeeze=False)
    for a, pc in zip(axs[0], pcs):
        q = p[p.carbon_value == pc].set_index("city").reindex(cities)[PW]
        q.iloc[::-1].plot.barh(stacked=True, ax=a, color=[COL[k] for k in PW], width=0.8, legend=False)
        a.set_xlim(0, 1); a.set_title(f"{SCN_LABEL[lang].get(A['scenarios'][0], A['scenarios'][0])}, {pc} USD/t CO2e", fontsize=9)
        a.set_ylabel("")
    axs[0][-1].legend([NAMES[k] for k in PW], loc="center left", bbox_to_anchor=(1, 0.5), frameon=False, fontsize=8)
    fig.tight_layout()
    return fig


def write_excel(A, path, lang="en"):
    L = T[lang]
    with pd.ExcelWriter(path, engine="openpyxl") as w:
        pd.DataFrame({L["caveat"]: L["caveats"] + [f"{L['warnings']}: {x}" for x in A["warnings"]]}).to_excel(w, sheet_name="README", index=False)
        central_table(A, lang).to_excel(w, sheet_name="central", index=False)
        A["prob"].assign(scenario=A["prob"].scenario.map(SCN_LABEL[lang])).to_excel(w, sheet_name="probability_best", index=False)
        A["best"].assign(scenario=A["best"].scenario.map(SCN_LABEL[lang])).to_excel(w, sheet_name="best_by_carbon_value", index=False)
        A["char"].round(3).to_excel(w, sheet_name="characterisation_gates")
        A["mc"].round(4).to_excel(w, sheet_name="montecarlo_all", index=False)
    return path


def write_html(A, path, lang="en"):
    L = T[lang]
    img = _fig_b64(prob_figure(A, lang))
    bp = A["best"][A["best"].scenario == "market"].drop(columns=["scenario"]).round(1)
    ch = A["char"][["Q_2025", "moisture", "LHV", "L0_sl", "rdf_ncv", "road_km", "gate_LHV", "gate_WtE_scale",
                    "gate_PSEL_generated", "gate_RDF", "gate_PHB"]].round(2)
    css = ("body{font-family:Arial,Helvetica,sans-serif;max-width:1100px;margin:24px auto;padding:0 16px;color:#222}"
           "table{border-collapse:collapse;font-size:12px;margin:8px 0 18px}td,th{border:1px solid #ccc;padding:3px 6px}"
           "th{background:#eef3f8}h1{font-size:22px}h2{font-size:16px;margin-top:26px}.warn{background:#fff4e5;padding:8px}")
    warn = "".join(f"<li>{x}</li>" for x in A["warnings"])
    html = f"""<html><head><meta charset="utf-8"><title>{L['title']}</title><style>{css}</style></head><body>
<h1>{L['title']}</h1>
<h2>{L['caveat']}</h2><ul>{''.join(f'<li>{c}</li>' for c in L['caveats'])}</ul>
{f'<div class="warn"><b>{L["warnings"]}</b><ul>{warn}</ul></div>' if warn else ''}
<h2>{L['prob']}</h2><img src="data:image/png;base64,{img}" style="max-width:100%">
{A['prob'].assign(scenario=A['prob'].scenario.map(SCN_LABEL[lang])).round(2).to_html(index=False)}
<h2>{L['bestpc']}</h2>{bp.to_html(index=False)}
<h2>{L['central']}</h2>{central_table(A, lang).to_html(index=False)}
<h2>{L['gates']}</h2>{ch.to_html()}
<p style="font-size:11px;color:#666">Monte Carlo draws per location and scenario: {A['N']}. Model: mswpath (v3.2).</p>
</body></html>"""
    Path(path).write_text(html, encoding="utf-8")
    return path

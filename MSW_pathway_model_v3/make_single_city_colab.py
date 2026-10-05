"""Write MSW_Single_City_Colab.ipynb: the reproducible one-city analysis for researchers and readers of the article.
Run: python make_single_city_colab.py"""
import nbformat
from nbformat.v4 import new_notebook, new_code_cell, new_markdown_cell

REPO, BRANCH = "mfachriridwan/claude", "main"
C = []
md = lambda s: C.append(new_markdown_cell(s.strip()))
code = lambda s: C.append(new_code_cell(s.strip()))

# the typed-input example is written from the database (Kota Padang RIPS), never typed by hand
from mswpath import single as _S
_e = _S.example_padang("data"); _f = _e["fields"]
_v = lambda x: "None" if (x == "" or x is None or (isinstance(x, float) and x != x)) else repr(round(float(x), 4)) if not isinstance(x, str) else repr(x)
TYPED = (
    "MY_CITY = dict(\n"
    f"    city_name={_v(_f['city_name'])}, province={_v(_f['province'])},\n"
    f"    grid_region={_v(_f['grid_region'])},              # Jamali, Sumatera or Mahakam\n"
    f"    tonnage_total_tpd={_v(_f['tonnage_total_tpd'])},            # t/day, wet, domestic + non-domestic\n"
    f"    tonnage_nondomestic_tpd={_v(_f['tonnage_nondomestic_tpd'])},       # part of the total (None = not separated)\n"
    f"    tonnage_year={int(_f['tonnage_year'])}, growth_rate={_v(_f['growth_rate'])},   # growth as a fraction per year (None = 1.73%/yr default)\n"
    f"    composition_year={int(_f['composition_year'])}, organik_lumped='no',\n"
    f"    lat={_v(_f['lat'])}, lon={_v(_f['lon'])},           # planned site; or give kiln_road_km instead\n"
    "    kiln_road_km=None,\n"
    f"    managed_share={_v(_f['managed_share'])}, managed_share_definition={_v(_f['managed_share_definition'])},\n"
    f"    source={_v(_f['source'])},\n"
    "    monte_carlo_draws=4000, random_seed=20251)\n"
    "#                 category: (domestic %, non-domestic %)\n"
    "MY_COMPOSITION = {\n" + "".join(f"    {k!r}: ({_v(a)}, {_v(b)}),\n" for k, (a, b) in _e["composition"].items()) + "}\n")

md(f"""
<a href="https://colab.research.google.com/github/{REPO}/blob/{BRANCH}/MSW_pathway_model_v3/MSW_Single_City_Colab.ipynb" target="_parent"><img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/></a>

# MSW recovery pathways: analysis for ONE city (v3.2)
*Analisis jalur pengolahan sampah untuk SATU kota*

This notebook reproduces the screening model of the article for **one city or regency** whose data you enter yourself,
either in the template (`templates/single_city_template.xlsx` or the three CSV files) or directly in Step 1.
It compares six options per tonne of mixed MSW at the facility gate in 2025:

| Code | Option |
|---|---|
| SL | Sanitary landfill with gas collection and flare |
| S1 | Waste-to-energy (grate incineration) |
| S2 | RDF co-processed in the nearest cement kiln |
| S3 | Anaerobic digestion (AD) of food waste |
| S4 | PHB bioplastic from landfill gas |
| S5 | Integrated RDF + AD |

**Ex-ante decision support.** The facilities need not exist: the results show which options merit a feasibility study
under your conditions, what they must achieve to beat a landfill, and which local data are worth collecting first.

| Step | Output |
|---|---|
| 0 | Setup (about 1 minute) |
| 1 | Your data: template upload, typed input, or the Kota Padang example |
| 2 | Waste characteristics and feasibility gates |
| 3 | Central results: GHG, cost, fossil energy, land |
| 4 | Decision at fixed carbon values, with Monte Carlo uncertainty |
| 5 | Uncertainty and the inputs that drive it |
| 6 | Scenarios and stress tests |
| 7 | Break-even targets against the landfill |
| 8 | Value of information: what to measure first |
| 9 | Download the report (Excel + figures) |
""")

md("## Step 0. Setup / Persiapan")
code(f"""
import os, sys, subprocess, pathlib
REPO_URL = "https://github.com/{REPO}.git"
BRANCH = "{BRANCH}"            # branch that contains the MSW_pathway_model_v3 folder
IN_COLAB = "google.colab" in sys.modules

def _here(p): return pathlib.Path(p, "mswpath").exists()
if not _here("."):
    if _here("MSW_pathway_model_v3"):
        os.chdir("MSW_pathway_model_v3")
    elif IN_COLAB:
        r = subprocess.run(["git", "clone", "--depth", "1", "-b", BRANCH, REPO_URL, "repo"])
        if r.returncode == 0 and _here("repo/MSW_pathway_model_v3"):
            os.chdir("repo/MSW_pathway_model_v3")
        else:   # private repository or no network: upload the release ZIP instead
            from google.colab import files
            print("Clone failed. Upload MSW_v3_results.zip / Gagal clone, unggah file ZIP:")
            up = files.upload(); z = next(iter(up))
            subprocess.run(["unzip", "-q", "-o", z]); os.chdir("MSW_pathway_model_v3")
if IN_COLAB:
    subprocess.run([sys.executable, "-m", "pip", "install", "-q", "-r", "requirements.txt"])
sys.path.insert(0, os.getcwd())
import numpy as np, pandas as pd, matplotlib.pyplot as plt
from IPython.display import display, Image
from mswpath import MSWModel, PW, NAMES, CARBON_VALUES, __version__
from mswpath import single as S
pd.set_option("display.width", 200); pd.set_option("display.max_columns", 30)
OUT = pathlib.Path("outputs/single_city"); OUT.mkdir(parents=True, exist_ok=True)
print("mswpath", __version__, "| working directory:", os.getcwd())
""")

md("""
## Step 1. Your data / Data Anda
Choose **one** input mode / pilih **satu** cara input:

* `"example"`: the Kota Padang RIPS data (reproduces the worked example of the article);
* `"upload"`: fill in `templates/single_city_template.xlsx` (or the three CSV files `single_city_city_data.csv`,
  `single_city_composition.csv`, `single_city_local_parameters.csv`) and upload it;
* `"typed"`: type the values in the next cell.

In the template: yellow cells are inputs, grey cells are the Padang example. **Leave a composition cell blank if the
category is not reported** (blank is not zero). Optional sheet `local_parameters`: your own measured or local values
(e.g. moisture of food waste, landfill cost, RDF price) replace the model defaults.
""")
code("""
INPUT_MODE = "example"          # "example", "upload" or "typed"

if IN_COLAB and INPUT_MODE == "upload":
    from google.colab import files
    files.download("templates/single_city_template.xlsx")
    print("Fill in the template, then upload the .xlsx (or the 2-3 CSV files) / Isi template lalu unggah:")
    up = files.upload()
    names = list(up)
    xlsx = [n for n in names if n.lower().endswith(".xlsx")]
    if xlsx:
        cd, comp, par = S.read_template(xlsx[0])
    else:
        pick = lambda key: next((n for n in names if key in n), None)
        cd, comp, par = S.read_template(city_csv=pick("city_data"), comp_csv=pick("composition"),
                                        par_csv=pick("local_parameters"))
elif INPUT_MODE == "upload":     # outside Colab: put your file next to this notebook and give its name here
    cd, comp, par = S.read_template("templates/single_city_example_kota_padang.xlsx")
elif INPUT_MODE == "example":
    cd, comp, par = S.read_template("templates/single_city_example_kota_padang.xlsx")
""")
md("""
**Typed input** (used only when `INPUT_MODE = "typed"`). Values below are Kota Padang; replace them with yours.
`None` = not reported / not known.
""")
code("""
""" + TYPED + """#                 parameter key: (your central, your low, your high, source)   -- optional, see templates
MY_PARAMETERS = {}   # e.g. {"moisture_food": (0.78, 0.72, 0.84, "own sampling 2025"), "c_sl": (15, 10, 22, "DLH budget")}

if INPUT_MODE == "typed":
    M0 = MSWModel("data")
    cd, comp, par = S.blank_template(M0)
    cd["value"] = cd.field.map({k: ("" if v is None else v) for k, v in MY_CITY.items()}).fillna("")
    comp["domestic_pct"] = comp.category.map({k: v[0] for k, v in MY_COMPOSITION.items()})
    comp["nondomestic_pct"] = comp.category.map({k: v[1] for k, v in MY_COMPOSITION.items()})
    for k, (c_, lo, hi, src) in MY_PARAMETERS.items():
        par.loc[par.key == k, ["your_central", "your_low", "your_high", "your_source"]] = [c_, lo, hi, src]
""")
md("""
### Validation and harmonisation / Validasi
The 2025 tonnage is projected **once**: $Q_{2025} = Q_t\\,(1+g)^{2025-t}$. Each stream's composition is normalised to
100% and weighted by its 2025 tonnage. Errors stop the run; warnings list every assumption applied.
""")
code("""
settings = {r.field: r.value for r in cd.itertuples()}
N = int(pd.to_numeric(settings.get("monte_carlo_draws"), errors="coerce") or 4000) if str(settings.get("monte_carlo_draws")).strip() not in ("", "nan") else 4000
SEED = int(float(settings.get("random_seed"))) if str(settings.get("random_seed")).strip() not in ("", "nan") else 20251
M = MSWModel("data", seed=SEED)                      # fresh model: defaults from data/*.csv
raw, errors, warnings = S.to_input(cd, comp, M)
print("Errors / Galat:", errors or "none")
for w in warnings: print("Warning / Peringatan:", w)
if errors: raise SystemExit("Fix the errors in the template and run this cell again.")
changed = S.apply_local_parameters(M, par)
print("\\nLocal parameter values used / Nilai parameter lokal:"); display(changed if len(changed) else "none (all defaults)")
display(comp.set_index("category")[["label_en", "model_fraction", "domestic_pct", "nondomestic_pct"]])
print(f"Monte Carlo draws: {N:,}; random seed: {SEED}")
""")

md("## Running the analysis / Menjalankan analisis (about 10-30 s)")
code("""
A = S.analyse(M, raw, N=N)
FIGS = S.figures(A, M, OUT)
print(f"{A['city']}: 2025 tonnage {A['Q2025']:,.1f} t/day")
""")

md("""
## Step 2. Waste characteristics and feasibility gates / Karakteristik sampah
Moisture $M=\\sum_j s_j w_j$, lower heating value as received $H=\\sum_j s_j h_j (1-w_j) - \\lambda M$, methane
potential $L_0$ (IPCC DOC on a dry basis $\\times$ DOCf). Gates: WtE needs LHV $\\ge$ 7 MJ/kg and $\\ge$ 150 t/day;
the Perpres 109/2025 tariff needs $\\ge$ 1,000 t/day; RDF needs NCV $\\ge$ 12.56 MJ/kg and a kiln within 300 km; PHB
needs $\\ge$ 500 t/yr.
""")
code("""
keys = ["Q_2025", "moisture", "LHV", "L0_sl", "wte_kwh", "rdf_yield", "rdf_ncv", "kiln", "road_km", "gate_LHV",
        "gate_WtE_scale", "gate_PSEL_generated", "gate_RDF", "gate_PHB"]
display(A["char"].loc[[k for k in keys if k in A["char"].index]])
display(Image(str(FIGS["composition"])))
""")

md("""
## Step 3. Central results per tonne / Hasil nilai tengah per ton
GHG in kg CO$_2$e/t (GWP100, AR6), net levelised cost in USD/t, fossil cumulative energy demand in MJ/t (negative =
saving), land take in m²/t. Abatement cost against the landfill: $MAC = 10^3 (C_k - C_{SL})/(G_{SL} - G_k)$.
""")
code("""
display(A["central"].round(2))
display(Image(str(FIGS["central"])))
""")

md("""
## Step 4. Decision at fixed carbon values / Keputusan pada nilai karbon tetap
The best option has the lowest **carbon-inclusive cost** $CIC_k = C_k + p\\,G_k/1000$ (USD/t) among the options that
pass their gates. The carbon value $p$ is a policy choice, so it is fixed (2 USD/t = Indonesian carbon tax, UU 7/2021).
$P(\\text{best})$ is the share of Monte Carlo draws in which an option wins, with its standard error
$\\sqrt{P(1-P)/N}$. A small lead means the choice depends on uncertain inputs (decision uncertainty), not on noise.
""")
code("""
pf = A["p_fixed"].assign(value=lambda d: d.p_best.round(3).astype(str) + " ± " + (1.96 * d.se).round(3).astype(str))
display(pf.pivot(index="option", columns="carbon_value", values="value").reindex(PW))
print("Best option at central values, by carbon value:"); display(A["best_central"])
display(Image(str(FIGS["decision"])))
""")

md("## Step 5. Uncertainty and its drivers / Ketidakpastian dan pemicunya")
code("""
display(A["mc"].round(1))
display(Image(str(FIGS["uncertainty"])))
print("Inputs most correlated with each output (|Spearman rho|):")
display(A["spearman"].round(2))
""")

md("""
## Step 6. Scenarios and stress tests / Skenario dan uji tekan
Each scenario changes one assumption with the same random numbers. Stress tests push an assumption to its bound:
`moisture_high_bound` (all moistures at their upper value), `rdf_ncv_stress` (RDF energy × 0.78). `phb_large_scale_cost`
is an aspirational what-if, not a planning value.
""")
code("""
display(A["scenarios"])
""")

md("""
## Step 7. Break-even targets against the landfill / Target impas
With all other inputs central, the value of one input at which the option's carbon-inclusive cost equals the
landfill's. *never*: no value in the tested range is enough; *always*: the option is already cheaper over the whole
range. These are targets that a tender, a pilot or an offtake contract would have to secure.
""")
code("""
be = A["break_even"]
display(be.pivot_table(index=["option", "input", "better_if", "central_value"], columns="carbon_value",
                       values="break_even", aggfunc="first"))
""")

md("""
## Step 8. Value of information: what to measure first / Data apa yang perlu diukur dulu
$EVPI = E[\\max_k NB_k] - \\max_k E[NB_k]$ with $NB = -CIC$: the most it is worth paying (per tonne) to remove all
uncertainty before choosing. EVPPI is the same for one group of inputs that one measurement campaign would resolve
(regression estimator; the value for random noise is subtracted). Multiplied by the 2025 tonnage it is in USD/year:
if a measurement costs less than its EVPPI for one year, it is worth doing before committing to a technology.
""")
code("""
vg = A["voi_groups"]
display(vg.pivot_table(index="group", columns="carbon_value", values="evppi_net").sort_values(100, ascending=False).round(3))
print("EVPI (USD/t and USD/yr):"); display(vg.groupby("carbon_value")[["evpi", "evpi_usd_per_year"]].first().round(2))
display(Image(str(FIGS["voi"])))
""")

md("## Step 9. Download the report / Unduh laporan")
code("""
safe = "".join(ch if ch.isalnum() else "_" for ch in A["city"]).strip("_")
rep = S.write_report(A, OUT / f"report_{safe}.xlsx", inputs=raw, warnings=warnings, overrides=changed)
import shutil; z = shutil.make_archive(str(OUT / f"results_{safe}"), "zip", OUT)
print("Written:", rep, "and", z)
if IN_COLAB:
    from google.colab import files; files.download(str(rep)); files.download(z)
""")

md("""
## Caveats and citation / Catatan dan sitasi
* Screening (ex-ante) LCA/TEA with class-5 costs: use it to decide which options to study further, not as a design.
* The composition is the sampling-year composition used as a 2025 proxy; categories left blank are not reported and
  get zero share (a stated assumption).
* Default parameters, their sources and evidence status are in `data/assumption_register.csv`,
  `data/scenario_parameters.csv` and `data/secondary_data_verification.csv`. Values you enter in `local_parameters`
  replace them and are listed in the report.
* The carbon-inclusive cost values only greenhouse gases; health and local pollution are not valued.
* With the example data and the default seed, this notebook reproduces the Kota Padang results of the article.

Cite: Ridwan, M.F., Halog, A. (2026). *mswpath: provenance-aware screening of MSW recovery pathways for Indonesian
cities*, v3.2 (see `CITATION.cff`). Methodology: `docs/MSW_Methodology_v3_EN.pdf`; worked example:
`docs/Worked_Example_Kota_Padang_EN.pdf`.
""")

nb = new_notebook(cells=C, metadata={"kernelspec": {"name": "python3", "display_name": "Python 3", "language": "python"},
                                     "colab": {"provenance": []}})
nbformat.write(nb, "MSW_Single_City_Colab.ipynb")
print(len(C), "cells written")

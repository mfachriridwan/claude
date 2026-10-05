"""Write MSW_Decision_Tool_Colab.ipynb (the user-facing notebook). Run: python make_colab_notebook.py"""
import nbformat
from nbformat.v4 import new_notebook, new_code_cell, new_markdown_cell

REPO, BRANCH = "mfachriridwan/claude", "main"
C = []
md = lambda s: C.append(new_markdown_cell(s.strip()))
code = lambda s: C.append(new_code_cell(s.strip()))

md(f"""
<a href="https://colab.research.google.com/github/{REPO}/blob/{BRANCH}/MSW_pathway_model_v3/MSW_Decision_Tool_Colab.ipynb" target="_parent"><img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/></a>

# MSW Recovery-Pathway Decision Tool (v3.1)
**Under what Indonesian city and waste-system conditions does each recovery pathway become preferable?**
*Pada kondisi kota dan sistem persampahan seperti apa setiap jalur pengolahan menjadi pilihan terbaik?*

This notebook screens six options per tonne of mixed municipal solid waste at the facility gate in 2025:
sanitary landfill with flare (SL), waste-to-energy (S1), RDF to cement kiln (S2), anaerobic digestion of food waste (S3),
PHB from landfill gas (S4) and integrated RDF + AD (S5). Indicators: climate change (GWP100), net cost, fossil
cumulative energy demand and land take. Every number comes from the CSV files in `data/`, each with its source and status.

| Step | What you get |
|---|---|
| 0 | Setup (about 1 minute in Colab) |
| 1 | Results for the 21 RIPS cities/regencies |
| 2 | Results for **your own cities** from an uploaded template |
| 3 | An interactive form for one city |
| 4 | **Decision rules**: the conditions under which each option wins (scenario discovery) |

*Screening-level study: read the caveats in Step 5 before using results for decisions.*
""")

md("## Step 0. Setup / Persiapan")
code(f"""
import os, sys, subprocess, pathlib
REPO_URL = "https://github.com/{REPO}.git"
BRANCH = "{BRANCH}"          # branch that contains the MSW_pathway_model_v3 folder
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
    from google.colab import output; output.enable_custom_widget_manager()
sys.path.insert(0, os.getcwd())
import numpy as np, pandas as pd, matplotlib.pyplot as plt
from mswpath import MSWModel, read_input, PW, NAMES, __version__
from mswpath.inputs import from_template, validate_template
from mswpath.report import analyse, central_table, prob_figure, write_excel, write_html
print("mswpath", __version__, "| working directory:", os.getcwd())
""")

md("Choose the language of tables and reports / Pilih bahasa tabel dan laporan (`'en'` or `'id'`).")
code('LANG = "id"\nM = MSWModel("data")\npathlib.Path("outputs/user_reports").mkdir(parents=True, exist_ok=True)')

md("""
## Step 1. The 21 RIPS cities/regencies / 21 kota-kabupaten RIPS
Uses `data/input_kota_2025_updated.csv` (2025 baseline, provenance and quality flags in each row).
1,000 Monte Carlo draws per location keeps this step to about a minute; the paper uses 4,000.
""")
code("""
raw21 = read_input("data/input_kota_2025_updated.csv")
A21 = analyse(M, raw21, N=1000, scenarios=("market", "perpres109", "food_separated_at_source"))
prob_figure(A21, LANG); plt.show()
display(A21["best"][A21["best"].scenario == "market"].drop(columns=["scenario"]).round(1))
write_excel(A21, "outputs/user_reports/rips21_report.xlsx", LANG); write_html(A21, "outputs/user_reports/rips21_report.html", LANG)
print("Reports: outputs/user_reports/rips21_report.xlsx and .html")
""")

md("""
## Step 2. Your own cities / Kota Anda sendiri
1. Download the template (`templates/city_input_template.xlsx`, sheet *guide_panduan* explains every column).
2. Fill one row per city: tonnage and its year, composition in wet-mass % (leave blank what is not reported),
   grid region, and either coordinates or the road distance to a cement kiln.
3. Upload it below as CSV or XLSX. The validator stops on errors and lists every assumption it applies as a warning.

Outside Colab the shipped example (Kab. Pati and Kota Magelang) is used.
""")
code("""
if IN_COLAB:
    from google.colab import files
    files.download("templates/city_input_template.xlsx")
    print("Fill the template, then upload it / Isi template lalu unggah:")
    up = files.upload(); fname = next(iter(up))
else:
    fname = "templates/city_input_template.csv"
user = pd.read_excel(fname, sheet_name=0) if fname.endswith("xlsx") else pd.read_csv(fname)
errors, warnings = validate_template(user, M)
print("Errors:", errors or "none"); print("Warnings:", warnings or "none")
if not errors:
    raw_user, warn = from_template(user, M)
    AU = analyse(M, raw_user, N=1000, warnings=warn,
                 scenarios=("market", "perpres109", "food_separated_at_source", "moisture_IPCC_default"))
    prob_figure(AU, LANG); plt.show()
    display(AU["best"].round(1)); display(central_table(AU, LANG))
    p1 = write_excel(AU, "outputs/user_reports/my_cities_report.xlsx", LANG)
    p2 = write_html(AU, "outputs/user_reports/my_cities_report.html", LANG)
    if IN_COLAB: files.download(str(p1)); files.download(str(p2))
""")

md("""
## Step 3. Interactive form for one city / Formulir interaktif satu kota
The form starts with Kab. Pati's numbers from the example template. Change them and press *Run* / *Jalankan*.
""")
code("""
from mswpath.ui import city_form
example = pd.read_csv("templates/city_input_template.csv").iloc[0].to_dict()
from mswpath.core import km
# road distance to the nearest kiln from the example's coordinates (great-circle x road factor of the register)
example["kiln_road_km"] = round(min(km((example["lat"], example["lon"]), v) for v in M.KILN.values()) * M.REG.central["tort"], 1)
city_form(M, LANG, example=example);
""")

md("""
## Step 4. Decision rules: when does each option win? / Aturan keputusan
The model is run over thousands of combinations of conditions a planner can observe or choose: carbon value,
tonnage, distance to a cement kiln, grid, WtE tariff, source separation of food, composition, moisture, landfill gas
collection and landfill cost (all other parameters sampled from the register). A shallow decision tree and PRIM boxes
summarise which conditions favour which option. Composition spans the 21 RIPS compositions; outside that range the
rules are extrapolations.
""")
code("""
from mswpath.discovery import sample_conditions, cart_rules, prim_box, plot_condition_maps
M.load_cities(raw21)
X, best, _, meta = sample_conditions(M, N=20000, seed=7)
print("Share of draws in which each option is best:"); display(best.value_counts(normalize=True).round(3))
CT = cart_rules(X, best, max_depth=4)
print(f"Tree accuracy on held-out draws: {CT['acc_test']:.2f} (majority-class baseline {CT['baseline_acc']:.2f})")
leaves = CT["leaves"].sort_values("share_of_draws", ascending=False)
display(leaves[["rule", "predicted", "purity", "share_of_draws"]].round(2))
plot_condition_maps(X, best, close=False); plt.show()
""")
code("""
# Where does a city sit? Change the values to describe your city and read the tree's prediction.
my_city = dict(carbon_value=50, Q_tpd=500, kiln_km=120, grid_ef=0.87, tariff=0, food_separated=0,
               food_share=0.45, plastic_share=0.18, paper_garden_share=0.20, moisture=0.50, LHV=7.0,
               gas_capture=0.5, landfill_cost=20)
for pc in (0, 25, 50, 100):
    q = pd.DataFrame([{**my_city, "carbon_value": pc}])[X.columns]
    print(f"carbon value {pc:>3} USD/t -> {NAMES[CT['tree'].predict(q)[0]]}")
""")

md("""
## Step 5. Caveats and citation / Catatan dan sitasi
- Screening LCA/TEA with class-5 costs; per tonne of mixed MSW at the facility gate in 2025.
- Composition is a sampling-year proxy for 2025; it is not a measured 2025 composition.
- Moisture as received, dry heating values and RDF transfer coefficients are assumptions. With IPCC default moisture,
  RDF + AD becomes the most probable option in most locations, so measured as-received moisture matters.
- The managed-waste share has different definitions across sources; results on managed tonnage are comparable only
  within one definition.
- CED and land-use factors are partly analyst assumptions (`data/lcia_factors.csv`).
- Probability leads below 0.05 are ties. Decision rules from Step 4 are approximations of the model (see accuracy).

Cite: Ridwan, M.F., Halog, A. (2026). *mswpath: provenance-aware screening of MSW recovery pathways for Indonesian
cities*, v3.1 (see `CITATION.cff`). Methodology: `docs/MSW_Methodology_v3_EN.pdf`.
""")

nb = new_notebook(cells=C, metadata={"kernelspec": {"name": "python3", "display_name": "Python 3", "language": "python"},
                                     "colab": {"provenance": []}})
nbformat.write(nb, "MSW_Decision_Tool_Colab.ipynb")
print(len(C), "cells written")

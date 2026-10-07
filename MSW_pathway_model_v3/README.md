# MSW Pathway Model v3.2 (2025 baseline)

Screening LCA + TEA of six municipal solid waste management options for 21 Indonesian cities/regencies, updated with the October 2026 research package. The update adds a 2025 tonnage baseline, a Level I/II/III taxonomy and material parameters with auditable provenance.

**Purpose (v3.2): ex-ante decision support.** None of the recovery facilities exists yet in the 21 locations. The model tells local governments, planners, offtakers and funders, before a feasibility study, under which conditions each option becomes preferable, which performance it must reach to beat a sanitary landfill (break-even targets), and which data are worth collecting first (value of information). Options are ranked by the carbon-inclusive cost C + pG/1000 at fixed carbon values (0, 2, 25, 50, 100 USD/t CO2e). Secondary data were re-verified in October 2026 (`data/secondary_data_verification.csv`).

## Run order
```bash
pip install numpy pandas matplotlib openpyxl      # reportlab, pillow and nbclient only for the PDFs/notebook
python build_database_v3.py     # sources/ -> data/*.csv
python msw_pathway_model_v3.py  # data/ -> outputs/ (about 15 s)
python run_discovery.py         # decision rules (CART/PRIM), condition maps, Sobol (about 1 min)
python run_voi.py               # value of information (EVPI/EVPPI) per location
python run_projection_2045.py   # 2045 vs 2025 (grid to net zero in 2060, real cost escalation)
python validate_v3.py           # 88 checks -> outputs/validation_report.md
python make_colab_notebook.py   # writes MSW_Decision_Tool_Colab.ipynb (several cities)
python make_single_city_template.py && python make_single_city_colab.py   # one-city template + notebook
python docs/make_methodology_pdf.py && python docs/make_manuscript_pdf.py && python docs/make_padang_example_pdf.py && python docs/make_projection_2045_pdfs.py && python docs/make_math_model_pdf.py && python docs/make_graphical_abstract.py
```
**One city (recommended for readers of the article):** open `MSW_Single_City_Colab.ipynb` in Google Colab. Fill in `templates/single_city_template.xlsx` (or the three CSV files `templates/single_city_city_data.csv`, `single_city_composition.csv`, `single_city_local_parameters.csv`) for your city, or type the values in the notebook, and run all cells. Step 9 of the notebook projects the city to 2045 (grid to net zero in 2060, real cost escalation). A filled example for Kota Padang is in `templates/single_city_example_kota_padang.xlsx`; it reproduces the worked example of the article.

**Several cities:** open `MSW_Decision_Tool_Colab.ipynb` in Google Colab. It installs itself, reproduces the 21 locations, accepts your own cities through `templates/city_input_template.xlsx`, shows the decision rules and writes Excel/HTML reports (English or Indonesian). If the repository is private, upload `MSW_v3_results.zip` when the notebook asks.

## Contents
| Path | What |
|---|---|
| `data/input_kota_2025_updated.csv` | Clean 21-location input with the 2025 baseline, provenance and quality flags |
| `data/table2_model_fraction_parameters.csv`, `data/fraction_parameters_L1_L2_L3.csv`, `data/taxonomy_crosswalk.csv` | Material parameters (Level I/II/III) and taxonomy crosswalk; blank = unknown |
| `data/table5_qmanaged_2025.csv` | Managed waste: status-index and local series kept apart |
| `data/assumption_register.csv`, `data/model_constants.csv` | Every number the model uses |
| `mswpath/` | Python package: model, template/validator, scenario discovery, reports, interactive form |
| `msw_pathway_model_v3.py`, `MSW_pathway_model_v3.ipynb` | Full analysis of the 21 locations (script and executed notebook) |
| `run_discovery.py` | Scenario discovery and Sobol indices |
| `MSW_Single_City_Colab.ipynb`, `mswpath/single.py` | One-city analysis from a manually filled template (XLSX/CSV) or typed input |
| `templates/single_city_*` | One-city template (XLSX with guide, and CSV), plus the Kota Padang example |
| `MSW_Decision_Tool_Colab.ipynb`, `templates/city_input_template.*` | Tool for several cities at once |
| `data/lcia_factors.csv` | CED and land-use factors with status |
| `data/scenario_parameters.csv` | AD-feed realism parameters, glass imputation, pseudocount and stress-test values |
| `data/secondary_data_verification.csv` | Verification of 23 secondary values (result, evidence, link, method) |
| `mswpath/thresholds.py`, `mswpath/voi.py`, `run_voi.py` | Break-even targets and value of information |
| `docs/Projection_2045_EN.pdf` | 2045 projection against 2025 (`run_projection_2045.py`, `mswpath/projection.py`, `data/projection_2045_parameters.csv`) |
| `docs/Worked_Example_Kota_Padang_EN.pdf` | Step-by-step calculation for one city with uncertainty and sensitivity |
| `validate_v3.py`, `outputs/validation_report.md` | Validation suite and its report |
| `docs/MSW_Methodology_v3_EN.pdf` | Methodology v3.2 |
| `docs/Graphical_Abstract_EN.pdf`, `docs/Research_Flow_Diagram_EN.pdf` | Graphical abstract and research flow diagram (vector PDF + 300 dpi PNG; `docs/make_graphical_abstract.py`) |
| `docs/MSW_Mathematical_Model_EN.pdf` | Every equation of the model, with symbols, data keys and values |
| `docs/MSW_Manuscript_v3_EN.pdf` | Draft journal article (authors still need to confirm the author list and declarations) |
| `CHANGELOG_AND_SOURCE_DECISIONS.md` | Changes, source decisions, data gaps, remaining assumptions |
| `sources/` | The update package files used (SHA-256 checked against its manifest) |

Status vocabulary used everywhere: local measurement, literature default, parent-category proxy, foreign benchmark, assumption/prior, scenario, gap.

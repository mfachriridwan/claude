# MSW Pathway Model v3.1 (2025 baseline)

Screening LCA + TEA of six municipal solid waste management options for 21 Indonesian cities/regencies, updated with the October 2026 research package. The update adds a 2025 tonnage baseline, a Level I/II/III taxonomy and material parameters with auditable provenance.

## Run order
```bash
pip install numpy pandas matplotlib openpyxl      # reportlab, pillow and nbclient only for the PDFs/notebook
python build_database_v3.py     # sources/ -> data/*.csv
python msw_pathway_model_v3.py  # data/ -> outputs/ (about 15 s)
python run_discovery.py         # decision rules (CART/PRIM), condition maps, Sobol (about 1 min)
python validate_v3.py           # 60 checks -> outputs/validation_report.md
python make_colab_notebook.py   # writes MSW_Decision_Tool_Colab.ipynb
python docs/make_methodology_pdf.py && python docs/make_manuscript_pdf.py
```
**For researchers and planners:** open `MSW_Decision_Tool_Colab.ipynb` in Google Colab. It installs itself, reproduces the 21 locations, accepts your own cities through `templates/city_input_template.xlsx`, shows the decision rules and writes Excel/HTML reports (English or Indonesian). If the repository is private, upload `MSW_v3_results.zip` when the notebook asks.

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
| `MSW_Decision_Tool_Colab.ipynb`, `templates/` | User tool for new cities (executed once locally) |
| `data/lcia_factors.csv` | CED and land-use factors with status |
| `validate_v3.py`, `outputs/validation_report.md` | Validation suite and its report |
| `docs/MSW_Methodology_v3_EN.pdf` | Methodology v3 |
| `docs/MSW_Manuscript_v3_EN.pdf` | Draft journal article (authors still need to confirm the author list and declarations) |
| `CHANGELOG_AND_SOURCE_DECISIONS.md` | Changes, source decisions, data gaps, remaining assumptions |
| `sources/` | The update package files used (SHA-256 checked against its manifest) |

Status vocabulary used everywhere: local measurement, literature default, parent-category proxy, foreign benchmark, assumption/prior, scenario, gap.

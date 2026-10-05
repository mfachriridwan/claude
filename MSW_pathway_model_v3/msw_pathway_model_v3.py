# %% [markdown]
# # MSW recovery-pathway screening model, version 3.1
# **Screening LCA + TEA of six management options for 21 Indonesian cities/regencies, baseline year 2025**
#
# Functional unit: management of **1 tonne of mixed MSW as received at the facility gate** in 2025.
# Options: SL sanitary landfill + flare, S1 WtE, S2 RDF to cement kiln, S3 AD of food waste,
# S4 PHB from landfill gas, S5 integrated RDF + AD. Baselines: open dump (OD) and sanitary landfill (SL).
# Indicators: climate change (GWP100), net levelised cost, fossil cumulative energy demand (CED), land take.
#
# The model lives in the `mswpath` package; every number is read from `data/*.csv`
# (built by `build_database_v3.py`). This script runs the full analysis for the 21 RIPS locations and writes
# `outputs/`. Scenario discovery (which conditions favour which option) is in `run_discovery.py`.

# %%
import numpy as np, pandas as pd
import matplotlib
import matplotlib.pyplot as plt
from pathlib import Path
from mswpath import MSWModel, read_input, PW, NAMES, SCENARIOS

try:
    ROOT = Path(__file__).resolve().parent
except NameError:                                  # notebook / Colab
    ROOT = Path.cwd()
DATA, OUT = ROOT / "data", ROOT / "outputs"
OUT.mkdir(exist_ok=True)

M = MSWModel(DATA)
raw = read_input(DATA / "input_kota_2025_updated.csv")
H = M.load_cities(raw)
M.check_single_projection()                        # stops if a tonnage would be projected twice
print(f"{len(H)} locations loaded")

# %% [markdown]
# ## Central results (all scenarios)

# %%
DET, CHAR, BEST = M.run_central()
pd.set_option("display.width", 250); pd.set_option("display.max_columns", 40)
print(CHAR.round(2).to_string())
print("\nBest feasible pathway by carbon value (USD per tonne CO2e), market scenario")
print(BEST[BEST.scenario == "market"].set_index("city").drop(columns=["scenario", "Q"]).to_string())

# %% [markdown]
# ## Monte Carlo: probability of being best, per scenario

# %%
MC = {}
for name in SCENARIOS:
    MC[name], smp = M.run_mc(name, keep=(name == "market"))
    if name == "market": SAMPLES = smp
MCALL = pd.concat(MC.values(), ignore_index=True)
winners = lambda df: M.winners(df, H.city)
for name, df in MC.items():
    print("\nProbability of being best | scenario:", name); print(winners(df).round(2).to_string())

# %% [markdown]
# ## Uncertainty sources kept apart, and Spearman sensitivity

# %%
USPLIT = M.uncertainty_split()
print("\nMedian 90% interval width over locations\n", USPLIT.round(1).to_string())
SENS = M.spearman(SAMPLES)

# %% [markdown]
# ## CED and land take (central values, market case)

# %%
d = DET[(DET.scenario == "market") & (DET.baseline == "OD")]
LCI = d.pivot(index="city", columns="pathway", values="CED")[PW].join(
    d.pivot(index="city", columns="pathway", values="LU")[PW], lsuffix="_CED_MJ", rsuffix="_LU_m2")
print(d.groupby("pathway")[["CED", "LU"]].median().reindex(PW).round(3).to_string())

# %% [markdown]
# ## Checks against measured values and Level II/III parameter coverage

# %%
c, P, s, Q, kw = M.setup(0, 1, True); X = M.model(s, P, c, Q)["X"]
assert abs(X["ad_feed"][0] + X["rdf_in_S5"][0] + X["landfilled_S5"][0] - 1) < 1e-9, "S5 mass balance broken"
assert X["rdf_t_S5"][0] <= X["rdf_in_S5"][0] - X["rdf_water_S5"][0] + 1e-12, "more RDF delivered than produced"
W, CARB, iF, LAM = M.W, M.CARB, M.iF, M.LAM
c_food = 1e3 * (1 - W[iF]) * CARB[iF]
c_gas = 1e3 * (1 - W[iF]) * M.REG.central["vs_ts"] * M.REG.central["y_ch4"] * M.RHO_CH4 * 12 / 16 / 0.6
assert c_gas < c_food, "AD yield exceeds the carbon available in food waste"
CHAR["HHV_dry"] = (CHAR.LHV + LAM * CHAR.moisture) / (1 - CHAR.moisture) + LAM * 9 * 0.06
span = lambda v, f: f"{CHAR[v].min():{f}} to {CHAR[v].max():{f}} (median {CHAR[v].median():{f}})"
BENCH = pd.DataFrame({
    "model, 21 locations": [span("moisture", ".2f"), span("LHV", ".1f"), span("HHV_dry", ".1f"), span("L0_sl", ".0f"),
                            span("rdf_yield", ".2f"), span("rdf_ncv", ".1f"), span("wte_kwh", ".0f"), f"{c_gas / c_food:.2f}"],
    "reference": ["0.53 to 0.56 at transfer points (Prabowo et al. 2019); 0.64 to 0.66 at a landfill (Fiki et al. 2022)",
                  "about 5 to 7 (HHV 6.9 to 9.0 measured, Prabowo et al. 2019; 6.5 assumed, Azis et al. 2021)",
                  "15.7 to 19.2, from HHV 6.9 to 9.0 as received at 53 to 56% moisture (Prabowo et al. 2019)",
                  "no Indonesian measurement; reported for transparency",
                  "0.20 to 0.42 (Indonesian RDF plants, GIZ 2023)",
                  "12.6 to 13.8 for biodried whole waste (GIZ 2023); sorted and dried SRF is higher",
                  "632 kWh/t at 35% gross conversion (Azis et al. 2021); model uses 18% net",
                  "must be below 1"]},
    index=["bulk moisture (-)", "LHV as received (MJ/kg)", "HHV of dry matter (MJ/kg)", "L0 at MCF = 1 (kg CH4/t)",
           "RDF yield (t/t MSW)", "RDF heating value (MJ/kg)", "WtE net electricity (kWh/t)",
           "carbon in biogas / carbon in food"])
print(BENCH.to_string())

TERM = pd.read_csv(DATA / "composition_terminal_partition_2025.csv", dtype={"code": str})
FP = pd.read_csv(DATA / "fraction_parameters_L1_L2_L3.csv", dtype={"code": str})
TERM = TERM.merge(FP[["level", "code", "moisture_wet_fraction", "DOC_dry_fraction", "DOCf", "carbon_dry_fraction"]],
                  on=["level", "code"], how="left")
PCOLS = ("moisture_wet_fraction", "DOC_dry_fraction", "DOCf", "carbon_dry_fraction")
COVER = (TERM.assign(**{f"has_{p}": TERM[p].notna() * TERM.percent_wet_mass for p in PCOLS})
         .groupby(["city_name", "stream"])[[f"has_{p}" for p in PCOLS]].sum().round(1))

# %% [markdown]
# ## Save tables and figures

# %%
for nm, df, idx in (("01_harmonised_inputs", H.round(4), True), ("02_characterisation_and_gates", CHAR.round(3), True),
                    ("03_deterministic_results", DET.round(3), False), ("04_best_pathway_by_carbon_value", BEST, False),
                    ("05_montecarlo_results", MCALL.round(4), False), ("06_sensitivity_spearman", SENS.round(3), True),
                    ("07_assumption_register_used", M.REG, True), ("08_fraction_properties_used", M.PROP, True),
                    ("09_validation_benchmarks", BENCH, True), ("10_uncertainty_split", USPLIT.round(2), True),
                    ("11_parameter_coverage_L2L3", COVER, True), ("13_ced_landuse_central", LCI.round(4), True),
                    ("14_lcia_factors_used", M.LCIA, True)):
    df.to_csv(OUT / f"{nm}.csv", index=idx)

COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}
fig, ax = plt.subplots(1, 3, figsize=(15, 6.5), sharey=True)
for a, key, ttl in zip(ax, ("market", "perpres109", "food_separated_at_source"),
                       ("Market case", "Perpres 109/2025 tariff for WtE\nat 1,000 t/day or more",
                        "What if food waste arrives\nalready separated")):
    winners(MC[key])[PW].iloc[::-1].plot.barh(stacked=True, ax=a, color=[COL[k] for k in PW], width=0.8, legend=False)
    a.set_title(ttl, fontsize=10); a.set_xlabel("Probability of being the best feasible pathway"); a.set_xlim(0, 1); a.set_ylabel("")
ax[2].legend([NAMES[k] for k in PW], loc="center left", bbox_to_anchor=(1.0, 0.5), frameon=False)
fig.tight_layout(); fig.savefig(OUT / "fig_probability_best.png", dpi=170); plt.close(fig)

fig, ax = plt.subplots(1, 2, figsize=(13, 5)); dmc = MC["market"]
for a, bg, ttl in zip(ax, ("OD", "SL"), ("Baseline: open dump", "Baseline: sanitary landfill")):
    for k in PW[(bg == "SL"):]:
        q = dmc[dmc.pathway == k]
        a.scatter(q[f"dC_{bg}_p50"], q[f"dG_{bg}_p50"], s=15 + 70 * q.p_feasible, c=COL[k], alpha=0.75, label=NAMES[k],
                  edgecolor="k", linewidth=0.3)
    a.axhline(0, c="k", lw=0.6); a.axvline(0, c="k", lw=0.6); a.set_title(ttl + " (one dot per location)", fontsize=10)
    a.set_xlabel("Extra cost over baseline, USD per tonne MSW (median)"); a.set_ylabel("GHG avoided, kg CO2e per tonne MSW (median)")
ax[0].legend(frameon=False, fontsize=8); fig.tight_layout(); fig.savefig(OUT / "fig_tradeoff.png", dpi=170); plt.close(fig)

fig, ax = plt.subplots(1, 2, figsize=(13, 5.5))
for a, nm, ttl in zip(ax, ("G", "C"), ("Life-cycle GHG", "Net levelised cost")):
    t = SENS.xs(nm, axis=1, level=1)[PW]; top = t.max(axis=1).nlargest(12).index[::-1]
    t.loc[top].plot.barh(ax=a, color=[COL[k] for k in PW], width=0.85, legend=False)
    a.set_xlabel("mean |Spearman rho| over 21 locations"); a.set_title(ttl + ": most influential inputs", fontsize=10)
ax[1].legend([NAMES[k] for k in PW], frameon=False, fontsize=8); fig.tight_layout()
fig.savefig(OUT / "fig_sensitivity.png", dpi=170); plt.close(fig)

fig, ax = plt.subplots(1, 2, figsize=(13, 4.8))
for a, ind, lab in zip(ax, ("CED", "LU"), ("Fossil CED, GJ per tonne MSW (negative = saving)", "Land take, m2 per tonne MSW")):
    q = MC["market"]
    vals = [q[q.pathway == k][f"{ind}_p50"].to_numpy() / (1e3 if ind == "CED" else 1) for k in PW]
    a.boxplot(vals, vert=False); a.set_yticks(range(1, len(PW) + 1), [NAMES[k] for k in PW])
    a.axvline(0, c="k", lw=0.5); a.set_xlabel(lab); a.set_title(lab.split(",")[0] + " (Monte Carlo medians, 21 locations)", fontsize=10)
fig.tight_layout(); fig.savefig(OUT / "fig_ced_landuse.png", dpi=170); plt.close(fig)

ROB = pd.DataFrame({nm: winners(MC[nm]).best for nm in SCENARIOS}).reindex(H.city)
ROB.to_csv(OUT / "12_most_probable_by_scenario.csv")
print("\nMost probable pathway by scenario\n", ROB.to_string())
print("Done. Files written to", OUT.resolve())

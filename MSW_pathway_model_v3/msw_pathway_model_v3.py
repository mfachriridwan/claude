# %% [markdown]
# # MSW recovery-pathway screening model, version 3.2
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
from mswpath import MSWModel, read_input, PW, NAMES, SCENARIOS, CARBON_VALUES
from mswpath.thresholds import threshold_table

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
# ## Main decision result: probability of being best at a fixed carbon value
# The carbon value is a policy choice, not an uncertain parameter, so the main result is P(best | carbon value) for
# 0, 2 (UU 7/2021 carbon tax, IDR 30/kg CO2e), 25, 50 and 100 USD per t CO2e, with the Monte Carlo standard error
# and a 95% interval. The decision criterion is the carbon-inclusive cost C + pG/1000 (USD per t MSW).

# %%
def pbest_table(df):
    rows = []
    for (city, k), g in df.groupby(["city", "pathway"], sort=False):
        r = g.iloc[0]
        for pc in CARBON_VALUES:
            p, se = r[f"p_best_at_{pc}"], r[f"p_best_at_{pc}_se"]
            rows.append(dict(city=city, scenario=r.scenario, carbon_value=pc, pathway=k, p_best=p, se=se,
                             ci95_low=max(p - 1.96 * se, 0), ci95_high=min(p + 1.96 * se, 1)))
    return pd.DataFrame(rows)


PBEST = pd.concat([pbest_table(df) for df in MC.values()], ignore_index=True)
TOP = (PBEST.sort_values("p_best", ascending=False).groupby(["scenario", "city", "carbon_value"], sort=False).head(1)
       .rename(columns={"pathway": "most_probable", "p_best": "p_most_probable"}))
print("\nMost probable pathway at fixed carbon values (market case)")
print(TOP[TOP.scenario == "market"].pivot(index="city", columns="carbon_value", values="most_probable")
      .reindex(H.city).to_string())

# %% [markdown]
# ## Stress tests: does the ranking change when an assumption is pushed to its bound?

# %%
STRESS = ["moisture_high_bound", "rdf_ncv_stress", "glass_imputed", "dirichlet_pseudocount", "AD_feed_optimistic",
          "moisture_IPCC_default", "phb_large_scale_cost"]
base = TOP[TOP.scenario == "market"].set_index(["city", "carbon_value"]).most_probable
srows = []
for nm in STRESS:
    alt = TOP[TOP.scenario == nm].set_index(["city", "carbon_value"]).most_probable.reindex(base.index)
    for pc in CARBON_VALUES:
        m = base.xs(pc, level=1) != alt.xs(pc, level=1)
        srows.append(dict(scenario=nm, carbon_value=pc, n_locations=int(m.size), n_changed=int(m.sum()),
                          changed=", ".join(f"{cty} ({base[(cty, pc)]}->{alt[(cty, pc)]})" for cty in m[m].index)))
STRESSTAB = pd.DataFrame(srows)
print(STRESSTAB.drop(columns="changed").pivot(index="scenario", columns="carbon_value", values="n_changed").to_string())

# %% [markdown]
# ## Break-even thresholds against the sanitary landfill
# How good must one input be for an option to match SL on carbon-inclusive cost (all other inputs central)?

# %%
THR = threshold_table(M)
print(THR[(THR.option == "S5") & (THR.carbon_value == 50)].groupby("input").break_even
      .agg(lambda x: ", ".join(sorted({f"{v:.3g}" if isinstance(v, float) else v for v in x}))[:120]).to_string())

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


def verdict(v, lo, hi):
    """Benchmark validation: compare the model range over 21 locations with an independent Indonesian reference range."""
    if lo is None: return "no reference (reported for transparency)"
    x = CHAR[v] if isinstance(v, str) else pd.Series([v]); med = x.median()
    if lo <= med <= hi: return "median within reference"
    if x.max() >= lo and x.min() <= hi: return "range overlaps reference"
    return "below reference" if med < lo else "above reference"


# Reference values re-checked in v3.2 (data/secondary_data_verification.csv); none comes from the 21 RIPS locations,
# so these are external plausibility checks, not a validation of facilities that do not yet exist.
BREF = [("bulk moisture (-)", "moisture", ".2f", 0.50, 0.60,
         "0.554, raw MSW at Cilacap before biodrying (single site); the 0.53-0.56 and 0.64-0.66 values cited in v3.1 "
         "could not be confirmed and were removed"),
        ("LHV as received (MJ/kg)", "LHV", ".1f", 6.28, 8.97,
         "6.28 (Yuliani et al. 2022, design basis) and 6.86-8.97 as received (Prabowo et al. 2019, HHV/LHV basis not "
         "stated in the abstract)"),
        ("HHV of dry matter (MJ/kg)", "HHV_dry", ".1f", None, None, "not compared: no reference with a stated dry basis"),
        ("L0 at MCF = 1 (kg CH4/t)", "L0_sl", ".0f", None, None, "no Indonesian measurement"),
        ("RDF yield (t/t MSW)", "rdf_yield", ".2f", 0.20, 0.42, "0.20-0.42, Indonesian RDF plants (GIZ 2023; not re-verified in v3.2)"),
        ("RDF heating value (MJ/kg)", "rdf_ncv", ".1f", 15.0, 16.7,
         "15-16.7, Cilacap RDF (3,991 kcal/kg at 24% moisture; about 15 after biodrying)"),
        ("WtE net electricity (kWh/t)", "wte_kwh", ".0f", 385, 473,
         "385 net (Yuliani et al. 2022: 24.08 MW from 1,500 t/d) to 473 (Azis et al. 2021: 19.7 MW from 1,000 t/d); "
         "v3.1 quoted 632 kWh/t, which is not in the source")]
BENCH = pd.DataFrame([dict(indicator=n, model_21_locations=span(v, f), reference_low=lo, reference_high=hi,
                           reference=ref, verdict=verdict(v, lo, hi)) for n, v, f, lo, hi, ref in BREF]
                     + [dict(indicator="carbon in biogas / carbon in food", model_21_locations=f"{c_gas / c_food:.2f}",
                             reference_low=0, reference_high=1, reference="must be below 1 (mass balance)",
                             verdict="median within reference" if c_gas < c_food else "above reference")]).set_index("indicator")
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
                    ("14_lcia_factors_used", M.LCIA, True), ("15_p_best_fixed_carbon_value", PBEST.round(4), False),
                    ("16_stress_tests_ranking_changes", STRESSTAB, False), ("17_break_even_thresholds_vs_SL", THR, False),
                    ("18_scenario_parameters_used", M.SP, True)):
    df.to_csv(OUT / f"{nm}.csv", index=idx)

COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}
fig, axs = plt.subplots(2, 3, figsize=(15, 12), sharey=True)
PANELS = [("market", 25, "Market case, 25 USD/t CO2e"), ("market", 50, "Market case, 50 USD/t CO2e"),
          ("market", 100, "Market case, 100 USD/t CO2e"),
          ("perpres109", 50, "Perpres 109/2025 WtE tariff\n(1,000 t/day or more), 50 USD/t CO2e"),
          ("AD_feed_optimistic", 50, "AD feed as good as source-separated\nfood (optimistic), 50 USD/t CO2e"),
          ("food_separated_at_source", 50, "Food waste already separated\nat source, 50 USD/t CO2e")]
for a, (key, pc, ttl) in zip(axs.flat, PANELS):
    winners(MC[key].rename(columns={f"p_best_at_{pc}": "p_fixed"}).assign(p_best=lambda d: d.p_fixed))[PW].iloc[::-1] \
        .plot.barh(stacked=True, ax=a, color=[COL[k] for k in PW], width=0.8, legend=False)
    a.set_title(ttl, fontsize=10); a.set_xlabel("P(best feasible pathway | carbon value)"); a.set_xlim(0, 1); a.set_ylabel("")
axs[0, 2].legend([NAMES[k] for k in PW], loc="center left", bbox_to_anchor=(1.0, 0.5), frameon=False)
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

ROB = TOP.pivot_table(index="city", columns=["scenario", "carbon_value"], values="most_probable", aggfunc="first").reindex(H.city)
ROB.to_csv(OUT / "12_most_probable_by_scenario.csv")
print("\nMost probable pathway by scenario at 50 USD/t CO2e\n", ROB.xs(50, axis=1, level=1).to_string())
print("Done. Files written to", OUT.resolve())

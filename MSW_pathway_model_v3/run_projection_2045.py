# %% [markdown]
# # Projection to 2045 against the existing condition of 2025
# Grid emission factor on a linear path to zero in 2060 (delta = 1/35 per year); real escalation of electricity value
# (2%/yr), plant O&M (1%/yr) and landfill cost (2%/yr); tonnage growth at each location's rate. Writes outputs/proj2045_*.

# %%
import numpy as np, pandas as pd
from pathlib import Path
from mswpath import MSWModel, read_input, PW, CARBON_VALUES
from mswpath.projection import model_for_year, read_parameters, grid_factor

try:
    ROOT = Path(__file__).resolve().parent
except NameError:
    ROOT = Path.cwd()
DATA, OUT = ROOT / "data", ROOT / "outputs"
raw = read_input(DATA / "input_kota_2025_updated.csv")
PR = read_parameters(DATA); Y = int(PR["target_year"])
N = 4000
KEY = ["city", "baseline", "pathway"]


def central(M):
    det, char, best = M.run_central({"market": {}})
    return det, char, best


# %% central results: 2025, 2045 and one driver at a time
M25 = MSWModel(DATA); M25.load_cities(raw)
runs = {"2025": central(M25)}
for lab, comp in (("2045", ("grid", "costs", "tonnage")), ("grid only", ("grid",)), ("costs only", ("costs",)),
                  ("tonnage only", ("tonnage",))):
    M, info = model_for_year(DATA, raw, Y, comp, pr=PR)
    runs[lab] = central(M)
    if lab == "2045": INFO = info
rows = []
for lab, (det, char, best) in runs.items():
    rows.append(det.assign(case=lab))
DET = pd.concat(rows, ignore_index=True)
DET.to_csv(OUT / "proj2045_central_all_cases.csv", index=False)

d25 = runs["2025"][0].set_index(KEY); d45 = runs["2045"][0].set_index(KEY)
CMP = pd.DataFrame({"G_2025": d25.G, "G_2045": d45.G, "C_2025": d25.C, "C_2045": d45.C, "CED_2025": d25.CED,
                    "CED_2045": d45.CED, "dG_2025": d25.dG, "dG_2045": d45.dG, "MAC_2025": d25.MAC, "MAC_2045": d45.MAC,
                    "feasible_2025": d25.feasible, "feasible_2045": d45.feasible}).reset_index()
CMP["G_change"] = CMP.G_2045 - CMP.G_2025; CMP["C_change"] = CMP.C_2045 - CMP.C_2025
CMP["G_change_pct"] = 100 * CMP.G_change / CMP.G_2025.abs(); CMP["C_change_pct"] = 100 * CMP.C_change / CMP.C_2025.abs()
for pc in (25, 50, 100):
    CMP[f"CIC{pc}_2025"] = CMP.C_2025 + pc * CMP.G_2025 / 1e3; CMP[f"CIC{pc}_2045"] = CMP.C_2045 + pc * CMP.G_2045 / 1e3
CMP.to_csv(OUT / "proj2045_comparison_central.csv", index=False)

# decomposition of the change (baseline OD rows carry the option values)
dec = []
for ind in ("G", "C", "CED"):
    base = d25[ind]
    parts = {lab: runs[lab][0].set_index(KEY)[ind] - base for lab in ("grid only", "costs only", "tonnage only", "2045")}
    t = pd.DataFrame(parts); t["interaction"] = t["2045"] - t[["grid only", "costs only", "tonnage only"]].sum(axis=1)
    dec.append(t.assign(indicator=ind).reset_index())
DEC = pd.concat(dec, ignore_index=True)
DEC.to_csv(OUT / "proj2045_decomposition.csv", index=False)

best25, best45 = runs["2025"][2].set_index("city"), runs["2045"][2].set_index("city")
BEST = pd.concat({"2025": best25[[f"best_at_{p}" for p in (0, 25, 50, 100)]],
                  "2045": best45[[f"best_at_{p}" for p in (0, 25, 50, 100)]]}, axis=1)
BEST.to_csv(OUT / "proj2045_best_central.csv")
CHAR = pd.concat({"2025": runs["2025"][1][["Q_2025", "wte_kwh", "rdf_ncv", "gate_WtE_scale", "gate_PSEL_generated", "phb_tpa", "gate_PHB"]],
                  "2045": runs["2045"][1][["Q_2025", "wte_kwh", "rdf_ncv", "gate_WtE_scale", "gate_PSEL_generated", "phb_tpa", "gate_PHB"]]}, axis=1)
CHAR.to_csv(OUT / "proj2045_scale_and_gates.csv")

# %% Monte Carlo, paired draws
mc25, s25 = M25.run_mc("market", N=N, keep=True)
M45, _ = model_for_year(DATA, raw, Y, pr=PR)
mc45, s45 = M45.run_mc("market", N=N, keep=True)
pb = []
for lab, mc in (("2025", mc25), ("2045", mc45)):
    for r in mc.itertuples():
        for pc in CARBON_VALUES:
            pb.append(dict(year=lab, city=r.city, pathway=r.pathway, carbon_value=pc, p_best=getattr(r, f"p_best_at_{pc}"),
                           se=getattr(r, f"p_best_at_{pc}_se")))
PB = pd.DataFrame(pb); PB.to_csv(OUT / "proj2045_p_best_fixed_carbon.csv", index=False)
q = lambda a: np.percentile(a, [5, 50, 95])
dist = []
for cid in s25:
    P0, _, R0 = s25[cid]; P1, _, R1 = s45[cid]; city = M25.H.loc[cid].city
    for j, k in enumerate(PW):
        for ind in ("G", "C", "CED"):
            a0, a1 = R0[ind][:, j], R1[ind][:, j]
            p5, p50, p95 = q(a1 - a0)
            dist.append(dict(city=city, pathway=k, indicator=ind, y2025_p50=np.median(a0), y2045_p50=np.median(a1),
                             change_p5=p5, change_p50=p50, change_p95=p95))
DIST = pd.DataFrame(dist); DIST.to_csv(OUT / "proj2045_change_distribution.csv", index=False)

# %% year path 2025-2060 (central values, all drivers)
path = []
for yr in range(2025, 2061, 5):
    M, info = (M25, {"grid_factor": 1.0}) if yr == 2025 else model_for_year(DATA, raw, yr, pr=PR)
    det, _, _ = M.run_central({"market": {}})
    o = det[det.baseline == "OD"]
    for k in PW:
        x = o[o.pathway == k]
        for city in ("Kota Padang", "median"):
            v = x[x.city == city] if city != "median" else x
            path.append(dict(year=yr, location=city, pathway=k, grid_factor=grid_factor(PR, yr), G=v.G.median(), C=v.C.median(),
                             CIC50=(v.C + 50 * v.G / 1e3).median(), CIC100=(v.C + 100 * v.G / 1e3).median()))
PATH = pd.DataFrame(path); PATH.to_csv(OUT / "proj2045_year_path.csv", index=False)

pd.Series(INFO).to_csv(OUT / "proj2045_factors.csv", header=["value"])
o = CMP[CMP.baseline == "OD"]
print("Median over 21 locations (central):")
print(o.groupby("pathway")[["G_2025", "G_2045", "G_change", "C_2025", "C_2045", "C_change"]].median().reindex(PW).round(1))
print(BEST.apply(lambda c: c.value_counts()).fillna(0).astype(int))
print("Done.")

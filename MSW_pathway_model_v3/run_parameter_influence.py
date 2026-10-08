# %% [markdown]
# # How much each input moves the results (one at a time, central values, 21 locations)
# Each register / realism parameter is set to its low and to its high value with all others central; the moisture of
# all fractions is moved together to its low and high bound. Writes outputs/parameter_influence.csv.

# %%
import numpy as np, pandas as pd
from pathlib import Path
from mswpath import MSWModel, read_input, PW

ROOT = Path(__file__).resolve().parent
DATA, OUT = ROOT / "data", ROOT / "outputs"
raw = read_input(DATA / "input_kota_2025_updated.csv")


def summary(M, fix=None):
    det, _, best = M.run_central({"market": dict(fix=fix) if fix else {}})
    o = det[det.baseline == "OD"]
    g = o.pivot(index="city", columns="pathway", values="G")[PW]
    c = o.pivot(index="city", columns="pathway", values="C")[PW]
    e = o.pivot(index="city", columns="pathway", values="CED")[PW]
    gap = {pc: (c.S5 + pc * g.S5 / 1e3) - (c.SL + pc * g.SL / 1e3) for pc in (50, 100)}
    return dict(G=g.median(), C=c.median(), CED=e.median(), gap50=gap[50].median(), gap100=gap[100].median(),
                best50=best.set_index("city").best_at_50, best100=best.set_index("city").best_at_100)


M = MSWModel(DATA); M.load_cities(raw); B = summary(M)
reg = M.REG[(M.REG.high > M.REG.low) & ~M.REG.index.isin(["alpha_hi", "alpha_lo"])]
sp = M.SP[(M.SP.use == "main case") & (M.SP.high > M.SP.low)]
rows = []


def record(key, meaning, unit, lo, c0, hi, s_lo, s_hi):
    r = dict(key=key, meaning=meaning, unit=unit, low=lo, central=c0, high=hi)
    for lab, s in (("lo", s_lo), ("hi", s_hi)):
        for k in PW:
            r[f"G_{k}_{lab}"] = s["G"][k] - B["G"][k]; r[f"C_{k}_{lab}"] = s["C"][k] - B["C"][k]
            r[f"CED_{k}_{lab}"] = s["CED"][k] - B["CED"][k]
        r[f"gap50_{lab}"] = s["gap50"]; r[f"gap100_{lab}"] = s["gap100"]
        r[f"n_change50_{lab}"] = int((s["best50"] != B["best50"]).sum()); r[f"n_change100_{lab}"] = int((s["best100"] != B["best100"]).sum())
    r["swing_gap50"] = abs(s_hi["gap50"] - s_lo["gap50"]); r["swing_gap100"] = abs(s_hi["gap100"] - s_lo["gap100"])
    rows.append(r)


for src in (reg, sp):
    for k, v in src.iterrows():
        Ml = MSWModel(DATA); Ml.load_cities(raw)
        record(k, v.meaning, v.unit, v.low, v.central, v.high, summary(Ml, {k: v.low}), summary(Ml, {k: v.high}))
# all moistures together (common wetness draw at its bounds)
res = []
for which in ("W_LO", "W_HI"):
    Mw = MSWModel(DATA); Mw.W = np.array(getattr(Mw, which), float); Mw.load_cities(raw); res.append(summary(Mw))
record("wetness", "Moisture of all fractions as received, moved together to its low / high bound", "-", 0, 0.5, 1, *res)
T = pd.DataFrame(rows)
T.to_csv(OUT / "parameter_influence.csv", index=False)
base = pd.DataFrame({"G": B["G"], "C": B["C"], "CED": B["CED"]})
base.to_csv(OUT / "parameter_influence_base.csv")
pd.Series({"gap50": B["gap50"], "gap100": B["gap100"]}).to_csv(OUT / "parameter_influence_base_gap.csv", header=["value"])
print("base gap S5-SL at 50:", round(B["gap50"], 2), "at 100:", round(B["gap100"], 2))
print(T.sort_values("swing_gap50", ascending=False)[["key", "low", "high", "gap50_lo", "gap50_hi", "n_change100_lo", "n_change100_hi"]].head(15).round(2).to_string(index=False))

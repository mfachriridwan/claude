# %% [markdown]
# # Scenario discovery: under what conditions does each option become preferable?
# Runs the model over a space of city and waste-system conditions, extracts decision rules (CART tree and PRIM boxes),
# draws condition maps, and computes Sobol indices for the 21 RIPS locations. Writes outputs/discovery_*.

# %%
import json
import numpy as np, pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from pathlib import Path
from mswpath import MSWModel, read_input, PW, NAMES
from mswpath.discovery import sample_conditions, cart_rules, prim_box, condition_map, sobol_indices, FEATURES, plot_condition_maps

try:
    ROOT = Path(__file__).resolve().parent
except NameError:
    ROOT = Path.cwd()
DATA, OUT = ROOT / "data", ROOT / "outputs"
OUT.mkdir(exist_ok=True)
COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}

M = MSWModel(DATA)
M.load_cities(read_input(DATA / "input_kota_2025_updated.csv"))

# %% [markdown]
# ## 1. Sample the condition space and record the best option

# %%
X, best, R, meta = sample_conditions(M, N=40000, seed=7)
print(meta)
print(best.value_counts(normalize=True).round(3).to_string())
pd.concat([X, best], axis=1).to_csv(OUT / "discovery_samples.csv.gz", index=False, compression="gzip")

# %% [markdown]
# ## 2. Decision rules: classification tree (CART)

# %%
CT = cart_rules(X, best, max_depth=4, min_leaf=0.02)
print(f"accuracy train {CT['acc_train']:.2f}, test {CT['acc_test']:.2f}, majority-class baseline {CT['baseline_acc']:.2f}")
print(CT["text"])
CT["leaves"].to_csv(OUT / "discovery_cart_rules.csv", index=False)
CT["importance"].rename("importance").to_csv(OUT / "discovery_cart_importance.csv")

# %% [markdown]
# ## 3. PRIM boxes: the conditions under which each option is best most often

# %%
boxes = []
for k in PW:
    if (best == k).mean() < 0.03:
        boxes.append(dict(option=k, base_rate=(best == k).mean(), note="best in fewer than 3% of draws; no box"))
        continue
    b, _ = prim_box(X, best == k, alpha=0.05, min_coverage=0.3)
    boxes.append(dict(option=k, base_rate=b["base_rate"], coverage=b["coverage"], density=b["density"], mass=b["mass"],
                      box="; ".join(f"{f}: {v[0]:.3g} to {v[1]:.3g}" if v[0] != v[1] else f"{f} = {v[0]:.0f}"
                                    for f, v in b["box"].items())))
PRIM = pd.DataFrame(boxes)
print(PRIM.to_string())
PRIM.to_csv(OUT / "discovery_prim_boxes.csv", index=False)

# %% [markdown]
# ## 4. Condition maps

# %%
plot_condition_maps(X, best, OUT / "fig_condition_maps.png")

fig, ax = plt.subplots(figsize=(7, 4))
CT["importance"].iloc[::-1].plot.barh(ax=ax, color="#46719e")
ax.set_yticks(range(len(CT["importance"])), [FEATURES[k] for k in CT["importance"].index[::-1]], fontsize=8)
ax.set_xlabel("Feature importance in the decision tree"); fig.tight_layout(); fig.savefig(OUT / "fig_cart_importance.png", dpi=170); plt.close(fig)

from sklearn.tree import plot_tree
fig, ax = plt.subplots(figsize=(22, 9))
plot_tree(CT["tree"], feature_names=list(X.columns), class_names=list(CT["tree"].classes_), filled=False, impurity=False,
          proportion=True, rounded=True, fontsize=7, ax=ax, precision=2)
fig.savefig(OUT / "fig_cart_tree.png", dpi=150, bbox_inches="tight"); plt.close(fig)

# %% [markdown]
# ## 5. Where the 21 locations sit: the tree applied to their central conditions

# %%
H = M.H; ch = pd.read_csv(OUT / "02_characterisation_and_gates.csv").set_index("city")
loc = pd.DataFrame({"carbon_value": 50.0, "Q_tpd": H.Q2025.values, "kiln_km": H.d_line.values * M.REG.central["tort"],
                    "grid_ef": H.ef_grid.values, "tariff": 0, "food_separated": 0,
                    "food_share": H.s_food.values, "plastic_share": H.s_plastic.values,
                    "paper_garden_share": (H.s_paper + H.s_garden + H.s_wood).values,
                    "moisture": ch.loc[H.city, "moisture"].values, "LHV": ch.loc[H.city, "LHV"].values,
                    "gas_capture": M.REG.central["cap"], "landfill_cost": M.REG.central["c_sl"]}, index=H.city)
for pc in (0, 25, 50, 100):
    loc[f"tree_best_at_{pc}"] = CT["tree"].predict(loc[X.columns].assign(carbon_value=float(pc)))
loc.to_csv(OUT / "discovery_locations_on_tree.csv")
print(loc[[c for c in loc.columns if c.startswith("tree_")]].to_string())

# %% [markdown]
# ## 6. Sobol indices (register parameters, composition fixed) for each location

# %%
SOB = pd.concat([sobol_indices(M, i, N=512) for i in range(len(M.H))], ignore_index=True)
SOB.to_csv(OUT / "discovery_sobol_by_location.csv.gz", index=False, compression="gzip")
SOBM = SOB.groupby(["output", "input"])[["S1", "ST"]].mean().reset_index()
SOBM.to_csv(OUT / "discovery_sobol_mean.csv", index=False)
top = (SOBM[SOBM.output == "CICgap_S5_SL"].sort_values("ST", ascending=False).head(12))
print(top.to_string())
fig, ax = plt.subplots(1, 2, figsize=(13, 5))
for a, outn, ttl in zip(ax, ("CICgap_S5_SL", "G_SL"), ("Carbon-inclusive cost gap RDF + AD minus landfill at 50 USD/t", "GHG of sanitary landfill")):
    t = SOBM[SOBM.output == outn].sort_values("ST", ascending=False).head(10).iloc[::-1]
    a.barh(t.input, t.ST, color="#c9d6e8", label="total (ST)"); a.barh(t.input, t.S1, color="#46719e", height=0.45, label="first order (S1)")
    a.set_title(ttl + "\n(mean over 21 locations)", fontsize=9); a.legend(frameon=False, fontsize=8); a.set_xlabel("Sobol index")
fig.tight_layout(); fig.savefig(OUT / "fig_sobol.png", dpi=170); plt.close(fig)

json.dump({k: (v if not isinstance(v, (np.floating,)) else float(v)) for k, v in meta.items()},
          open(OUT / "discovery_meta.json", "w"), indent=1, default=float)
json.dump(dict(acc_train=CT["acc_train"], acc_test=CT["acc_test"], baseline=CT["baseline_acc"]),
          open(OUT / "discovery_cart_accuracy.json", "w"), indent=1)
print("Done.")

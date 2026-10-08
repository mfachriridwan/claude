# %% [markdown]
# # Value of information: what should be measured first?
# EVPI and EVPPI per location at carbon values of 25, 50 and 100 USD/t CO2e (market case, 4,000 draws).
# Writes outputs/voi_*.csv and outputs/fig_voi.png.

# %%
import numpy as np, pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
from mswpath import MSWModel, read_input, PW
from mswpath.voi import voi_location, voi_groups

try:
    ROOT = Path(__file__).resolve().parent
except NameError:
    ROOT = Path.cwd()
DATA, OUT = ROOT / "data", ROOT / "outputs"
M = MSWModel(DATA)
M.load_cities(read_input(DATA / "input_kota_2025_updated.csv"))
_, samples = M.run_mc("market", keep=True)

rows, summ, grows = [], [], []
for pc in (25, 50, 100):
    for cid, (P, s, R) in samples.items():
        c = M.H.loc[cid]
        df, meta = voi_location(P, s, R, M.FR, pc=pc)
        q = c.Q2025 * 365
        df = df.assign(city=c.city, carbon_value=pc, Q2025=c.Q2025, evppi_usd_per_year=df.evppi_net * q)
        rows.append(df)
        summ.append(dict(city=c.city, carbon_value=pc, Q2025=c.Q2025, evpi=meta["evpi"], evpi_usd_per_year=meta["evpi"] * q,
                         noise_floor=meta["floor"], best_expected=meta["best_expected"], choice_set=meta["choice_set"]))
        dg, _ = voi_groups(P, s, R, M.FR, pc=pc)
        grows.append(dg.assign(city=c.city, carbon_value=pc, Q2025=c.Q2025, evppi_usd_per_year=dg.evppi_net * q))
VOI = pd.concat(rows, ignore_index=True)
GRP = pd.concat(grows, ignore_index=True)
GRP.to_csv(OUT / "voi_groups_by_location.csv", index=False)
GRANK = (GRP.groupby(["carbon_value", "group"]).agg(median_evppi=("evppi_net", "median"), mean_share_of_evpi=("share_of_evpi", "mean"),
                                                    median_usd_per_year=("evppi_usd_per_year", "median"),
                                                    max_usd_per_year=("evppi_usd_per_year", "max"))
         .reset_index().sort_values(["carbon_value", "median_evppi"], ascending=[True, False]))
GRANK.to_csv(OUT / "voi_groups_ranking.csv", index=False)
SUM = pd.DataFrame(summ)
VOI.to_csv(OUT / "voi_evppi_by_location.csv", index=False)
SUM.to_csv(OUT / "voi_evpi_by_location.csv", index=False)
RANK = (VOI.groupby(["carbon_value", "input"]).agg(median_evppi=("evppi_net", "median"), max_evppi=("evppi_net", "max"),
                                                    median_usd_per_year=("evppi_usd_per_year", "median"),
                                                    n_locations_positive=("evppi_net", lambda x: int((x > 0.05).sum())))
        .reset_index().sort_values(["carbon_value", "median_evppi"], ascending=[True, False]))
RANK.to_csv(OUT / "voi_ranking.csv", index=False)
print(SUM.groupby("carbon_value")[["evpi", "evpi_usd_per_year"]].median().round(2))
for pc in (25, 50, 100):
    print(f"\nCarbon value {pc}: top inputs by median EVPPI (USD/t)")
    print(RANK[RANK.carbon_value == pc].head(8).round(3).to_string(index=False))

for pc in (25, 50, 100):
    print(f"\nCarbon value {pc}: groups"); print(GRANK[GRANK.carbon_value == pc].round(3).to_string(index=False))
fig, ax = plt.subplots(1, 3, figsize=(15, 4.6), sharey=True)
for a, pc in zip(ax, (25, 50, 100)):
    t = GRANK[GRANK.carbon_value == pc].set_index("group").reindex(list(dict.fromkeys(GRANK.group)))
    a.barh([g.split(" (")[0] for g in t.index[::-1]], t.median_evppi.values[::-1], color="#46719e", label="median over 21 locations")
    mx = GRP[GRP.carbon_value == pc].groupby("group").evppi_net.max().reindex(t.index)
    a.scatter(mx.values[::-1], [g.split(" (")[0] for g in t.index[::-1]], color="#d9822b", zorder=3, s=14, label="maximum")
    ev = SUM[SUM.carbon_value == pc].evpi.median()
    a.set_title(f"Carbon value {pc} USD/t CO2e (median EVPI {ev:.2f} USD/t)", fontsize=9)
    a.set_xlabel("EVPPI of the group, USD per tonne MSW"); a.tick_params(labelsize=8)
ax[0].legend(fontsize=7, frameon=False)
fig.tight_layout(); fig.savefig(OUT / "fig_voi.png", dpi=170); plt.close(fig)
print("Done.")

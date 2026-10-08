"""
User input for new cities: a simple one-row-per-city template, its validation, and conversion to the v3 input format.

The template asks only for what a planner usually has. Rules applied (and reported back as warnings):
- Tonnage is projected once to 2025: Q2025 = Qt (1 + g)^(2025 - t), unless `tonnage_is_2025` is yes.
  A blank growth rate uses the fallback median of the RIPS rates (register g_default, an assumption).
- Composition percentages (wet mass) are normalised to 100 and the factor is reported. Blank = not reported, not zero.
- One composition is used for the whole stream (domestic and non-domestic are not separated in the template).
- Either lat/lon (nearest kiln from data/cement_kilns.csv) or a road distance to a kiln must be given.
"""
import numpy as np
import pandas as pd
from .core import RIPS_CATS

TEMPLATE_COLUMNS = {
    # column: (required, description EN, description ID)
    "city_id": (True, "Unique id without spaces", "Kode unik tanpa spasi"),
    "city_name": (True, "Name of the city/regency", "Nama kota/kabupaten"),
    "province": (False, "Province", "Provinsi"),
    "grid_region": (True, "Grid: Jamali, Sumatera or Mahakam", "Sistem listrik: Jamali, Sumatera, atau Mahakam"),
    "tonnage_tpd": (True, "Waste to be managed, t/day (wet)", "Timbulan/sampah yang dikelola, ton/hari (basah)"),
    "tonnage_year": (True, "Year the tonnage refers to", "Tahun data tonase"),
    "tonnage_is_2025": (False, "yes if the tonnage already refers to 2025", "yes bila tonase sudah tahun 2025"),
    "growth_rate": (False, "Annual growth of tonnage, fraction (e.g. 0.017); blank = fallback 1.73%", "Laju pertumbuhan per tahun, pecahan (mis. 0.017); kosong = 1,73%"),
    "composition_year": (True, "Year of the composition sampling", "Tahun sampling komposisi"),
    "organik_lumped": (False, "yes if food and garden waste are reported together as 'organik' in sisa_makanan", "yes bila sampah organik digabung di sisa_makanan"),
    "lat": (False, "Latitude of the facility site (decimal degrees)", "Lintang lokasi fasilitas"),
    "lon": (False, "Longitude of the facility site", "Bujur lokasi fasilitas"),
    "kiln_road_km": (False, "Road distance to the nearest cement kiln, km (overrides lat/lon)", "Jarak jalan ke pabrik semen terdekat, km"),
    "managed_share": (False, "Share of the tonnage collected/managed today (0-1)", "Porsi sampah yang terkelola saat ini (0-1)"),
    "managed_share_definition": (False, "What the managed share measures (free text)", "Definisi indikator terkelola"),
    "source": (False, "Source document and page", "Dokumen sumber dan halaman"),
}
for c in RIPS_CATS:
    TEMPLATE_COLUMNS[f"pct_{c}"] = (False, f"Wet-mass % of {c} (blank = not reported)", f"% berat basah {c} (kosong = tidak dilaporkan)")

YES = {"yes", "y", "ya", "true", "1", "1.0"}


def _yes(v):
    return str(v).strip().lower() in YES


def validate_template(df, M):
    """Return (errors, warnings) as lists of strings. Errors stop the conversion."""
    err, warn = [], []
    for c, (req, _, _) in TEMPLATE_COLUMNS.items():
        if req and c not in df.columns:
            err.append(f"missing required column '{c}'")
    if err:
        return err, warn
    if df.city_id.duplicated().any():
        err.append("city_id values must be unique")
    pcols = [f"pct_{c}" for c in RIPS_CATS if f"pct_{c}" in df.columns]
    if not pcols:
        err.append("no composition columns (pct_...) found")
    for _, r in df.iterrows():
        tag = f"[{r.city_id}]"
        if str(r.grid_region) not in M.EF_GRID:
            err.append(f"{tag} grid_region must be one of {list(M.EF_GRID)}")
        q = pd.to_numeric(r.tonnage_tpd, errors="coerce")
        if not (q > 0):
            err.append(f"{tag} tonnage_tpd must be a positive number")
        for y in ("tonnage_year", "composition_year"):
            v = pd.to_numeric(r[y], errors="coerce")
            if not (1990 <= v <= 2025):
                err.append(f"{tag} {y} must be a year between 1990 and 2025")
        vals = pd.to_numeric(r[pcols], errors="coerce")
        if (vals < 0).any():
            err.append(f"{tag} composition percentages cannot be negative")
        tot = vals.sum(skipna=True)
        if not (90 <= tot <= 110):
            err.append(f"{tag} composition sums to {tot:.2f}%; expected about 100%")
        elif abs(tot - 100) > 0.05:
            warn.append(f"{tag} composition sums to {tot:.2f}%; normalised (factor {100 / tot:.4f})")
        has_xy = pd.notna(r.get("lat")) and pd.notna(r.get("lon"))
        has_km = pd.notna(r.get("kiln_road_km"))
        if not (has_xy or has_km):
            err.append(f"{tag} give lat/lon or kiln_road_km")
        g = r.get("growth_rate")
        if not _yes(r.get("tonnage_is_2025", "")) and pd.isna(g) and int(r.tonnage_year) < 2025:
            warn.append(f"{tag} growth_rate blank: fallback {M.REG.central['g_default']:.4f}/yr used (assumption)")
        m = pd.to_numeric(r.get("managed_share"), errors="coerce")
        if pd.notna(m) and not (0 <= m <= 1):
            err.append(f"{tag} managed_share must be between 0 and 1")
        if pd.notna(m) and not str(r.get("managed_share_definition", "")).strip():
            warn.append(f"{tag} managed_share has no definition; it cannot be compared with other locations")
        if int(r.composition_year) < 2020:
            warn.append(f"{tag} composition sampled before 2020: used as a 2025 proxy, wider uncertainty (alpha 40)")
    return err, warn


def from_template(df, M):
    """Convert a validated template to the v3 input format read by MSWModel. Raises ValueError on errors."""
    df = df.copy()
    err, warn = validate_template(df, M)
    if err:
        raise ValueError("Template errors:\n- " + "\n- ".join(err))
    g_fb = M.REG.central["g_default"]
    rows = []
    for _, r in df.iterrows():
        qt = float(r.tonnage_tpd); t = int(r.tonnage_year)
        already = _yes(r.get("tonnage_is_2025", "")) or t >= 2025
        g_user = pd.to_numeric(r.get("growth_rate"), errors="coerce")
        g, basis = (float(g_user), "user-supplied growth rate") if pd.notna(g_user) else (g_fb, "fallback median of RIPS rates (assumption)")
        expo = 0 if already else 2025 - t
        q25 = qt * (1 + g) ** expo
        out = dict(city_id=str(r.city_id), city_name=str(r.city_name), province=r.get("province", ""),
                   admin_level="", year_base=int(r.composition_year), composition_mode="combined",
                   M_dom_tpd=qt, M_nd_tpd=0.0, organik_lumped_to_food=_yes(r.get("organik_lumped", "")),
                   glass_folded_into_inert=False, source_pdf=r.get("source", "user template"), source_pages="",
                   composition_sampling_year=int(r.composition_year), tonnage_source_value_tpd=qt,
                   tonnage_source_year=t, tonnage_year_status="user_supplied", tonnage_already_2025=already,
                   baseline_year=2025, growth_rate_used=g, growth_rate_basis=basis, projection_exponent_years=expo,
                   M_dom_tpd_2025=q25, M_nd_tpd_2025=0.0, Q2025_tpd=q25, Q2025_alt_tpd=q25,
                   managed_share_status_index=np.nan,
                   managed_share_local=pd.to_numeric(r.get("managed_share"), errors="coerce"),
                   managed_share_local_lower=np.nan, grid_region=str(r.grid_region),
                   lat_used=pd.to_numeric(r.get("lat"), errors="coerce"),
                   lon_used=pd.to_numeric(r.get("lon"), errors="coerce"),
                   kiln_road_km=pd.to_numeric(r.get("kiln_road_km"), errors="coerce"),
                   glass_status="reported_separately" if pd.notna(r.get("pct_kaca")) else "not_reported_blank")
        for c in RIPS_CATS:
            v = pd.to_numeric(r.get(f"pct_{c}"), errors="coerce")
            out[f"dom_{c}"] = v
            out[f"nd_{c}"] = np.nan
        rows.append(out)
    raw = pd.DataFrame(rows)
    raw["tonnage_already_2025"] = raw.tonnage_already_2025.astype(bool)
    return raw, warn


def template_frame(lang="en"):
    """Empty template with a description row."""
    k = 1 if lang == "en" else 2
    return pd.DataFrame([{c: v[k] for c, v in TEMPLATE_COLUMNS.items()}])


def example_from_v3(raw_v3, city_ids):
    """Fill the template from existing v3 rows (used for the shipped example and for the round-trip test).
    Uses the domestic composition and the 2025 tonnage."""
    rows = []
    for _, r in raw_v3[raw_v3.city_id.isin(city_ids)].iterrows():
        d = dict(city_id=r.city_id, city_name=r.city_name, province=r.province, grid_region=r.grid_region,
                 tonnage_tpd=round(float(r.Q2025_tpd), 4), tonnage_year=2025, tonnage_is_2025="yes", growth_rate="",
                 composition_year=int(r.composition_sampling_year),
                 organik_lumped="yes" if bool(r.organik_lumped_to_food) else "no",
                 lat=r.lat_used, lon=r.lon_used, kiln_road_km="",
                 managed_share=r.managed_share_status_index,
                 managed_share_definition="RIPS status index" if pd.notna(r.managed_share_status_index) else "",
                 source=f"{r.source_pdf} {r.source_pages}")
        for c in RIPS_CATS:
            d[f"pct_{c}"] = r[f"dom_{c}"]
        rows.append(d)
    return pd.DataFrame(rows)

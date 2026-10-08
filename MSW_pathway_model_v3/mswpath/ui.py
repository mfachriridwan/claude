"""Interactive single-city form for notebooks (Jupyter / Google Colab), in English or Indonesian."""
from pathlib import Path
import pandas as pd
from .core import RIPS_CATS
from .inputs import from_template
from .report import analyse, central_table, prob_figure, write_excel, write_html, SCN_LABEL

LBL = {
    "en": dict(name="City name", grid="Grid region", q="Tonnage (t/day)", qy="Tonnage year", g="Growth rate (blank = fallback)",
               cy="Composition year", lump="Organics lumped in food", km="Road distance to cement kiln (km)",
               ms="Managed share (0-1, optional)", n="Monte Carlo draws", scen="Scenarios", run="Run analysis",
               comp="Composition, wet mass % (blank = not reported)", total="Total", wait="Running...",
               done="Reports written:", lang="Language"),
    "id": dict(name="Nama kota", grid="Sistem listrik", q="Tonase (ton/hari)", qy="Tahun tonase", g="Laju pertumbuhan (kosong = default)",
               cy="Tahun komposisi", lump="Organik digabung di sisa makanan", km="Jarak jalan ke pabrik semen (km)",
               ms="Porsi terkelola (0-1, opsional)", n="Jumlah iterasi Monte Carlo", scen="Skenario", run="Jalankan analisis",
               comp="Komposisi, % berat basah (kosong = tidak dilaporkan)", total="Total", wait="Sedang dihitung...",
               done="Laporan tersimpan:", lang="Bahasa"),
}
DEFAULT_PCT = {}   # filled from an example city so that the form starts with real (Kota Magelang) numbers


def city_form(M, lang="id", example=None, out_dir="outputs/user_reports"):
    import ipywidgets as W
    from IPython.display import display, clear_output
    L = LBL[lang]
    ex = example if example is not None else {}
    st = {"description_width": "260px"}; lay = W.Layout(width="520px")
    f = dict(
        name=W.Text(value=str(ex.get("city_name", "Kota Contoh")), description=L["name"], style=st, layout=lay),
        grid=W.Dropdown(options=list(M.EF_GRID), value=ex.get("grid_region", "Jamali"), description=L["grid"], style=st, layout=lay),
        q=W.BoundedFloatText(value=float(ex.get("tonnage_tpd", 500)), min=1, max=20000, description=L["q"], style=st, layout=lay),
        qy=W.BoundedIntText(value=int(ex.get("tonnage_year", 2025)), min=1990, max=2025, description=L["qy"], style=st, layout=lay),
        g=W.Text(value="", description=L["g"], style=st, layout=lay),
        cy=W.BoundedIntText(value=int(ex.get("composition_year", 2024)), min=1990, max=2025, description=L["cy"], style=st, layout=lay),
        lump=W.Checkbox(value=str(ex.get("organik_lumped", "no")) == "yes", description=L["lump"], style=st),
        km=W.BoundedFloatText(value=float(ex.get("kiln_road_km", 150) or 150), min=1, max=2000, description=L["km"], style=st, layout=lay),
        ms=W.Text(value="", description=L["ms"], style=st, layout=lay),
        n=W.IntSlider(value=1000, min=200, max=4000, step=200, description=L["n"], style=st, layout=lay),
        scen=W.SelectMultiple(options=[(SCN_LABEL[lang][k], k) for k in ("market", "perpres109", "food_separated_at_source",
                                                                          "GWP20", "moisture_IPCC_default")],
                              value=("market", "perpres109", "food_separated_at_source"), description=L["scen"],
                              style=st, layout=W.Layout(width="520px", height="110px")),
    )
    pct = {c: W.FloatText(value=ex.get(f"pct_{c}") if pd.notna(ex.get(f"pct_{c}", float("nan"))) else None,
                          description=c, style={"description_width": "120px"}, layout=W.Layout(width="250px"))
           for c in RIPS_CATS}
    total = W.HTML()

    def upd(*_):
        s = sum(w.value for w in pct.values() if w.value is not None)
        total.value = f"<b>{L['total']}: {s:.2f}%</b>" + ("" if 90 <= s <= 110 else " &#9888;")
    for w in pct.values(): w.observe(upd, "value")
    upd()
    btn = W.Button(description=L["run"], button_style="primary", layout=W.Layout(width="220px"))
    out = W.Output()

    def run(_):
        with out:
            clear_output(); print(L["wait"])
            row = dict(city_id=f["name"].value.strip().lower().replace(" ", "_").replace(".", ""), city_name=f["name"].value,
                       grid_region=f["grid"].value, tonnage_tpd=f["q"].value, tonnage_year=f["qy"].value,
                       tonnage_is_2025="yes" if f["qy"].value >= 2025 else "no",
                       growth_rate=pd.to_numeric(f["g"].value, errors="coerce"), composition_year=f["cy"].value,
                       organik_lumped="yes" if f["lump"].value else "no", kiln_road_km=f["km"].value,
                       managed_share=pd.to_numeric(f["ms"].value, errors="coerce"),
                       managed_share_definition="user-entered" if f["ms"].value.strip() else "")
            for c, w in pct.items(): row[f"pct_{c}"] = w.value
            try:
                raw, warn = from_template(pd.DataFrame([row]), M)
            except ValueError as e:
                clear_output(); print(e); return
            A = analyse(M, raw, N=f["n"].value, scenarios=tuple(f["scen"].value), warnings=warn)
            clear_output()
            for x in warn: print("!", x)
            display(prob_figure(A, lang))
            display(A["prob"].round(2))
            display(A["best"].assign(scenario=A["best"].scenario.map(SCN_LABEL[lang])).round(1))
            display(central_table(A, lang))
            od = Path(out_dir); od.mkdir(parents=True, exist_ok=True)
            p1 = write_excel(A, od / f"{row['city_id']}_report.xlsx", lang)
            p2 = write_html(A, od / f"{row['city_id']}_report.html", lang)
            print(L["done"], p1, p2)
            try:
                from google.colab import files
                files.download(str(p1)); files.download(str(p2))
            except ImportError:
                pass
    btn.on_click(run)
    form = W.VBox([W.HTML(f"<h3>{L['name']}</h3>")] + list(f.values())[:9] +
                  [W.HTML(f"<b>{L['comp']}</b>"), W.GridBox(list(pct.values()), layout=W.Layout(grid_template_columns="repeat(3, 260px)")),
                   total, f["n"], f["scen"], btn, out])
    display(form)
    return form

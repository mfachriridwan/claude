"""Word document explaining the mathematical model: what every equation computes, what every symbol means, and how
each variable moves the results (direction, mechanism and size, from outputs/parameter_influence.csv).
Equations are native Word equations and are numbered as in MSW_Mathematical_Model_EN. Language: Indonesian, with the
English technical terms used in the article.  Run: python docs/make_math_explanation_docx.py"""
import ast
import subprocess
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

DOCS = Path(__file__).resolve().parent; ROOT = DOCS.parent; DATA, OUT = ROOT / "data", ROOT / "outputs"
import sys; sys.path.insert(0, str(DOCS))
from make_word_versions import PANDOC, reference_doc, postprocess

REG = pd.read_csv(DATA / "assumption_register.csv").set_index("key")
SP = pd.read_csv(DATA / "scenario_parameters.csv").set_index("key")
LC = pd.read_csv(DATA / "lcia_factors.csv").set_index("key")
K = pd.read_csv(DATA / "model_constants.csv").set_index("key")
T2 = pd.read_csv(DATA / "table2_model_fraction_parameters.csv").set_index("fraction")
INF = pd.read_csv(OUT / "parameter_influence.csv").set_index("key")
BASE = pd.read_csv(OUT / "parameter_influence_base.csv", index_col=0)
SOB = pd.read_csv(OUT / "discovery_sobol_mean.csv")
SOBG = SOB[SOB.output == "CICgap_S5_SL"].set_index("input").ST
PW = ["SL", "S1", "S2", "S3", "S4", "S5"]
NM = {"SL": "landfill", "S1": "WtE", "S2": "RDF", "S3": "AD", "S4": "PHB", "S5": "RDF + AD"}
GAP50 = float(pd.read_csv(OUT / "parameter_influence_base_gap.csv", index_col=0).value["gap50"])   # median over locations

# equations exactly as in the mathematical-model document (same order and numbers)
_src = (DOCS / "make_math_model_pdf.py").read_text()
_calls = []
class _V(ast.NodeVisitor):
    def visit_Call(self, n):
        if getattr(n.func, "id", "") == "E": _calls.append(n)
        self.generic_visit(n)
_V().visit(ast.parse(_src)); _calls.sort(key=lambda c: (c.lineno, c.col_offset))
EQ = {i: eval(compile(ast.Expression(c.args[0]), "eq", "eval"), {"gf": 1 - 20 / 35}) for i, c in enumerate(_calls, 1)}


def fmtv(x):
    if pd.isna(x): return "-"
    x = float(x)
    if abs(x) >= 1e5: return f"{x / 1e6:g} juta"
    return f"{x:.4g}"


def val(key):
    for src in (REG, SP, LC):
        if key in src.index:
            r = src.loc[key]
            rng = f" ({fmtv(r.low)}–{fmtv(r.high)})" if r.low != r.high else ""
            return f"{fmtv(r.central)}{rng} {r.unit if isinstance(r.unit, str) and r.unit != '-' else ''}".strip()
    if key in K.index:
        r = K.loc[key]; return f"{r.value:g} {r.unit if r.unit != '-' else ''}".strip()
    return key


def infl(key, opts=None, show_gap=True):
    """One sentence with the measured effect of `key` from its low to its high value."""
    if key not in INF.index: return ""
    r = INF.loc[key]; parts = []
    for k in (opts or PW):
        dg = r[f"G_{k}_hi"] - r[f"G_{k}_lo"]; dc = r[f"C_{k}_hi"] - r[f"C_{k}_lo"]
        bits = []
        if abs(dg) >= 0.5: bits.append(f"GRK {dg:+.0f} kg CO$_2$e/t")
        if abs(dc) >= 0.05: bits.append(f"biaya {dc:+.1f} USD/t")
        if bits: parts.append(f"{NM[k]}: " + ", ".join(bits))
    lo, hi = fmtv(r.low), fmtv(r.high)
    s = f"*Efek terukur* (batas bawah {lo} → batas atas {hi}, median 21 lokasi): " + ("; ".join(parts) if parts else "praktis tidak mengubah GRK/biaya") + "."
    if show_gap:
        s += (f" Selisih CIC (RDF + AD − landfill) pada 50 USD/t: {r.gap50_lo:+.1f} → {r.gap50_hi:+.1f} USD/t (dasar {GAP50:+.1f}; "
              f"negatif = RDF + AD lebih murah); lokasi yang pilihan terbaiknya berubah pada 100 USD/t: {int(r.n_change100_lo)} (bawah) / "
              f"{int(r.n_change100_hi)} (atas).")
    if key in SOBG.index:
        s += f" Indeks Sobol total untuk selisih tersebut: {SOBG[key]:.2f}."
    return s


md = []
A = md.append
eqn = lambda i: A(f"$${EQ[i]}\\qquad({i})$$")


def symtable(rows):
    lines = ["| Simbol | Arti | Satuan / nilai (tengah, rentang) | Kunci data |", "|------|--------------------------|-----------------|---------|"]
    for s, meaning, v, key in rows:
        lines.append(f"| ${s}$ | {meaning} | {v} | {('`' + key + '`') if key else '-'} |")
    A("\n".join(lines))


def block(i, what, syms, effects):
    A(f"### Persamaan ({i})")
    eqn(i)
    A(f"**Apa yang dihitung.** {what}")
    if syms: symtable(syms)
    if effects:
        A("**Bagaimana variabelnya memengaruhi hasil.**")
        for e in effects: A(f"- {e}")
    A("")


# ======================================================================== front matter
A('::: {custom-style="Title"}\nPenjelasan Model Matematis\n:::')
A('::: {custom-style="Subtitle"}\nArti setiap persamaan dan simbol, serta bagaimana tiap variabel memengaruhi hasil — pendamping MSW_Mathematical_Model_EN (nomor persamaan sama) — mswpath v3.2\n:::')
A('::: {custom-style="Subtitle"}\nMuhammad Fachri Ridwan — The University of Queensland — Oktober 2026\n:::')
A("# 0 Cara membaca dokumen ini")
A("Setiap persamaan ditulis sebagai persamaan Word (klik untuk mengedit) dengan nomor yang sama seperti di dokumen *Mathematical Model*. "
  "Di bawah setiap persamaan ada tiga bagian: **apa yang dihitung**, **tabel simbol** (arti, satuan, nilai tengah dan rentang dari "
  "`data/*.csv`, serta nama kunci datanya di kode), dan **bagaimana variabelnya memengaruhi hasil** (arah, mekanisme, dan besar efeknya).")
A("Konvensi yang dipakai di seluruh dokumen:")
for t in ["Semua hasil dinyatakan **per ton sampah campuran (basah) di gerbang fasilitas pada 2025** (unit fungsional). $G_k$ = emisi GRK "
          "opsi $k$ (kg CO$_2$e/t; makin kecil makin baik), $C_k$ = biaya bersih (USD/t; makin kecil makin baik).",
          "Tanda ↑ berarti variabel dinaikkan. \"Efek terukur\" dihitung dengan menjalankan model (file `run_parameter_influence.py`): satu "
          "variabel dipasang di batas bawah lalu batas atas rentangnya, variabel lain di nilai tengah, dan hasilnya diambil median 21 lokasi. "
          "Angka positif = naik dari batas bawah ke batas atas.",
          f"**Selisih CIC** = CIC(RDF + AD) − CIC(landfill) pada nilai karbon 50 USD/t CO$_2$e. Ini besaran kunci keputusan: nilai dasarnya "
          f"{GAP50:+.1f} USD/t (landfill lebih murah). Bila variabel membuat selisih ini negatif, RDF + AD menjadi pilihan.",
          "Indeks Sobol total ($S_T$) menyatakan porsi variansi selisih CIC yang disebabkan variabel tersebut (termasuk interaksinya)."]:
    A(f"- {t}")
A("")
A("\n".join(["| Opsi | GRK median (kg CO$_2$e/t) | Biaya median (USD/t) | CED fosil median (MJ/t) |", "|------|------|------|------|"]
             + [f"| {k} {NM[k]} | {BASE.loc[k, 'G']:.0f} | {BASE.loc[k, 'C']:.1f} | {BASE.loc[k, 'CED']:.0f} |" for k in PW]))
A('::: {custom-style="Caption"}\nTabel 0: Nilai dasar (nilai tengah, kasus pasar, median 21 lokasi) yang menjadi titik acuan semua \"efek terukur\".\n:::')


# ======================================================================== influence map figure
def influence_map():
    p = DOCS / "figures" / "math_influence_map.png"
    fig, ax = plt.subplots(figsize=(11, 7.2)); ax.set_xlim(0, 11); ax.set_ylim(0, 7.2); ax.axis("off")
    def bx(x, y, w, h, t, fc, ec, fs=7.6, b=False):
        ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.02,rounding_size=0.08", fc=fc, ec=ec, lw=0.9))
        ax.text(x + w / 2, y + h / 2, t, ha="center", va="center", fontsize=fs, weight="bold" if b else "normal")
    def ar(x1, y1, x2, y2, s="", c="#555"):
        ax.add_patch(FancyArrowPatch((x1, y1), (x2, y2), arrowstyle="-|>", mutation_scale=9, color=c, lw=0.8))
        if s: ax.text((x1 + x2) / 2, (y1 + y2) / 2 + 0.06, s, fontsize=7, color=c, ha="center", weight="bold")
    ax.text(1.2, 7.0, "INPUT (parameter)", ha="center", fontsize=9, weight="bold", color="#a07a3c")
    ax.text(5.0, 7.0, "BESARAN ANTARA", ha="center", fontsize=9, weight="bold", color="#2f5d8a")
    ax.text(8.2, 7.0, "HASIL", ha="center", fontsize=9, weight="bold", color="#3b8a43")
    ins = [("Komposisi $s_j$ (makanan, plastik)", 6.35), ("Kadar air $w_j$", 5.75), ("DOC, DOCf, $k_D$, MCF, F", 5.15),
           ("Penangkapan gas $\\eta$, oksidasi OX", 4.55), ("Nilai kalor $h_j$, $k_h$", 3.95), ("Lini RDF: $\\tau$, $\\omega$, $q_{dry}$, $\\psi$", 3.35),
           ("AD: $\\kappa$, $y_{CH_4}$, $k_{mech}$, $f_{AD}$", 2.75), ("Faktor grid EF, $k_{EF}$", 2.15),
           ("Harga: $p_{el}$, $p_{RDF}$", 1.55), ("Biaya: K, o, $c_{SL}$, r, n, b", 0.95), ("Nilai karbon p", 0.35)]
    for t, y in ins: bx(0.05, y - 0.22, 2.3, 0.44, t, "#f6efe3", "#a07a3c")
    mids = [("Kadar air curah M\nLHV H", 5.9), ("CH$_4$ landfill\n(dihasilkan, tertangkap, lepas)", 4.75),
            ("Listrik WtE $E_{el}$", 3.75), ("Energi & NCV RDF $e_{net}$", 2.95), ("Metana & listrik AD V, $E_{AD}$", 2.15),
            ("Biaya modal per ton $c^{cap}$", 1.2)]
    for t, y in mids: bx(3.6, y - 0.3, 2.8, 0.6, t, "#e8f0f8", "#2f5d8a")
    outs = [("$G_k$: GRK tiap opsi", 4.6), ("$C_k$: biaya tiap opsi", 2.6)]
    for t, y in outs: bx(7.1, y - 0.3, 2.2, 0.6, t, "#e9f5ea", "#3b8a43", b=True)
    bx(9.6, 3.3, 1.3, 0.6, "CIC = C + pG/1000", "#fbe9e7", "#b5523b", b=True)
    bx(9.6, 2.2, 1.3, 0.6, "Opsi terbaik\n& P(terbaik)", "#fbe9e7", "#b5523b", b=True)
    L = [(6.35, 5.9, "+"), (5.75, 5.9, ""), (5.15, 4.75, "+"), (4.55, 4.75, "−"), (3.95, 5.9, "+"), (5.75, 4.75, "−"),
         (3.35, 2.95, ""), (2.75, 2.15, "+"), (0.95, 1.2, "+")]
    for y1, y2, s in L: ar(2.35, y1, 3.6, y2, s)
    ar(6.4, 5.9, 7.1, 4.6); ar(6.4, 5.9, 6.95, 3.75 + 0.0); ar(6.4, 4.75, 7.1, 4.6, "+"); ar(6.4, 3.75, 7.1, 4.6, "−"); ar(6.4, 2.95, 7.1, 4.6, "−")
    ar(6.4, 2.15, 7.1, 4.6, "−"); ar(6.4, 3.75, 7.1, 2.6, "−"); ar(6.4, 2.95, 7.1, 2.6, "−"); ar(6.4, 2.15, 7.1, 2.6, "−"); ar(6.4, 1.2, 7.1, 2.6, "+")
    ar(2.35, 2.15, 7.1, 4.6, "", "#999"); ar(2.35, 1.55, 7.1, 2.6, "−", "#999"); ar(2.35, 0.95, 7.1, 2.6, "+", "#999")
    ar(9.3, 4.6, 9.6, 3.6); ar(9.3, 2.6, 9.6, 3.5); ar(2.35, 0.35, 9.6, 3.3, "", "#b5523b"); ar(10.25, 3.3, 10.25, 2.8)
    ax.text(5.5, 0.05, "Tanda pada panah: + = menaikkan besaran tujuan; − = menurunkannya. Panah abu-abu: pengaruh langsung ke hasil.",
            ha="center", fontsize=7.2, color="#555")
    fig.tight_layout(); fig.savefig(p, dpi=220); plt.close(fig); return p


A(f"![Gambar 0: Peta pengaruh — bagaimana kelompok input mengalir melalui besaran antara ke GRK, biaya, CIC dan pilihan terbaik.]({influence_map()}){{width=16cm}}")

# ======================================================================== sections
A("# 1 Harmonisasi data ke baseline 2025")
block(1, "Tonase tahun data $t$ diproyeksikan **sekali** ke 2025. Aliran domestik ($d$) dan non-domestik ($n$) memakai faktor yang sama, sehingga jumlahnya tetap sama dengan total.",
      [("Q_t", "Tonase yang dilaporkan RIPS untuk tahun $t$", "t/hari", ""), ("g", "Laju pertumbuhan tonase per tahun (laju RIPS; bila tidak ada, nilai bawaan)", val("g_default"), "g_default"),
       ("Q_{2025}", "Tonase 2025 yang dipakai model", "t/hari", "Q2025_tpd"), ("M^d, M^n", "Tonase aliran domestik dan non-domestik", "t/hari", "")],
      ["$g$ ↑ atau $t$ makin lama → $Q_{2025}$ ↑. **Tonase tidak mengubah GRK per ton**, tetapi memengaruhi **biaya per ton** lewat skala pabrik (pers. 10–11) dan **syarat kelayakan** (WtE ≥ 150 t/hari, tarif Perpres ≥ 1.000 t/hari, PHB ≥ 500 t/tahun).",
       "Proyeksi hanya sekali: model memeriksa bahwa tonase yang dipakai sama dengan nilai di database, jadi tidak mungkin terproyeksi dua kali."])
block(2, "Persen komposisi tiap aliran dinormalkan menjadi 100% (faktor normalisasi dicatat), lalu kedua aliran digabung dengan bobot tonase 2025 masing-masing.",
      [("x_j^{(\\ell)}", "Persen berat basah fraksi $j$ di aliran $\\ell$ (dari RIPS)", "%", "dom_*, nd_*"), ("s_j", "Pangsa fraksi $j$ dalam 1 ton sampah campuran", "t/t (jumlah = 1)", ""),
       ("f^{(\\ell)}", "Faktor normalisasi", "-", "")],
      ["**Komposisi adalah penggerak utama semua opsi**: pangsa makanan menaikkan metana landfill (pers. 7) dan umpan AD (pers. 19); pangsa plastik menaikkan CO$_2$ fosil WtE (pers. 6) dan nilai kalor (pers. 5); kertas dan kayu menaikkan keduanya.",
       "Kategori yang kosong di RIPS diberi pangsa nol (asumsi, bukan pengukuran); skenario *glass_imputed* mengujinya dan tidak mengubah keputusan."])
block(3, "Aturan harmonisasi untuk RIPS yang kategorinya tidak standar: bila sampah organik digabung (L), sebagian ($\\gamma$) dipindah ke kebun/daun; bila kayu ≥ 10% tanpa kategori daun (W), sebagian ($y$) dianggap sampah kebun.",
      [("\\gamma", "Pangsa sampah kebun dalam 'organik' gabungan", val("gamma"), "gamma"), ("y", "Pangsa kayu yang dianggap sampah kebun", val("yard"), "yard")],
      [f"$\\gamma$ ↑ → makanan ↓, kebun ↑. Sampah kebun lebih kering (kadar air {T2.loc['garden', 'moisture_as_received']:.2f} vs {T2.loc['food', 'moisture_as_received']:.2f}) dan DOC keringnya lebih tinggi ({T2.loc['garden', 'DOC_dry']:.2f} vs {T2.loc['food', 'DOC_dry']:.2f}), dengan DOCf sama, sehingga per ton basah membawa lebih banyak karbon terurai: metana landfill ↑, sedangkan umpan AD ↓ (AD hanya menerima makanan).",
       infl("gamma", ["SL", "S3", "S5"])])
block(4, "Hanya pada skenario *glass_imputed*: lokasi yang tidak melaporkan kaca diberi pangsa kaca $u_g$, dan fraksi lain dikecilkan sebanding.",
      [("u_g", "Pangsa kaca yang diimputasi", val("glass_imp_dist"), "glass_imp_dist")],
      ["$u_g$ ↑ → semua fraksi lain ↓ sedikit; kaca inert, jadi metana, nilai kalor dan CO$_2$ fosil turun sedikit. Efeknya kecil (kaca median 1,2%)."])

A("# 2 Karakterisasi sampah")
block(5, "Kadar air curah $M$ dan nilai kalor bawah saat diterima $H$ (LHV) dari campuran 10 fraksi. Air mengurangi bahan kering yang bisa terbakar dan menyerap panas penguapan $\\lambda$.",
      [("w_j", "Kadar air fraksi $j$ saat diterima (basah)", "makanan " + f"{T2.loc['food', 'moisture_as_received']:.2f} ({T2.loc['food', 'moisture_as_received_low']:.2f}–{T2.loc['food', 'moisture_as_received_high']:.2f})", "table2: moisture_as_received"),
       ("h_j", "LHV bahan kering fraksi $j$", "plastik " + f"{T2.loc['plastic', 'LHV_dry']:.1f} MJ/kg", "table2: LHV_dry"),
       ("k_h, k_{h,P}", "Faktor kalibrasi LHV (lama) dan pengali LHV plastik", f"{val('h_k')}; {val('h_plastic_k')}", "h_k, h_plastic_k"),
       ("\\lambda", "Panas laten penguapan air", val("LAM"), "LAM"), ("M, H", "Kadar air curah; LHV saat diterima", "-; MJ/kg", "")],
      ["**Kadar air adalah variabel kedua terpenting dalam keputusan.** $w_j$ ↑ → bahan kering ↓ → $H$ ↓ (dua kali: lebih sedikit yang terbakar dan lebih banyak panas untuk menguapkan air) → listrik WtE ↓, energi RDF ↓; sekaligus metana landfill ↓ karena DOC dihitung atas bahan kering. Jadi sampah basah melemahkan opsi energi **dan** melemahkan baseline landfill.",
       infl("wetness"), "$k_h$ ↑ → $H$ ↑ → WtE dan RDF lebih baik. " + infl("h_k", ["S1", "S2", "S5"], show_gap=False),
       "LHV ≥ 7 MJ/kg adalah syarat kelayakan WtE (G1)."])
block(6, "CO$_2$ fosil yang dilepas bila massa $m$ dibakar (WtE, atau RDF di kiln). Hanya karbon fosil (terutama plastik) yang dihitung; karbon biogenik netral.",
      [("C_j", "Kandungan karbon bahan kering", "plastik " + f"{T2.loc['plastic', 'carbon_dry']:.2f}", "table2: carbon_dry"),
       ("\\varphi_j", "Pangsa karbon yang fosil", "plastik " + f"{T2.loc['plastic', 'fossil_carbon_share']:.2f}; makanan 0", "table2: fossil_carbon_share"),
       ("44/12", "Konversi massa C → CO$_2$", "-", "")],
      ["Pangsa plastik ↑ → $E_{fos}$ ↑ → GRK WtE ↑ (pembakaran plastik adalah sumber emisi terbesar WtE). Pada RDF, plastik juga masuk ke RDF sehingga CO$_2$ fosilnya dihitung, tetapi diimbangi kredit batubara yang digantikan.",
       "Kadar air $w_j$ ↑ → bahan kering ↓ → $E_{fos}$ ↓ (per ton basah)."])

A("# 3 Modul landfill (open dump dan sanitary landfill)")
block(7, "Metana yang terbentuk di landfill dari karbon organik yang dapat terurai (metode IPCC, tanpa dinamika waktu).",
      [("DOC_j", "Karbon organik terurai, basis kering", "makanan " + f"{T2.loc['food', 'DOC_dry']:.2f}; kertas {T2.loc['paper', 'DOC_dry']:.2f}", "table2: DOC_dry"),
       ("DOCf_j", "Fraksi DOC yang benar-benar terurai", "makanan " + f"{T2.loc['food', 'DOCf_model']:.2f}; kayu {T2.loc['wood', 'DOCf_model']:.2f}", "table2: DOCf_model"),
       ("k_D", "Pengali ketidakpastian DOC·DOCf", val("doc_k"), "doc_k"), ("MCF", "Faktor koreksi metana (SL / open dump)", f"{val('mcf_sl')} / {val('mcf_od')}", "mcf_sl, mcf_od"),
       ("F", "Fraksi metana dalam gas landfill", val("F"), "F"), ("16/12", "Konversi C → CH$_4$", "-", "")],
      ["$k_D$, MCF, $F$, DOC, DOCf ↑ → metana ↑ → GRK landfill ↑. Ini menaikkan GRK **baseline** (open dump dan SL) dan residu yang ditimbun oleh opsi lain, sehingga **opsi yang mengalihkan organik dari landfill tampak lebih baik**.",
       infl("doc_k"), "MCF open dump ↑ → GRK open dump ↑ → semua opsi tampak lebih besar manfaatnya terhadap open dump, tetapi pilihan antaropsi tidak berubah."])
block(8, "Pada sanitary landfill, sebagian metana tertangkap ($\\eta$) dan dibakar di flare; sisanya lepas setelah sebagian teroksidasi di tanah penutup (OX). Open dump: tanpa penangkapan dan oksidasi.",
      [("\\eta", "Efisiensi penangkapan gas seumur landfill", val("cap"), "cap"), ("OX", "Oksidasi di tanah penutup", val("ox"), "ox"),
       ("[SL]", "Bernilai 1 untuk sanitary landfill, 0 untuk open dump", "-", "")],
      ["**$\\eta$ adalah variabel terpenting dalam keputusan.** $\\eta$ ↑ → metana lepas ↓ → GRK landfill ↓ → landfill makin sulit dikalahkan. Sebaliknya, landfill dengan penangkapan buruk membuat RDF + AD (dan opsi pengalihan lain) jauh lebih menarik.",
       infl("cap"), "$\\eta$ juga menentukan bahan baku PHB (pers. 22): $\\eta$ ↑ → produksi PHB ↑."])
block(9, "GRK landfill = metana lepas × GWP + emisi operasional (solar, material) sebanding dengan massa yang ditimbun.",
      [("GWP_{CH_4}", "Potensi pemanasan global metana (100 tahun, AR6)", val("GWP100_CH4"), "GWP100_CH4"),
       ("a_{LF}", "Emisi operasional landfill per ton", f"SL {val('anc_sl')}; OD {val('anc_od')}", "anc_sl, anc_od"), ("t", "Massa yang ditimbun (termasuk inert)", "t/t", "")],
      ["GWP ↑ (mis. GWP20 = 79,7) → semua emisi metana dihitung jauh lebih berat → landfill kalah; pada skenario GWP20 landfill tidak lagi terbaik pada 50 USD/t.",
       "$a_{LF}$ kecil dibanding metana; pengaruhnya minor."])
block(10, "Biaya landfill per ton. Biaya sanitary landfill turun bila landfill makin besar (skala ekonomi dengan elastisitas $e_{SL}$); open dump berbiaya tetap.",
      [("c_{SL}", "Biaya penuh sanitary landfill pada 500 t/hari", val("c_sl"), "c_sl"), ("e_{SL}", "Elastisitas skala biaya landfill", val("e_sl"), "e_sl"),
       ("c_{OD}", "Biaya open dump", val("c_od"), "c_od"), ("Q_{ref,SL}", "Kapasitas acuan", val("QREF_sl"), "QREF_sl")],
      ["$c_{SL}$ ↑ → biaya landfill ↑ **dan** biaya residu semua opsi lain ↑; karena landfill menimbun seluruh ton sedangkan opsi lain hanya residunya, efek bersihnya menguntungkan opsi pengalihan.",
       infl("c_sl"), "Tonase ↑ → biaya landfill per ton ↓ (skala)."])

A("# 4 Blok tekno-ekonomi")
block(11, "Biaya modal per ton: CAPEX acuan diskalakan ke kapasitas pabrik dengan eksponen $b$, dianualisasi dengan CRF, lalu dibagi throughput tahunan.",
      [("r", "Tingkat diskonto riil", val("r"), "r"), ("n", "Umur ekonomi", val("n"), "n"), ("b", "Eksponen skala CAPEX", val("b"), "b"),
       ("A", "Ketersediaan pabrik", val("AVAIL"), "AVAIL"), ("K_{ref}, q_{ref}", "CAPEX pada kapasitas acuan", "lihat pers. 14, 18, 21", "K_wte, K_rdf, K_ad"),
       ("q", "Throughput unit", "t/hari", "")],
      ["$r$ ↑ atau $n$ ↓ → CRF ↑ → biaya modal ↑ untuk semua pabrik; opsi padat modal (WtE, AD) paling terdampak. " + infl("r", ["S1", "S3", "S5"], show_gap=False),
       "Karena $c^{cap} \\propto q^{b-1}$ dan $b<1$, **pabrik lebih besar lebih murah per ton**; $b$ ↑ melemahkan skala ekonomi. " + infl("b", ["S1", "S5"], show_gap=False)])

A("# 5 Enam opsi")
block(12, "Sanitary landfill dengan flare = modul landfill diterapkan ke seluruh ton.", [], ["Dipengaruhi oleh semua variabel pers. 7–10; paling kuat oleh $\\eta$, kadar air, $k_D$ dan $c_{SL}$."])
block(13, "WtE: listrik bersih dari LHV dan efisiensi; GRK = CO$_2$ fosil + N$_2$O + bahan bantu − kredit listrik + timbunan abu.",
      [("\\eta_W", "Efisiensi listrik bersih insinerator", val("eta_wte"), "eta_wte"), ("n_{N_2O}", "Emisi N$_2$O", val("n2o_wte"), "n2o_wte"),
       ("a_W", "Bahan bakar bantu dan reagen gas buang", val("anc_wte"), "anc_wte"), ("\\alpha", "Abu yang ditimbun", val("ash"), "ash"),
       ("EF", "Faktor emisi grid × pengali", "Jamali 0,877; Sumatera 0,832; Mahakam 1,128 kg CO$_2$/kWh", "grid, efg_k")],
      ["$H$ ↑ atau $\\eta_W$ ↑ → $E_{el}$ ↑ → kredit listrik ↑ dan pendapatan ↑ → WtE lebih baik. " + infl("eta_wte", ["S1"], show_gap=False),
       "**EF ↑ → kredit listrik ↑ → GRK WtE ↓.** Inilah sebabnya WtE melemah pada proyeksi 2045 ketika grid makin bersih. " + infl("efg_k", ["S1", "S3", "S2"], show_gap=False),
       "Pangsa plastik ↑ → $E_{fos}$ ↑ → GRK WtE ↑, meski LHV juga ↑."])
block(14, "Biaya WtE = biaya modal + O&M − pendapatan listrik + biaya timbunan abu.",
      [("K_W", "CAPEX WtE pada 1.000 t/hari", val("K_wte"), "K_wte"), ("o_W", "O&M WtE", val("o_wte"), "o_wte"),
       ("p_{el}", "Nilai pasar listrik (atau tarif Perpres 0,20 USD/kWh)", val("p_el"), "p_el")],
      ["$p_{el}$ ↑ → biaya bersih WtE ↓; tarif Perpres 109/2025 (0,20 USD/kWh untuk ≥ 1.000 t/hari) adalah syarat utama WtE menang. " + infl("p_el", ["S1", "S3"], show_gap=False),
       "$K_W$, $o_W$ ↑ → biaya WtE ↑. " + infl("K_wte", ["S1"], show_gap=False)])
block(15, "Lini RDF memilah fraksi ke RDF dengan koefisien transfer $\\tau_j$, lalu mengeringkannya sampai kadar air target $\\omega$.",
      [("\\tau_j, k_\\tau", "Koefisien transfer ke RDF dan pengalinya", val("tau_k"), "table2: tau; tau_k"), ("\\omega", "Kadar air target RDF", val("omega"), "omega"),
       ("r_j", "Massa fraksi $j$ yang masuk RDF", "t/t", ""), ("m_{out}", "Massa RDF setelah dikeringkan", "t/t", "")],
      ["$k_\\tau$ ↑ → lebih banyak plastik/kertas masuk RDF → lebih banyak batubara digantikan, tetapi CO$_2$ fosil RDF juga ↑; residu ke landfill ↓. " + infl("tau_k", ["S2", "S5"]),
       "Kadar air ↑ → lebih banyak air harus diuapkan → $m_{out}$ dan energi bersih ↓."])
block(16, "Energi dalam RDF dikurangi panas pengeringan; $m_{del}$ = RDF yang dikirim setelah sebagian dibakar untuk mengeringkan.",
      [("q_{dry}", "Panas pengering per ton air", val("q_dry"), "q_dry"), ("k_{NCV}", "Pengali energi RDF (1; uji tekan 0,78)", val("rdf_ncv_k"), "rdf_ncv_k"),
       ("e, e_{net}", "Energi RDF kotor dan bersih", "GJ/t", "")],
      ["$q_{dry}$ ↑ → $e_{net}$ ↓ → kredit batubara dan pendapatan RDF ↓. " + infl("q_dry", ["S2", "S5"], show_gap=False),
       "Uji tekan $k_{NCV}$ = 0,78 (NCV ≈ 13–14 MJ/kg) membuat RDF + AD kalah di 17 dari 21 lokasi pada 100 USD/t: kualitas RDF menentukan."])
block(17, "GRK RDF = CO$_2$ fosil RDF + listrik dan solar pabrik + angkutan ke kiln − batubara yang digantikan + residu yang ditimbun. NCV ≥ 12,56 MJ/kg dan jarak ≤ 300 km adalah syarat kelayakan.",
      [("\\psi", "GJ batubara digantikan per GJ RDF", val("psi"), "psi"), ("EF_{coal}", "Faktor emisi batubara", val("ef_coal"), "ef_coal"),
       ("e_R", "Listrik pabrik RDF", val("e_rdf"), "e_rdf"), ("D", "Jarak jalan ke kiln terdekat", "km (jarak garis × tortuositas " + val("tort") + ")", "tort"),
       ("ef_{tr}", "Emisi truk", val("ef_truck"), "ef_truck")],
      ["$\\psi$ ↑ atau $EF_{coal}$ ↑ → kredit batubara ↑ → GRK RDF ↓. " + infl("psi", ["S2", "S5"]),
       "$D$ ↑ → emisi dan biaya angkut ↑; di atas 300 km RDF dianggap tidak layak.", "EF ↑ → listrik pabrik RDF lebih kotor → GRK RDF sedikit ↑ (efek kebalikan WtE)."])
block(18, "Biaya RDF = modal + O&M + angkutan − pendapatan dari kiln + biaya residu.",
      [("K_R", "CAPEX RDF pada 300 t/hari", val("K_rdf"), "K_rdf"), ("o_R", "O&M RDF", val("o_rdf"), "o_rdf"),
       ("p_{RDF}", "Harga RDF di kiln", val("p_rdf"), "p_rdf"), ("c_{tr}", "Biaya truk", val("c_truck"), "c_truck")],
      ["**$o_R$ adalah variabel ketiga terpenting.** $o_R$ ↑ → biaya RDF dan RDF + AD ↑. " + infl("o_rdf", ["S2", "S5"]),
       "$p_{RDF}$ ↑ → pendapatan ↑ → biaya ↓. " + infl("p_rdf", ["S2", "S5"])])
block(19, "AD: makanan yang tertangkap ($a$) diubah menjadi metana; untuk makanan yang dipilah dari sampah campuran, hasil metana dikali $k_{mech}$. Listrik = metana × LHV × efisiensi mesin, dikurangi kebocoran dan pemakaian sendiri.",
      [("\\kappa", "Pangsa makanan yang masuk digester", val("kappa"), "kappa"), ("VS/TS", "Padatan volatil / padatan total", val("vs_ts"), "vs_ts"),
       ("y_{CH_4}", "Hasil metana makanan terpilah di sumber", val("y_ch4"), "y_ch4"), ("k_{mech}", "Hasil relatif makanan dipilah mekanis", val("y_pen_mech"), "y_pen_mech"),
       ("f_{AD}", "Kebocoran metana", val("fug_ad"), "fug_ad"), ("\\eta_{CHP}", "Efisiensi listrik mesin biogas", val("eta_chp"), "eta_chp"),
       ("\\pi_{AD}", "Pemakaian listrik sendiri", val("par_ad"), "par_ad")],
      ["$y_{CH_4}$, $k_{mech}$, $\\eta_{CHP}$ ↑ → listrik AD ↑ → kredit dan pendapatan ↑. " + infl("y_pen_mech", ["S3", "S5"]),
       "$\\kappa$ ↑ → lebih banyak makanan keluar dari landfill (metana landfill ↓) dan lebih banyak listrik. " + infl("kappa", ["S3", "S5"], show_gap=False)])
block(20, "GRK AD = kebocoran metana × GWP + solar − kredit listrik + residu dan digestat yang ditimbun + bagian listrik/solar lini pemilah depan.",
      [("\\rho_{CH_4}", "Densitas metana", val("RHO_CH4"), "RHO_CH4"), ("a_A", "Solar pabrik AD per ton umpan", val("anc_ad"), "anc_ad"),
       ("\\delta", "Digestat yang ditimbun per ton umpan", val("dig"), "dig"), ("\\pi", "Porsi lini RDF yang dibebankan untuk memilah makanan", val("pre"), "pre")],
      ["$f_{AD}$ ↑ → GRK AD ↑ tajam (metana × 27). " + infl("fug_ad", ["S3", "S5"], show_gap=False),
       "Manfaat iklim utama AD bukan listriknya, melainkan **makanan tidak lagi membusuk di landfill**."])
block(21, "Biaya AD = modal + O&M + pra-olah per ton umpan − pendapatan listrik + residu + biaya lini pemilah depan.",
      [("K_A", "CAPEX AD pada 100 t/hari umpan", val("K_ad"), "K_ad"), ("o_A", "O&M AD per ton umpan", val("o_ad"), "o_ad"),
       ("c_{pre}", "Pra-olah tambahan makanan dipilah mekanis", val("pre_ofmsw"), "pre_ofmsw")],
      ["$K_A$, $o_A$ ↑ → biaya AD dan RDF + AD ↑. " + infl("K_ad", ["S3", "S5"]),
       "Bila makanan sudah terpilah di sumber, $\\pi$ = 0, $k_{mech}$ = 1 dan $c_{pre}$ = 0: AD menjadi pilihan terbaik di 20 lokasi pada 50 USD/t. **Pemilahan di sumber adalah syarat AD.**"])
block(22, "PHB: metana yang tertangkap di landfill diubah menjadi bioplastik; biaya produksi turun dengan skala.",
      [("R_{PHB}", "Metana per ton PHB", val("r_phb"), "r_phb"), ("c_{500}", "Biaya produksi pada 500 t/tahun", val("c_phb"), "c_phb"),
       ("e_{PHB}", "Elastisitas skala biaya PHB", val("e_phb"), "e_phb")],
      ["$\\eta$ ↑ → lebih banyak bahan baku PHB. $c_{500}$ ↓ → PHB lebih murah; hanya pada biaya skala besar yang aspiratif (1,3 USD/kg) PHB bisa menang."])
block(23, "GRK dan biaya PHB = landfill + (bahan kimia − polipropilena yang digantikan) dan (biaya − harga jual) per kg PHB.",
      [("ef_{PHB}", "Emisi bahan kimia PHB", val("ef_phb"), "ef_phb"), ("ef_{PP}", "Jejak karbon polipropilena", val("ef_pp"), "ef_pp"),
       ("\\sigma", "kg PP digantikan per kg PHB", val("sub_pp"), "sub_pp"), ("p_{PHB}", "Harga jual PHB", val("p_phb"), "p_phb")],
      ["Manfaat iklim PHB kecil (sekitar 14 kg CO$_2$e/t terhadap landfill) karena hanya memanfaatkan gas yang sudah tertangkap. " + infl("p_phb", ["S4"], show_gap=False)])
block(24, "RDF + AD: seluruh ton melewati lini RDF yang juga memisahkan makanan untuk digester; hanya penolakan dan digestat yang ditimbun.",
      [], ["Dipengaruhi gabungan variabel RDF (pers. 15–18) dan AD (pers. 19–21). Karena paling sedikit menimbun organik, RDF + AD paling diuntungkan bila landfill buruk ($\\eta$ rendah) dan paling dirugikan bila $o_R$, $K_A$ tinggi atau RDF berkualitas rendah."])

A("# 6 Energi fosil (CED) dan kebutuhan lahan")
block(25, "Faktor konversi: energi primer fosil per kWh grid (PEF) dan per kg CO$_2$e solar.",
      [("\\eta_{pp}", "Efisiensi pembangkit fosil", val("pef_eta"), "pef_eta"), ("u_{fuel}", "Tambahan rantai pasok bahan bakar", val("up_fuel"), "up_fuel"),
       ("ef_{diesel}", "Faktor emisi solar", val("ef_diesel"), "ef_diesel")],
      ["$\\eta_{pp}$ ↓ → PEF ↑ → penghematan energi dari listrik yang diekspor ↑ (WtE, AD). Grid lebih bersih ($k_{EF}$ ↓) → PEF ↓."])
block(26, "CED WtE dan RDF: konsumsi energi fosil dikurangi energi fosil yang dihemat (listrik grid, batubara kiln).", [],
      ["Listrik WtE ↑ → penghematan ↑. RDF menghemat batubara secara langsung, sehingga tidak bergantung pada grid."])
block(27, "CED AD dan PHB; CED landfill hanya dari solar operasional.", [], ["PHB menghemat energi melalui polipropilena yang digantikan."])
block(28, "Lahan: volume timbunan dibagi densitas × tinggi landfill, dikali faktor luas kotor; pabrik: jejak lahan dibagi throughput seumur hidup.",
      [("\\rho, H", "Densitas timbunan dan tinggi landfill", f"SL {val('rho_sl')}, {val('h_sl')}", "rho_sl, h_sl"), ("f_{gross}", "Faktor luas kotor", val("f_gross_sl"), "f_gross_sl"),
       ("FP", "Jejak lahan pabrik", "m$^2$", "fp_*")],
      ["Makin sedikit yang ditimbun → lahan ↓; WtE paling hemat lahan (hanya abu)."])

A("# 7 Syarat kelayakan (G1–G6)")
A("Bukan persamaan bernomor, tetapi menentukan opsi mana yang boleh dipilih: WtE perlu LHV ≥ 7 MJ/kg dan ≥ 150 t/hari; tarif Perpres perlu ≥ 1.000 t/hari; RDF dan RDF + AD perlu NCV ≥ 12,56 MJ/kg dan kiln ≤ 300 km; PHB perlu ≥ 500 t/tahun. Variabel yang menggeser opsi melewati ambang (kadar air, tonase, jarak kiln) bisa membuat opsi tiba-tiba layak atau tidak layak, sehingga pengaruhnya tidak mulus.")

A("# 8 Analisis keputusan")
block(29, "Manfaat dan biaya tambahan terhadap baseline (open dump atau landfill), serta biaya abatemen (USD per t CO$_2$e dihindari).", [],
      ["Biaya abatemen RDF + AD terhadap landfill (median sekitar 73 USD/t CO$_2$e) adalah **nilai karbon minimum** agar RDF + AD layak dipilih; semua variabel yang menurunkan biaya atau menaikkan manfaatnya akan menurunkan angka ini."])
block(30, "Biaya dengan valuasi karbon (CIC): biaya + nilai karbon × GRK; opsi terbaik = CIC terkecil di antara opsi yang layak.",
      [("p", "Nilai karbon (pilihan kebijakan)", "0, 2, 25, 50, 100 USD/t CO$_2$e", ""), ("\\mathcal{F}_i", "Opsi yang lolos syarat pada iterasi $i$", "-", "")],
      ["$p$ ↑ → GRK makin mahal → opsi rendah emisi (RDF + AD, WtE) makin kompetitif. Dua opsi bertukar posisi pada $p^* = 10^3(C_b-C_a)/(G_a-G_b)$.",
       "Variabel yang menurunkan GRK opsi $k$ berpengaruh lebih besar pada $p$ tinggi; variabel biaya berpengaruh sama pada semua $p$."])
block(31, "Opsi efisien Pareto: tidak ada opsi lain yang sekaligus lebih murah dan lebih rendah emisi.", [],
      ["Landfill dan RDF + AD hampir selalu di frontier Pareto; nilai karbon yang memilih di antara keduanya."])
block(32, "Peluang menjadi terbaik = proporsi iterasi Monte Carlo di mana opsi menang, beserta galat bakunya.",
      [("N", "Jumlah iterasi Monte Carlo", val("N_MC"), "N_MC")],
      ["$N$ ↑ → galat baku ↓ (≤ 0,008 pada N = 4.000). Selisih peluang yang kecil berarti keputusan bergantung pada input yang tidak pasti, bukan derau."])

A("# 9 Ketidakpastian")
block(33, "Distribusi triangular: setiap parameter diambil acak antara batas bawah $a$, nilai tengah $c$ dan batas atas $b$.", [],
      ["Rentang yang lebar → hasil lebih menyebar → peluang terbaik lebih jauh dari 0 atau 1. Rentang tiap parameter ada di tabel simbol di atas."])
block(34, "Komposisi diambil acak dari distribusi Dirichlet di sekitar komposisi RIPS; konsentrasi $\\alpha_0$ mengatur seberapa jauh boleh menyimpang.",
      [("\\alpha_0", "Konsentrasi Dirichlet (heuristik)", f"{val('alpha_hi')} (data baik) / {val('alpha_lo')} (data ditandai)", "alpha_hi, alpha_lo"),
       ("\\epsilon", "Pseudocount (skenario)", val("pseudo_scen"), "pseudo_scen")],
      ["$\\alpha_0$ ↓ → komposisi lebih bervariasi → GRK WtE (melalui plastik) lebih tidak pasti."])

A("# 10 Analisis sensitivitas")
block(35, "Korelasi peringkat Spearman antara input dan hasil di sampel Monte Carlo.", [], ["$|\\rho_S|$ besar = input itu banyak menentukan hasil; tanda menunjukkan arah."])
block(36, "Indeks Sobol: $S_1$ = efek input sendiri; $S_T$ = efek total termasuk interaksi.", [],
      ["Untuk selisih CIC RDF + AD − landfill, peringkat $S_T$ teratas adalah: " + ", ".join(f"{k} ({v:.2f})" for k, v in SOBG.sort_values(ascending=False).head(6).items()) + "."])

A("# 11 Scenario discovery")
block(37, "Konsentrasi Dirichlet untuk komposisi kota hipotetis, diestimasi dari sebaran 21 komposisi RIPS.", [], ["Menentukan seberapa beragam komposisi yang diuji dalam 40.000 kondisi."])
block(38, "Pohon keputusan CART memilih pemisahan kondisi (mis. nilai karbon > x) yang paling mengurangi ketidakmurnian Gini.", [],
      ["Variabel yang sering dipakai di pemisahan atas (nilai karbon, pemilahan di sumber, skala, LHV) adalah penentu utama pilihan."])
block(39, "Kotak PRIM: rentang kondisi di mana satu opsi menang dengan kepadatan tinggi.", [], ["Kepadatan tinggi dan cakupan tinggi = aturan keputusan yang kuat."])

A("# 12 Nilai informasi")
block(40, "EVPI: nilai maksimum yang layak dibayar (per ton) untuk menghilangkan semua ketidakpastian sebelum memilih.", [],
      ["EVPI naik dengan nilai karbon (0,24 → 1,05 → 3,18 USD/t pada 25, 50 dan 100), karena keputusan makin bergantung pada input yang tidak pasti."])
block(41, "EVPPI: nilai informasi untuk satu kelompok input (satu kampanye pengukuran).", [],
      ["Kelompok dengan EVPPI tertinggi (karakterisasi sampah, kinerja gas landfill) adalah data yang harus diukur lebih dulu. Dikalikan tonase tahunan, hasilnya USD per tahun."])
block(42, "Target impas: nilai satu input yang membuat CIC opsi sama dengan landfill.", [],
      ["Menerjemahkan parameter menjadi syarat kontrak/tender, mis. biaya O&M RDF maksimum atau harga RDF minimum."])

A("# 13 Proyeksi 2045")
block(43, "Faktor emisi grid turun linear ke nol pada 2060 ($\\delta$ = 1/35 per tahun); pada 2045 tinggal 0,429 × nilai 2025.",
      [("\\delta", "Laju penurunan faktor grid", "1/35 per tahun", "delta_grid")],
      ["EF ↓ → kredit listrik WtE dan AD ↓ → GRK WtE naik ~177 kg CO$_2$e/t pada 2045; GRK RDF turun karena listrik prosesnya lebih bersih."])
block(44, "Eskalasi riil harga listrik (2%/tahun), O&M (1%/tahun) dan biaya landfill (2%/tahun).", [],
      ["$p_{el}$ ↑ → WtE lebih murah; O&M ↑ → semua pabrik lebih mahal; biaya landfill ↑ → pengalihan lebih menarik."])
block(45, "Tonase tumbuh dengan laju tiap lokasi; perubahan total dipecah per pemicu (grid, biaya, tonase) dan interaksinya.", [],
      ["Tonase ↑ → biaya per ton ↓ (skala) dan lebih banyak lokasi lolos ambang 1.000 t/hari."])

# ======================================================================== ranking table
A("# 14 Peringkat pengaruh semua parameter")
A("Tabel berikut mengurutkan semua parameter menurut besarnya ayunan selisih CIC (RDF + AD − landfill) pada 50 USD/t ketika parameter digerakkan dari batas bawah ke batas atas. Kolom GRK dan biaya: perubahan median (atas − bawah).")
rank = INF.sort_values("swing_gap50", ascending=False)
tl = ["| Parameter | Arti | Rentang | ΔG landfill | ΔG WtE | ΔG RDF+AD | ΔC landfill | ΔC WtE | ΔC RDF+AD | Selisih CIC 50: bawah → atas | Lokasi berubah (100) |",
      "|---|------------|---|---|---|---|---|---|---|---|---|"]
for k, r in rank.iterrows():
    d = lambda c: r[f"{c}_hi"] - r[f"{c}_lo"]
    tl.append(f"| `{k}` | {str(r.meaning)[:70]} | {fmtv(r.low)}–{fmtv(r.high)} | {d('G_SL'):+.0f} | {d('G_S1'):+.0f} | {d('G_S5'):+.0f} | "
              f"{d('C_SL'):+.1f} | {d('C_S1'):+.1f} | {d('C_S5'):+.1f} | {r.gap50_lo:+.1f} → {r.gap50_hi:+.1f} | {int(max(r.n_change100_lo, r.n_change100_hi))} |")
A("\n".join(tl))
A('::: {custom-style="Caption"}\nTabel 14: Pengaruh satu-per-satu (nilai tengah, kasus pasar, median 21 lokasi; outputs/parameter_influence.csv). ΔG dalam kg CO$_2$e/t, ΔC dalam USD/t.\n:::')
top5 = ", ".join(f"`{k}`" for k in rank.index[:5])
A("# 15 Ringkasan: variabel yang paling menentukan keputusan")
for t in [f"Lima variabel dengan pengaruh terbesar terhadap pilihan antara landfill dan RDF + AD: {top5}.",
          "**Penangkapan gas landfill ($\\eta$)** dan **kadar air sampah ($w$)** menentukan seberapa buruk baseline landfill dan seberapa banyak energi yang bisa dipulihkan — keduanya belum diukur di lokasi studi, karena itu menjadi prioritas pengukuran (nilai informasi tertinggi).",
          "**Biaya O&M RDF, CAPEX AD dan harga RDF** menentukan biaya RDF + AD dan menjadi target dalam tender/kontrak offtake.",
          "**Faktor grid** menentukan manfaat iklim WtE dan AD; karena grid akan makin bersih, manfaat itu menyusut menuju 2045.",
          "**Nilai karbon $p$** bukan parameter tidak pasti melainkan pilihan kebijakan: di bawah ~50 USD/t landfill menang; mendekati 100 USD/t RDF + AD menang."]:
    A(f"- {t}")

# ======================================================================== build
src = DOCS / "word_build" / "MSW_Mathematical_Model_Explained_ID.md"; src.parent.mkdir(exist_ok=True)
src.write_text("---\nlang: id-ID\n---\n\n" + "\n\n".join(md), encoding="utf-8")
out = DOCS / "MSW_Mathematical_Model_Explained_ID.docx"
r = subprocess.run([PANDOC, str(src), "-f", "markdown+tex_math_dollars+pipe_tables", "-o", str(out), "--reference-doc",
                    str(reference_doc()), "--columns=40"], capture_output=True, text=True)
if r.returncode: raise RuntimeError(r.stderr)
postprocess(out)
print("wrote", out, r.stderr[:500])

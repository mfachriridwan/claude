"""Dashboard Paper -> Life Cycle Inventory (LCI).

Upload paper (PDF/DOCX/HTML/TXT) atau beri URL, lalu ekstrak kandidat data
LCI: functional unit + aliran (energi, material, emisi, air, limbah, dll.)
beserta jumlah & satuan, dengan grafik per kategori dan ekspor CSV/JSON.

Jalankan dari folder research_assistant:
    streamlit run dashboard/lci_app.py
atau:
    python riset.py dashboard --lci
"""

from __future__ import annotations

import io
import json
import os
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import streamlit as st  # noqa: E402

from doknlp import load_document, extract_lci, LCI_CSV_FIELDS  # noqa: E402

st.set_page_config(page_title="Paper → LCI", page_icon="♻️", layout="wide")

st.title("♻️ Paper → Life Cycle Inventory")
st.caption(
    "Ekstrak kandidat data LCI dari paper. Hasil rule-based adalah KANDIDAT "
    "untuk diverifikasi — gunakan mode Claude untuk hasil yang lebih akurat."
)

CATEGORY_LABEL = {
    "energi": "⚡ Energi",
    "material": "🧱 Material",
    "emisi_udara": "💨 Emisi Udara",
    "emisi_air": "💧 Emisi Air",
    "air": "🚰 Air",
    "limbah": "🗑️ Limbah",
    "transport": "🚚 Transport",
    "lahan": "🌍 Lahan",
    "lainnya": "❓ Lainnya",
}


def _load_upload(upload) -> str:
    suffix = os.path.splitext(upload.name)[1] or ".txt"
    with tempfile.NamedTemporaryFile(delete=False, suffix=suffix) as tmp:
        tmp.write(upload.getbuffer())
        tmp_path = tmp.name
    try:
        return load_document(tmp_path)
    finally:
        os.unlink(tmp_path)


with st.sidebar:
    st.header("Sumber Paper")
    uploaded = st.file_uploader(
        "Upload paper", type=["pdf", "docx", "html", "htm", "txt", "md"]
    )
    url = st.text_input("...atau URL paper (http/https)")
    pasted = st.text_area("...atau tempel teks paper", height=160)
    st.divider()
    has_key = bool(os.environ.get("ANTHROPIC_API_KEY"))
    use_llm = st.toggle(
        "Ekstraksi via Claude (lebih akurat)",
        value=has_key,
        disabled=not has_key,
        help="Butuh ANTHROPIC_API_KEY di environment." if not has_key else None,
    )
    if not has_key:
        st.caption("💡 Set `ANTHROPIC_API_KEY` untuk mengaktifkan mode Claude.")

text = ""
try:
    if uploaded is not None:
        text = _load_upload(uploaded)
    elif url.strip():
        with st.spinner("Mengunduh paper..."):
            text = load_document(url.strip())
    elif pasted.strip():
        text = pasted
except (RuntimeError, FileNotFoundError) as exc:
    st.error(f"Gagal memuat paper: {exc}")
    st.stop()

if not text.strip():
    st.info("⬅️ Pilih sumber paper di sidebar untuk mulai.")
    st.stop()

with st.spinner("Mengekstrak data LCI..."):
    result = extract_lci(text, use_llm=use_llm)

flows = result["flows"]

# --- Header info ---
c1, c2, c3 = st.columns(3)
c1.metric("Total flow", result["jumlah_flow"])
c2.metric("Kategori", len(result["ringkasan_kategori"]))
c3.metric("Metode", result["metode"])

st.markdown(f"**Functional unit:** {result['functional_unit'] or '_(tidak terdeteksi — set manual saat memakai data ini)_'}")
if result.get("system_boundary"):
    st.markdown(f"**System boundary:** {result['system_boundary']}")

if not flows:
    st.warning("Tidak ada data kuantitatif LCI yang terdeteksi pada paper ini.")
    st.stop()

# --- Grafik per kategori ---
st.subheader("Jumlah flow per kategori")
ringkasan = {CATEGORY_LABEL.get(k, k): v
             for k, v in sorted(result["ringkasan_kategori"].items(),
                                key=lambda x: x[1], reverse=True)}
st.bar_chart(ringkasan)

# --- Tabel flows (filter per kategori) ---
st.subheader("Tabel Life Cycle Inventory")
kategori_tersedia = sorted({f["kategori"] for f in flows})
pilih = st.multiselect(
    "Filter kategori",
    options=kategori_tersedia,
    default=kategori_tersedia,
    format_func=lambda k: CATEGORY_LABEL.get(k, k),
)
tampil = [f for f in flows if f["kategori"] in pilih]
tabel = [{k: f.get(k, "") for k in LCI_CSV_FIELDS} for f in tampil]
st.dataframe(tabel, use_container_width=True, hide_index=True)

# --- Ekspor ---
st.subheader("Ekspor")
col_csv, col_json = st.columns(2)

import csv as _csv

buf = io.StringIO()
writer = _csv.DictWriter(buf, fieldnames=LCI_CSV_FIELDS, extrasaction="ignore")
writer.writeheader()
for f in flows:
    writer.writerow({k: f.get(k, "") for k in LCI_CSV_FIELDS})
col_csv.download_button("⬇️ Unduh CSV", buf.getvalue(),
                        file_name="lci.csv", mime="text/csv")

col_json.download_button(
    "⬇️ Unduh JSON",
    json.dumps(result, ensure_ascii=False, indent=2),
    file_name="lci.json",
    mime="application/json",
)

st.caption(
    "⚠️ Data ini hasil ekstraksi otomatis. Selalu verifikasi nilai & satuan "
    "terhadap paper asli sebelum dipakai dalam studi LCA."
)

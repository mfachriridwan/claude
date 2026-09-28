"""Dashboard Streamlit untuk asisten analisis dokumen.

Jalankan dari folder research_assistant:
    streamlit run dashboard/app.py
atau:
    python riset.py dashboard
"""

from __future__ import annotations

import os
import sys
import tempfile

# Pastikan paket doknlp bisa diimpor saat dijalankan via `streamlit run`.
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import streamlit as st  # noqa: E402

from doknlp import (  # noqa: E402
    load_document,
    document_stats,
    summarize,
    extract_keywords,
    sentiment,
    topics,
    answer_question,
    analyze_documents,
)

st.set_page_config(page_title="Riset - Asisten Dokumen", page_icon="📄", layout="wide")

st.title("📄 Riset — Asisten Analisis Dokumen")
st.caption("Upload dokumen (.txt/.md/.html/.docx/.pdf), tempel teks, atau beri URL.")


def _load_upload(upload) -> str:
    """Simpan file upload ke temp lalu muat via loader (mendukung docx/pdf/html)."""
    suffix = os.path.splitext(upload.name)[1] or ".txt"
    with tempfile.NamedTemporaryFile(delete=False, suffix=suffix) as tmp:
        tmp.write(upload.getbuffer())
        tmp_path = tmp.name
    try:
        return load_document(tmp_path)
    finally:
        os.unlink(tmp_path)


def render_wordcloud(freqs: dict) -> bool:
    """Render wordcloud bila library tersedia. Return True kalau berhasil."""
    try:
        import matplotlib.pyplot as plt
        from wordcloud import WordCloud
    except ImportError:
        return False

    wc = WordCloud(width=800, height=400, background_color="white")
    wc.generate_from_frequencies(freqs)
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.imshow(wc, interpolation="bilinear")
    ax.axis("off")
    st.pyplot(fig)
    plt.close(fig)
    return True


with st.sidebar:
    st.header("Sumber Dokumen")
    uploaded = st.file_uploader(
        "Upload file", type=["txt", "md", "text", "html", "htm", "docx", "pdf"]
    )
    url = st.text_input("...atau tempel URL (http/https)")
    pasted = st.text_area("...atau tempel teks", height=160)
    st.divider()
    n_summary = st.slider("Jumlah kalimat ringkasan", 1, 15, 5)
    n_keywords = st.slider("Jumlah kata kunci", 5, 40, 15)
    n_topics = st.slider("Jumlah topik", 2, 10, 5)

text = ""
try:
    if uploaded is not None:
        text = _load_upload(uploaded)
    elif url.strip():
        with st.spinner("Mengunduh URL..."):
            text = load_document(url.strip())
    elif pasted.strip():
        text = pasted
except (RuntimeError, FileNotFoundError) as exc:
    st.error(f"Gagal memuat dokumen: {exc}")
    st.stop()

if not text.strip():
    st.info("⬅️ Pilih sumber dokumen di sidebar untuk mulai.")
    st.stop()

stats = document_stats(text)
sent = sentiment(text)

# --- Metrik ringkas ---
c1, c2, c3, c4 = st.columns(4)
c1.metric("Kata", stats["kata"])
c2.metric("Kalimat", stats["kalimat"])
c3.metric("Waktu baca (mnt)", stats["estimasi_waktu_baca_menit"])
c4.metric("Sentimen", sent["label"], delta=sent["polaritas"])

tab_sum, tab_kw, tab_topik, tab_qa, tab_batch = st.tabs(
    ["📝 Ringkasan", "🔑 Kata Kunci", "🧩 Topik", "💬 Tanya-Jawab", "📚 Batch"]
)

with tab_sum:
    for i, s in enumerate(summarize(text, n_sentences=n_summary), 1):
        st.markdown(f"**{i}.** {s}")

with tab_kw:
    kws = extract_keywords(text, n=n_keywords)
    freqs = {w: s for w, s in kws}
    if not render_wordcloud(freqs):
        st.caption("💡 Pasang `wordcloud` + `matplotlib` untuk visualisasi awan kata.")
    st.bar_chart(freqs)
    st.write(", ".join(f"`{w}`" for w, _ in kws))

with tab_topik:
    tp = topics(text, n_topics=n_topics)
    for i, topic in enumerate(tp, 1):
        st.markdown(f"**Topik {i}:** {', '.join(topic)}")
        # Grafik bobot: kata teratas diberi bobot menurun sesuai peringkat.
        weights = {w: len(topic) - j for j, w in enumerate(topic)}
        st.bar_chart(weights)

with tab_qa:
    q = st.text_input("Tanyakan sesuatu tentang dokumen ini")
    use_llm = st.checkbox(
        "Sintesis jawaban via Claude (butuh ANTHROPIC_API_KEY)", value=False
    )
    if q:
        result = answer_question(text, q, use_llm=use_llm)
        st.markdown(f"**Jawaban:** {result['jawaban']}")
        if result["konteks"]:
            with st.expander("Kalimat sumber"):
                for c in result["konteks"]:
                    st.markdown(f"- {c}")

with tab_batch:
    st.markdown(
        "Analisis banyak dokumen sekaligus. Masukkan path file/folder/URL "
        "(satu per baris), lalu unduh hasilnya sebagai CSV."
    )
    sources_raw = st.text_area("Sumber (satu per baris)", height=120,
                               key="batch_sources")
    if st.button("Jalankan batch") and sources_raw.strip():
        sources = [s.strip() for s in sources_raw.splitlines() if s.strip()]
        with st.spinner(f"Menganalisis {len(sources)} sumber..."):
            rows = analyze_documents(sources)
        if rows:
            st.dataframe(rows, use_container_width=True)
            import csv as _csv
            import io

            buf = io.StringIO()
            writer = _csv.DictWriter(buf, fieldnames=list(rows[0].keys()))
            writer.writeheader()
            writer.writerows(rows)
            st.download_button("⬇️ Unduh CSV", buf.getvalue(),
                               file_name="hasil_batch.csv", mime="text/csv")

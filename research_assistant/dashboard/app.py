"""Dashboard Streamlit untuk asisten analisis dokumen.

Jalankan dari folder research_assistant:
    streamlit run dashboard/app.py
atau:
    python riset.py dashboard
"""

from __future__ import annotations

import os
import sys

# Pastikan paket doknlp bisa diimpor saat dijalankan via `streamlit run`.
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import streamlit as st  # noqa: E402

from doknlp import (  # noqa: E402
    document_stats,
    summarize,
    extract_keywords,
    sentiment,
    topics,
    answer_question,
)

st.set_page_config(page_title="Riset - Asisten Dokumen", page_icon="📄", layout="wide")

st.title("📄 Riset — Asisten Analisis Dokumen")
st.caption("Upload dokumen (.txt/.md) atau tempel teks, lalu jelajahi analisisnya.")

with st.sidebar:
    st.header("Sumber Dokumen")
    uploaded = st.file_uploader("Upload file teks", type=["txt", "md", "text"])
    pasted = st.text_area("...atau tempel teks di sini", height=200)
    n_summary = st.slider("Jumlah kalimat ringkasan", 1, 15, 5)
    n_keywords = st.slider("Jumlah kata kunci", 5, 40, 15)
    n_topics = st.slider("Jumlah topik", 2, 10, 5)

text = ""
if uploaded is not None:
    text = uploaded.read().decode("utf-8", errors="ignore")
elif pasted.strip():
    text = pasted

if not text.strip():
    st.info("⬅️ Upload file atau tempel teks di sidebar untuk mulai.")
    st.stop()

stats = document_stats(text)
sent = sentiment(text)

# --- Metrik ringkas ---
c1, c2, c3, c4 = st.columns(4)
c1.metric("Kata", stats["kata"])
c2.metric("Kalimat", stats["kalimat"])
c3.metric("Waktu baca (mnt)", stats["estimasi_waktu_baca_menit"])
c4.metric("Sentimen", sent["label"], delta=sent["polaritas"])

tab_sum, tab_kw, tab_topik, tab_qa = st.tabs(
    ["📝 Ringkasan", "🔑 Kata Kunci", "🧩 Topik", "💬 Tanya-Jawab"]
)

with tab_sum:
    for i, s in enumerate(summarize(text, n_sentences=n_summary), 1):
        st.markdown(f"**{i}.** {s}")

with tab_kw:
    kws = extract_keywords(text, n=n_keywords)
    st.bar_chart({w: s for w, s in kws})
    st.write(", ".join(f"`{w}`" for w, _ in kws))

with tab_topik:
    for i, topic in enumerate(topics(text, n_topics=n_topics), 1):
        st.markdown(f"**Topik {i}:** {', '.join(topic)}")

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

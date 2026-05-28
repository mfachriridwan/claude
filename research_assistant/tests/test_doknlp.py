"""Tes untuk core NLP. Hanya butuh stdlib + pytest."""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from doknlp import (  # noqa: E402
    document_stats,
    summarize,
    extract_keywords,
    sentiment,
    topics,
    answer_question,
    load_document,
    build_report,
)
from doknlp.preprocess import split_sentences, tokenize_words, content_words  # noqa: E402

SAMPLE = (
    "Optimasi lokasi kontainer sangat penting untuk efisiensi pelabuhan. "
    "Penempatan kontainer yang baik mengurangi waktu bongkar muat. "
    "Model optimasi membantu menentukan lokasi kontainer terbaik. "
    "Pelabuhan modern menggunakan algoritma untuk efisiensi. "
    "Sistem yang buruk menyebabkan keterlambatan dan kerugian besar. "
    "Penelitian ini menunjukkan peningkatan efisiensi yang signifikan."
)


def test_split_sentences():
    assert len(split_sentences(SAMPLE)) == 6
    assert split_sentences("") == []


def test_tokenize_and_content_words():
    assert "optimasi" in tokenize_words(SAMPLE)
    cw = content_words(SAMPLE)
    assert "yang" not in cw  # stopword dibuang
    assert "di" not in cw    # terlalu pendek / stopword


def test_document_stats():
    stats = document_stats(SAMPLE)
    assert stats["kalimat"] == 6
    assert stats["kata"] > 0
    assert 0 <= stats["keberagaman_leksikal"] <= 1


def test_document_stats_empty():
    stats = document_stats("")
    assert stats["kata"] == 0
    assert stats["rata_kata_per_kalimat"] == 0.0


def test_summarize_count_and_order():
    out = summarize(SAMPLE, n_sentences=3)
    assert len(out) == 3
    # Urutan harus mengikuti urutan asli di dokumen.
    idxs = [SAMPLE.index(s[:20]) for s in out]
    assert idxs == sorted(idxs)


def test_summarize_short_doc_returns_all():
    short = "Satu kalimat saja."
    assert summarize(short, n_sentences=5) == ["Satu kalimat saja."]


def test_extract_keywords():
    kws = extract_keywords(SAMPLE, n=5)
    assert len(kws) == 5
    words = [w for w, _ in kws]
    # 'kontainer'/'efisiensi'/'optimasi' adalah tema utama -> harus muncul.
    assert any(w in words for w in ("kontainer", "efisiensi", "optimasi"))
    # Skor terurut menurun.
    scores = [s for _, s in kws]
    assert scores == sorted(scores, reverse=True)


def test_sentiment_positive():
    s = sentiment("Hasilnya sangat baik, bagus, dan sukses meningkat.")
    assert s["label"] == "positif"
    assert s["polaritas"] > 0


def test_sentiment_negative():
    s = sentiment("Sistemnya buruk, gagal, dan menyebabkan kerugian serta masalah.")
    assert s["label"] == "negatif"
    assert s["polaritas"] < 0


def test_sentiment_neutral():
    s = sentiment("Kontainer berada di lokasi pelabuhan nomor tujuh.")
    assert s["label"] == "netral"


def test_topics():
    tp = topics(SAMPLE, n_topics=2, n_words=4)
    assert len(tp) >= 1
    assert all(isinstance(t, list) and t for t in tp)


def test_answer_question_extractive():
    res = answer_question(SAMPLE, "kenapa optimasi lokasi kontainer penting?")
    assert res["sumber_llm"] is False
    assert res["konteks"]  # ada kalimat relevan
    assert "kontainer" in res["jawaban"].lower()


def test_answer_question_no_match():
    res = answer_question(SAMPLE, "zxqwv unrelated gibberish term")
    assert "Tidak ditemukan" in res["jawaban"] or res["konteks"] == []


def test_load_document(tmp_path):
    p = tmp_path / "doc.txt"
    p.write_text(SAMPLE, encoding="utf-8")
    assert load_document(str(p)) == SAMPLE


def test_load_missing_file():
    import pytest

    with pytest.raises(FileNotFoundError):
        load_document("/tidak/ada/file.txt")


def test_build_report():
    md = build_report(SAMPLE, title="Tes")
    assert "# Laporan Analisis: Tes" in md
    assert "## Ringkasan" in md
    assert "## Sentimen" in md
    assert "## Kata Kunci" in md

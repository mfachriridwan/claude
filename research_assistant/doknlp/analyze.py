"""Analisis NLP inti: statistik, ringkasan ekstraktif, keyword, sentimen, topik.

Semua fungsi default murni stdlib & deterministik (mudah dites). Topic modeling
otomatis memakai LDA dari scikit-learn kalau terpasang, dengan fallback berbasis
ko-okurensi.
"""

from __future__ import annotations

import math
from collections import Counter, defaultdict

from .preprocess import (
    content_words,
    split_sentences,
    tokenize_words,
    STOPWORDS,
)

# Leksikon sentimen ringkas dwibahasa (ID + EN).
_POSITIVE = {
    "baik", "bagus", "hebat", "luar", "biasa", "senang", "puas", "sukses",
    "berhasil", "untung", "manfaat", "efektif", "efisien", "unggul", "positif",
    "meningkat", "tumbuh", "cepat", "mudah", "indah", "cinta", "suka", "kuat",
    "good", "great", "excellent", "amazing", "happy", "success", "successful",
    "benefit", "effective", "efficient", "positive", "improve", "improved",
    "growth", "fast", "easy", "love", "like", "strong", "best", "wonderful",
    "gain", "advantage", "robust", "optimal", "win", "winning",
}
_NEGATIVE = {
    "buruk", "jelek", "gagal", "rugi", "masalah", "lemah", "lambat", "sulit",
    "susah", "salah", "turun", "menurun", "negatif", "bahaya", "krisis",
    "kecewa", "benci", "takut", "rusak", "cacat", "hambatan", "kendala",
    "bad", "poor", "fail", "failure", "loss", "problem", "weak", "slow",
    "difficult", "hard", "wrong", "decline", "negative", "danger", "crisis",
    "disappoint", "hate", "fear", "broken", "defect", "risk", "threat",
    "worst", "bug", "error", "issue",
}


def document_stats(text: str) -> dict:
    """Statistik dasar dokumen."""
    tokens = tokenize_words(text)
    sentences = split_sentences(text)
    n_words = len(tokens)
    n_sent = len(sentences)
    n_chars = len(text)
    unique = len(set(tokens))
    avg_sent_len = round(n_words / n_sent, 2) if n_sent else 0.0
    lexical_diversity = round(unique / n_words, 4) if n_words else 0.0
    # Asumsi kecepatan baca 200 kata/menit.
    reading_minutes = round(n_words / 200, 1)
    return {
        "karakter": n_chars,
        "kata": n_words,
        "kalimat": n_sent,
        "kata_unik": unique,
        "rata_kata_per_kalimat": avg_sent_len,
        "keberagaman_leksikal": lexical_diversity,
        "estimasi_waktu_baca_menit": reading_minutes,
    }


def _word_frequencies(text: str) -> Counter:
    """Frekuensi kata isi (tanpa stopword), dinormalisasi ke 0..1."""
    words = content_words(text)
    freq = Counter(words)
    if not freq:
        return freq
    top = freq.most_common(1)[0][1]
    for w in freq:
        freq[w] = freq[w] / top
    return freq


def summarize(text: str, n_sentences: int = 5) -> list[str]:
    """Ringkasan ekstraktif berbasis frekuensi kata.

    Setiap kalimat diberi skor = jumlah frekuensi-ternormalisasi kata isinya
    (dirata-rata terhadap panjang supaya tidak bias ke kalimat panjang). Kalimat
    skor tertinggi dipilih, lalu dikembalikan sesuai urutan asli.
    """
    sentences = split_sentences(text)
    if len(sentences) <= n_sentences:
        return sentences

    freq = _word_frequencies(text)
    scored: list[tuple[int, float]] = []
    for idx, sent in enumerate(sentences):
        words = content_words(sent)
        if not words:
            scored.append((idx, 0.0))
            continue
        score = sum(freq.get(w, 0.0) for w in words) / math.sqrt(len(words))
        scored.append((idx, score))

    top_idx = sorted(scored, key=lambda x: x[1], reverse=True)[:n_sentences]
    chosen = sorted(i for i, _ in top_idx)
    return [sentences[i] for i in chosen]


def extract_keywords(text: str, n: int = 15) -> list[tuple[str, float]]:
    """Keyword via TF-IDF dengan tiap kalimat sebagai 'dokumen'.

    Memberi bobot lebih ke kata yang khas (muncul di sedikit kalimat) ketimbang
    kata yang tersebar di mana-mana. Mengembalikan daftar (kata, skor).
    """
    sentences = split_sentences(text)
    if not sentences:
        return []

    # Document frequency: berapa kalimat mengandung kata.
    df: Counter = Counter()
    tf: Counter = Counter()
    for sent in sentences:
        words = content_words(sent)
        tf.update(words)
        for w in set(words):
            df[w] += 1

    n_docs = len(sentences)
    scores: dict[str, float] = {}
    for word, freq in tf.items():
        idf = math.log((1 + n_docs) / (1 + df[word])) + 1.0
        scores[word] = freq * idf

    ranked = sorted(scores.items(), key=lambda x: x[1], reverse=True)[:n]
    return [(w, round(s, 3)) for w, s in ranked]


def sentiment(text: str) -> dict:
    """Analisis sentimen berbasis leksikon dwibahasa."""
    tokens = tokenize_words(text)
    pos = sum(1 for t in tokens if t in _POSITIVE)
    neg = sum(1 for t in tokens if t in _NEGATIVE)
    total = pos + neg
    if total == 0:
        polarity = 0.0
        label = "netral"
    else:
        polarity = round((pos - neg) / total, 3)
        if polarity > 0.15:
            label = "positif"
        elif polarity < -0.15:
            label = "negatif"
        else:
            label = "netral"
    return {
        "label": label,
        "polaritas": polarity,
        "kata_positif": pos,
        "kata_negatif": neg,
    }


def _topics_lda(sentences: list[str], n_topics: int, n_words: int):
    """Topic modeling LDA via scikit-learn (kalau tersedia)."""
    from sklearn.decomposition import LatentDirichletAllocation
    from sklearn.feature_extraction.text import CountVectorizer

    vectorizer = CountVectorizer(
        stop_words=list(STOPWORDS), token_pattern=r"\b[\w']+\b", min_df=1
    )
    dtm = vectorizer.fit_transform(sentences)
    vocab = vectorizer.get_feature_names_out()
    n_topics = min(n_topics, dtm.shape[0]) or 1

    lda = LatentDirichletAllocation(
        n_components=n_topics, random_state=42, learning_method="batch"
    )
    lda.fit(dtm)

    result = []
    for topic in lda.components_:
        top = topic.argsort()[: -n_words - 1 : -1]
        result.append([vocab[i] for i in top])
    return result


def _topics_cooccurrence(sentences: list[str], n_topics: int, n_words: int):
    """Fallback tanpa sklearn: kelompokkan keyword teratas via ko-okurensi.

    Ambil keyword paling kuat sebagai 'benih' tiap topik, lalu lampirkan kata
    yang paling sering muncul bersama benih tersebut dalam kalimat yang sama.
    """
    full_text = " ".join(sentences)
    seeds = [w for w, _ in extract_keywords(full_text, n=n_topics)]
    if not seeds:
        return []

    cooc: dict[str, Counter] = {s: Counter() for s in seeds}
    for sent in sentences:
        words = set(content_words(sent))
        for seed in seeds:
            if seed in words:
                for w in words:
                    if w != seed:
                        cooc[seed][w] += 1

    result = []
    for seed in seeds:
        companions = [w for w, _ in cooc[seed].most_common(n_words - 1)]
        result.append([seed] + companions)
    return result


def topics(text: str, n_topics: int = 5, n_words: int = 8) -> list[list[str]]:
    """Topic modeling. Pakai LDA bila scikit-learn ada, fallback ko-okurensi."""
    sentences = split_sentences(text)
    if not sentences:
        return []
    try:
        return _topics_lda(sentences, n_topics, n_words)
    except ImportError:
        return _topics_cooccurrence(sentences, n_topics, n_words)
    except ValueError:
        # Misal vocab kosong setelah stopword removal.
        return _topics_cooccurrence(sentences, n_topics, n_words)

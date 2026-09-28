"""Preprocessing teks: tokenisasi kata & kalimat, daftar stopword.

Mendukung campuran Bahasa Indonesia + Inggris. Semua murni stdlib (regex).
"""

from __future__ import annotations

import re

# Stopword ringkas dwibahasa (ID + EN). Sengaja tidak terlalu besar supaya
# tidak butuh download korpus eksternal.
_STOPWORDS_ID = {
    "yang", "dan", "di", "ke", "dari", "ini", "itu", "untuk", "pada", "dengan",
    "atau", "juga", "adalah", "akan", "tidak", "ada", "dalam", "sebagai", "oleh",
    "karena", "agar", "sudah", "saya", "kami", "kita", "mereka", "dia", "ia",
    "saat", "bisa", "dapat", "lebih", "para", "tersebut", "namun", "tetapi",
    "sehingga", "maka", "bahwa", "jika", "kalau", "hanya", "telah", "masih",
    "antara", "setiap", "suatu", "salah", "satu", "dua", "tiga", "yaitu",
    "merupakan", "menjadi", "sangat", "harus", "kemudian", "yakni", "serta",
}
_STOPWORDS_EN = {
    "the", "a", "an", "and", "or", "of", "to", "in", "on", "for", "with", "as",
    "is", "are", "was", "were", "be", "been", "being", "this", "that", "these",
    "those", "it", "its", "at", "by", "from", "but", "not", "no", "so", "if",
    "then", "than", "too", "very", "can", "could", "will", "would", "should",
    "may", "might", "must", "we", "you", "they", "he", "she", "i", "me", "my",
    "our", "their", "his", "her", "do", "does", "did", "have", "has", "had",
    "which", "who", "whom", "what", "when", "where", "why", "how", "all", "any",
    "some", "such", "into", "about", "over", "more", "most", "also", "between",
}
STOPWORDS = _STOPWORDS_ID | _STOPWORDS_EN

_WORD_RE = re.compile(r"\b[\w']+\b", re.UNICODE)
# Pemecah kalimat sederhana: berhenti di . ! ? diikuti spasi/baris baru.
_SENT_RE = re.compile(r"(?<=[.!?])\s+")


def tokenize_words(text: str, lowercase: bool = True) -> list[str]:
    """Pecah teks menjadi daftar token kata."""
    tokens = _WORD_RE.findall(text)
    if lowercase:
        tokens = [t.lower() for t in tokens]
    return tokens


def content_words(text: str, min_len: int = 3) -> list[str]:
    """Token kata tanpa stopword & angka, panjang minimal `min_len`."""
    words = []
    for tok in tokenize_words(text):
        if len(tok) < min_len:
            continue
        if tok in STOPWORDS:
            continue
        if tok.isdigit():
            continue
        words.append(tok)
    return words


def split_sentences(text: str) -> list[str]:
    """Pecah teks menjadi daftar kalimat (dibersihkan dari spasi berlebih)."""
    # Normalisasi whitespace dulu supaya newline tidak memecah kalimat.
    normalized = re.sub(r"\s+", " ", text).strip()
    if not normalized:
        return []
    raw = _SENT_RE.split(normalized)
    return [s.strip() for s in raw if s.strip()]

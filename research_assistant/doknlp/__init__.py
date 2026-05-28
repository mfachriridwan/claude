"""Riset - Asisten analisis dokumen.

Paket core NLP yang dipakai bersama oleh CLI (riset.py) dan dashboard Streamlit.
Core ini ditulis murni dengan standard library Python supaya bisa jalan tanpa
dependency berat. Fitur tambahan (LDA topik, baca PDF, Q&A via Claude) aktif
otomatis kalau library opsionalnya terpasang.
"""

from .loader import load_document
from .preprocess import tokenize_words, split_sentences, STOPWORDS
from .analyze import (
    document_stats,
    summarize,
    extract_keywords,
    sentiment,
    topics,
)
from .qa import answer_question
from .report import build_report

__all__ = [
    "load_document",
    "tokenize_words",
    "split_sentences",
    "STOPWORDS",
    "document_stats",
    "summarize",
    "extract_keywords",
    "sentiment",
    "topics",
    "answer_question",
    "build_report",
]

__version__ = "0.1.0"

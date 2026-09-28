"""Riset - Asisten analisis dokumen.

Paket core NLP yang dipakai bersama oleh CLI (riset.py) dan dashboard Streamlit.
Core ini ditulis murni dengan standard library Python supaya bisa jalan tanpa
dependency berat. Fitur tambahan (LDA topik, baca PDF, Q&A via Claude) aktif
otomatis kalau library opsionalnya terpasang.
"""

from .loader import load_document, html_to_text, is_url, SUPPORTED_EXTENSIONS
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
from .batch import analyze_documents, analyze_one, write_csv, expand_sources
from .lci import (
    extract_lci,
    extract_flows_rulebased,
    detect_functional_unit,
    write_lci_csv,
    CATEGORY_KEYWORDS,
    LCI_CSV_FIELDS,
)

__all__ = [
    "load_document",
    "html_to_text",
    "is_url",
    "SUPPORTED_EXTENSIONS",
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
    "analyze_documents",
    "analyze_one",
    "write_csv",
    "expand_sources",
    "extract_lci",
    "extract_flows_rulebased",
    "detect_functional_unit",
    "write_lci_csv",
    "CATEGORY_KEYWORDS",
    "LCI_CSV_FIELDS",
]

__version__ = "0.3.0"

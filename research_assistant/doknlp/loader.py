"""Pembaca dokumen.

Mendukung .txt dan .md secara native (dibaca per-chunk supaya hemat memori untuk
file besar). Untuk .pdf dipakai pypdf/PyPDF2 kalau tersedia.
"""

from __future__ import annotations

import os


def _read_text_file(filepath: str, chunk_size: int = 1024 * 1024) -> str:
    """Baca file teks besar per-chunk (default 1 MB) supaya hemat memori."""
    parts: list[str] = []
    with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
        while True:
            chunk = f.read(chunk_size)
            if not chunk:
                break
            parts.append(chunk)
    return "".join(parts)


def _read_pdf_file(filepath: str) -> str:
    """Baca PDF memakai pypdf atau PyPDF2 kalau terpasang."""
    reader = None
    try:  # pypdf modern
        from pypdf import PdfReader

        reader = PdfReader(filepath)
    except ImportError:
        try:  # fallback ke PyPDF2 lama
            from PyPDF2 import PdfReader

            reader = PdfReader(filepath)
        except ImportError as exc:  # pragma: no cover - bergantung lingkungan
            raise RuntimeError(
                "Untuk membaca PDF, pasang 'pypdf' (pip install pypdf)."
            ) from exc

    pages = []
    for page in reader.pages:
        pages.append(page.extract_text() or "")
    return "\n".join(pages)


def load_document(filepath: str) -> str:
    """Muat dokumen menjadi satu string teks.

    Format didukung: .txt, .md, .text, .pdf. Format lain dicoba dibaca sebagai
    teks biasa.
    """
    if not os.path.isfile(filepath):
        raise FileNotFoundError(f"File tidak ditemukan: {filepath}")

    ext = os.path.splitext(filepath)[1].lower()
    if ext == ".pdf":
        return _read_pdf_file(filepath)
    return _read_text_file(filepath)

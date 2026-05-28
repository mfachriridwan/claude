"""Pembaca dokumen.

Format didukung tanpa dependency tambahan (murni stdlib):
- .txt / .md / .text  : dibaca per-chunk (hemat memori untuk file besar)
- .html / .htm        : tag HTML dibersihkan via html.parser
- .docx               : teks diekstrak dari word/document.xml via zipfile
- URL (http/https)    : diunduh via urllib lalu dibersihkan seperti HTML

Format opsional (aktif bila library terpasang):
- .pdf                : pypdf / PyPDF2
"""

from __future__ import annotations

import os
import re
import zipfile
from html.parser import HTMLParser
from xml.etree import ElementTree


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


class _HTMLTextExtractor(HTMLParser):
    """Ambil teks dari HTML, abaikan isi <script> dan <style>."""

    _SKIP = {"script", "style", "head", "noscript"}

    def __init__(self) -> None:
        super().__init__()
        self._chunks: list[str] = []
        self._skip_depth = 0

    def handle_starttag(self, tag, attrs):
        if tag in self._SKIP:
            self._skip_depth += 1

    def handle_endtag(self, tag):
        if tag in self._SKIP and self._skip_depth > 0:
            self._skip_depth -= 1

    def handle_data(self, data):
        if self._skip_depth == 0 and data.strip():
            self._chunks.append(data)

    def get_text(self) -> str:
        text = " ".join(self._chunks)
        return re.sub(r"\s+", " ", text).strip()


def html_to_text(html: str) -> str:
    """Konversi string HTML menjadi teks polos (stdlib)."""
    parser = _HTMLTextExtractor()
    parser.feed(html)
    return parser.get_text()


def _read_html_file(filepath: str) -> str:
    return html_to_text(_read_text_file(filepath))


def _read_docx_file(filepath: str) -> str:
    """Ekstrak teks dari .docx via zipfile + XML (tanpa python-docx)."""
    try:
        with zipfile.ZipFile(filepath) as zf:
            with zf.open("word/document.xml") as doc:
                xml = doc.read()
    except (KeyError, zipfile.BadZipFile) as exc:
        raise RuntimeError(f"Bukan file .docx yang valid: {filepath}") from exc

    ns = "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}"
    root = ElementTree.fromstring(xml)
    paragraphs = []
    for para in root.iter(f"{ns}p"):
        texts = [node.text for node in para.iter(f"{ns}t") if node.text]
        if texts:
            paragraphs.append("".join(texts))
    return "\n".join(paragraphs)


def _read_url(url: str, timeout: int = 20) -> str:
    """Unduh halaman web lalu bersihkan jadi teks. Pakai requests bila ada."""
    html = None
    try:  # requests kalau tersedia (handle kompresi/redirect lebih baik)
        import requests

        resp = requests.get(url, timeout=timeout, headers={"User-Agent": "Riset/0.2"})
        resp.raise_for_status()
        html = resp.text
    except ImportError:
        import urllib.request

        req = urllib.request.Request(url, headers={"User-Agent": "Riset/0.2"})
        with urllib.request.urlopen(req, timeout=timeout) as resp:  # noqa: S310
            charset = resp.headers.get_content_charset() or "utf-8"
            html = resp.read().decode(charset, errors="ignore")
    except Exception as exc:  # pragma: no cover - bergantung jaringan
        raise RuntimeError(f"Gagal mengunduh URL: {exc}") from exc
    return html_to_text(html)


def is_url(source: str) -> bool:
    return source.startswith("http://") or source.startswith("https://")


SUPPORTED_EXTENSIONS = (".txt", ".md", ".text", ".pdf", ".html", ".htm", ".docx")


def load_document(source: str) -> str:
    """Muat dokumen menjadi satu string teks.

    `source` bisa berupa path file (.txt/.md/.html/.docx/.pdf) atau URL
    http(s). Format tak dikenal dicoba dibaca sebagai teks biasa.
    """
    if is_url(source):
        return _read_url(source)

    if not os.path.isfile(source):
        raise FileNotFoundError(f"File tidak ditemukan: {source}")

    ext = os.path.splitext(source)[1].lower()
    if ext == ".pdf":
        return _read_pdf_file(source)
    if ext in (".html", ".htm"):
        return _read_html_file(source)
    if ext == ".docx":
        return _read_docx_file(source)
    return _read_text_file(source)

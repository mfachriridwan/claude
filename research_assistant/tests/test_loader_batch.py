"""Tes untuk loader format baru (HTML/DOCX) dan pemrosesan batch."""

import os
import sys
import zipfile

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from doknlp import load_document, html_to_text, is_url  # noqa: E402
from doknlp.batch import (  # noqa: E402
    analyze_one,
    analyze_documents,
    expand_sources,
    write_csv,
    CSV_FIELDS,
)

HTML = """
<html><head><title>Judul</title><style>.x{color:red}</style></head>
<body>
  <h1>Optimasi Kontainer</h1>
  <p>Penempatan kontainer yang baik meningkatkan efisiensi.</p>
  <script>console.log('abaikan ini');</script>
  <p>Pelabuhan modern memakai algoritma.</p>
</body></html>
"""


def test_html_to_text_strips_tags_and_scripts():
    text = html_to_text(HTML)
    assert "Optimasi Kontainer" in text
    assert "efisiensi" in text
    assert "console.log" not in text  # isi <script> dibuang
    assert "color:red" not in text    # isi <style> dibuang
    assert "<" not in text


def test_is_url():
    assert is_url("https://contoh.com")
    assert is_url("http://contoh.com")
    assert not is_url("/path/ke/file.txt")


def test_load_html_file(tmp_path):
    p = tmp_path / "page.html"
    p.write_text(HTML, encoding="utf-8")
    text = load_document(str(p))
    assert "kontainer" in text.lower()
    assert "<p>" not in text


def _make_docx(path, paragraphs):
    """Bikin .docx minimal yang valid (zip berisi word/document.xml)."""
    ns = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    body = "".join(f'<w:p><w:r><w:t>{p}</w:t></w:r></w:p>' for p in paragraphs)
    doc = f'<?xml version="1.0"?><w:document xmlns:w="{ns}"><w:body>{body}</w:body></w:document>'
    with zipfile.ZipFile(path, "w") as zf:
        zf.writestr("word/document.xml", doc)


def test_load_docx_file(tmp_path):
    p = tmp_path / "doc.docx"
    _make_docx(p, ["Baris pertama dokumen.", "Baris kedua tentang kontainer."])
    text = load_document(str(p))
    assert "Baris pertama dokumen." in text
    assert "kontainer" in text


def test_load_invalid_docx(tmp_path):
    import pytest

    p = tmp_path / "rusak.docx"
    p.write_text("ini bukan zip", encoding="utf-8")
    with pytest.raises(RuntimeError):
        load_document(str(p))


def test_analyze_one(tmp_path):
    p = tmp_path / "a.txt"
    p.write_text("Hasilnya sangat baik dan sukses. Efisiensi meningkat pesat.",
                 encoding="utf-8")
    row = analyze_one(str(p))
    assert set(row.keys()) == set(CSV_FIELDS)
    assert row["error"] == ""
    assert row["sentimen"] == "positif"
    assert row["kata"] > 0


def test_analyze_one_handles_error_gracefully():
    row = analyze_one("/tidak/ada/file.txt")
    assert row["error"]            # ada pesan error
    assert row["sumber"] == "/tidak/ada/file.txt"


def test_expand_sources_directory(tmp_path):
    (tmp_path / "a.txt").write_text("dokumen a", encoding="utf-8")
    (tmp_path / "b.md").write_text("dokumen b", encoding="utf-8")
    (tmp_path / "abaikan.bin").write_text("x", encoding="utf-8")
    found = expand_sources([str(tmp_path)])
    assert len(found) == 2  # hanya ekstensi didukung
    assert all(f.endswith((".txt", ".md")) for f in found)


def test_expand_sources_keeps_url():
    assert expand_sources(["https://contoh.com"]) == ["https://contoh.com"]


def test_analyze_documents_and_csv(tmp_path):
    (tmp_path / "a.txt").write_text("Dokumen bagus dan sukses besar.", encoding="utf-8")
    (tmp_path / "b.txt").write_text("Sistem buruk dan gagal total.", encoding="utf-8")
    rows = analyze_documents([str(tmp_path)])
    assert len(rows) == 2

    out = tmp_path / "hasil.csv"
    write_csv(rows, str(out))
    content = out.read_text(encoding="utf-8")
    assert "sumber" in content.splitlines()[0]  # header
    assert len(content.strip().splitlines()) == 3  # header + 2 baris

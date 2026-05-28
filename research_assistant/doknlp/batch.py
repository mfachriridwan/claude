"""Pemrosesan banyak dokumen sekaligus + ekspor CSV.

Mengubah setiap dokumen menjadi satu baris ringkasan analisis (statistik +
sentimen + kata kunci teratas), berguna untuk membandingkan banyak dokumen.
"""

from __future__ import annotations

import csv
import glob
import os

from .loader import load_document, is_url, SUPPORTED_EXTENSIONS
from .analyze import document_stats, sentiment, extract_keywords

# Kolom CSV (urutan tetap).
CSV_FIELDS = [
    "sumber",
    "kata",
    "kalimat",
    "kata_unik",
    "rata_kata_per_kalimat",
    "keberagaman_leksikal",
    "estimasi_waktu_baca_menit",
    "sentimen",
    "polaritas",
    "kata_kunci",
    "error",
]


def expand_sources(sources: list[str], recursive: bool = False) -> list[str]:
    """Perluas daftar sumber: folder -> file di dalamnya, glob -> match, URL apa adanya.

    File difilter berdasarkan ekstensi yang didukung.
    """
    expanded: list[str] = []
    for src in sources:
        if is_url(src):
            expanded.append(src)
            continue
        if os.path.isdir(src):
            pattern = "**/*" if recursive else "*"
            for path in sorted(glob.glob(os.path.join(src, pattern), recursive=recursive)):
                if os.path.isfile(path) and path.lower().endswith(SUPPORTED_EXTENSIONS):
                    expanded.append(path)
        elif any(ch in src for ch in "*?[") and not os.path.exists(src):
            expanded.extend(sorted(glob.glob(src, recursive=recursive)))
        else:
            expanded.append(src)
    return expanded


def analyze_one(source: str, n_keywords: int = 10) -> dict:
    """Analisis satu dokumen menjadi satu baris dict. Error ditangkap, bukan dilempar."""
    row = {field: "" for field in CSV_FIELDS}
    row["sumber"] = source
    try:
        text = load_document(source)
        stats = document_stats(text)
        sent = sentiment(text)
        kws = extract_keywords(text, n=n_keywords)
        row.update(
            {
                "kata": stats["kata"],
                "kalimat": stats["kalimat"],
                "kata_unik": stats["kata_unik"],
                "rata_kata_per_kalimat": stats["rata_kata_per_kalimat"],
                "keberagaman_leksikal": stats["keberagaman_leksikal"],
                "estimasi_waktu_baca_menit": stats["estimasi_waktu_baca_menit"],
                "sentimen": sent["label"],
                "polaritas": sent["polaritas"],
                "kata_kunci": "; ".join(w for w, _ in kws),
            }
        )
    except Exception as exc:  # tetap lanjut ke dokumen lain
        row["error"] = str(exc)
    return row


def analyze_documents(sources: list[str], n_keywords: int = 10,
                      recursive: bool = False) -> list[dict]:
    """Analisis banyak dokumen, kembalikan daftar baris dict."""
    paths = expand_sources(sources, recursive=recursive)
    return [analyze_one(p, n_keywords=n_keywords) for p in paths]


def write_csv(rows: list[dict], output_path: str) -> None:
    """Tulis hasil batch ke file CSV."""
    with open(output_path, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=CSV_FIELDS)
        writer.writeheader()
        writer.writerows(rows)

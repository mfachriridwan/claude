#!/usr/bin/env python3
"""Riset - asisten analisis dokumen via command line.

Contoh:
    python riset.py stats dokumen.txt
    python riset.py summarize dokumen.txt -n 5
    python riset.py keywords dokumen.pdf -n 20
    python riset.py sentiment dokumen.txt
    python riset.py topics dokumen.txt -k 5
    python riset.py ask dokumen.txt -q "apa kesimpulan utamanya?"
    python riset.py report dokumen.txt -o laporan.md
    python riset.py batch dokumen/ *.pdf https://contoh.com -o hasil.csv
    python riset.py dashboard

Sumber dokumen bisa berupa .txt/.md/.html/.docx/.pdf atau URL http(s).
"""

from __future__ import annotations

import argparse
import os
import sys

from doknlp import (
    load_document,
    document_stats,
    summarize,
    extract_keywords,
    sentiment,
    topics,
    answer_question,
    build_report,
    analyze_documents,
    write_csv,
)


def _load(path: str) -> str:
    try:
        return load_document(path)
    except (FileNotFoundError, RuntimeError) as exc:
        print(f"Error: {exc}", file=sys.stderr)
        sys.exit(1)


def cmd_stats(args) -> None:
    stats = document_stats(_load(args.file))
    print(f"Statistik untuk: {args.file}\n")
    width = max(len(k) for k in stats)
    for key, val in stats.items():
        print(f"  {key.replace('_', ' ').ljust(width)} : {val}")


def cmd_summarize(args) -> None:
    sents = summarize(_load(args.file), n_sentences=args.n)
    print(f"Ringkasan ({len(sents)} kalimat):\n")
    for i, s in enumerate(sents, 1):
        print(f"{i}. {s}")


def cmd_keywords(args) -> None:
    kws = extract_keywords(_load(args.file), n=args.n)
    print(f"Kata kunci teratas ({len(kws)}):\n")
    for word, score in kws:
        print(f"  {score:>7.3f}  {word}")


def cmd_sentiment(args) -> None:
    s = sentiment(_load(args.file))
    print(f"Sentimen   : {s['label']}")
    print(f"Polaritas  : {s['polaritas']}")
    print(f"Positif    : {s['kata_positif']}")
    print(f"Negatif    : {s['kata_negatif']}")


def cmd_topics(args) -> None:
    tp = topics(_load(args.file), n_topics=args.k, n_words=args.words)
    print(f"Topik ({len(tp)}):\n")
    for i, topic in enumerate(tp, 1):
        print(f"  Topik {i}: {', '.join(topic)}")


def cmd_ask(args) -> None:
    result = answer_question(_load(args.file), args.q, k=args.k, use_llm=args.llm)
    tag = "Claude" if result["sumber_llm"] else "ekstraktif"
    print(f"Pertanyaan: {args.q}\n")
    print(f"Jawaban ({tag}):\n{result['jawaban']}\n")
    if result["konteks"]:
        print("Kalimat sumber:")
        for c in result["konteks"]:
            print(f"  - {c}")


def cmd_report(args) -> None:
    text = _load(args.file)
    md = build_report(text, title=args.file, n_summary=args.summary,
                       n_keywords=args.keywords, n_topics=args.topics)
    if args.output:
        with open(args.output, "w", encoding="utf-8") as f:
            f.write(md)
        print(f"Laporan disimpan ke: {args.output}")
    else:
        print(md)


def cmd_batch(args) -> None:
    rows = analyze_documents(args.sources, n_keywords=args.keywords,
                             recursive=args.recursive)
    if not rows:
        print("Tidak ada dokumen yang cocok.", file=sys.stderr)
        sys.exit(1)

    if args.output:
        write_csv(rows, args.output)
        ok = sum(1 for r in rows if not r["error"])
        print(f"{ok}/{len(rows)} dokumen dianalisis. CSV disimpan ke: {args.output}")
        errors = [r for r in rows if r["error"]]
        if errors:
            print(f"\n{len(errors)} dokumen gagal:")
            for r in errors:
                print(f"  - {r['sumber']}: {r['error']}")
    else:
        # Tabel ringkas ke layar.
        print(f"{'sumber':<40} {'kata':>7} {'sentimen':>10} {'polaritas':>10}")
        print("-" * 70)
        for r in rows:
            if r["error"]:
                print(f"{os.path.basename(str(r['sumber'])):<40} ERROR: {r['error']}")
            else:
                name = str(r["sumber"])
                name = name if len(name) <= 40 else "..." + name[-37:]
                print(f"{name:<40} {r['kata']:>7} {r['sentimen']:>10} {r['polaritas']:>10}")


def cmd_dashboard(args) -> None:
    import subprocess

    app = os.path.join(os.path.dirname(os.path.abspath(__file__)), "dashboard", "app.py")
    print("Menjalankan dashboard Streamlit...")
    try:
        subprocess.run(["streamlit", "run", app], check=True)
    except FileNotFoundError:
        print("Streamlit belum terpasang. Jalankan: pip install streamlit",
              file=sys.stderr)
        sys.exit(1)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="riset", description="Asisten analisis dokumen (NLP)."
    )
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("stats", help="Statistik dokumen")
    p.add_argument("file")
    p.set_defaults(func=cmd_stats)

    p = sub.add_parser("summarize", help="Ringkasan ekstraktif")
    p.add_argument("file")
    p.add_argument("-n", type=int, default=5, help="jumlah kalimat (default 5)")
    p.set_defaults(func=cmd_summarize)

    p = sub.add_parser("keywords", help="Ekstraksi kata kunci (TF-IDF)")
    p.add_argument("file")
    p.add_argument("-n", type=int, default=15, help="jumlah kata kunci (default 15)")
    p.set_defaults(func=cmd_keywords)

    p = sub.add_parser("sentiment", help="Analisis sentimen")
    p.add_argument("file")
    p.set_defaults(func=cmd_sentiment)

    p = sub.add_parser("topics", help="Topic modeling")
    p.add_argument("file")
    p.add_argument("-k", type=int, default=5, help="jumlah topik (default 5)")
    p.add_argument("--words", type=int, default=8, help="kata per topik (default 8)")
    p.set_defaults(func=cmd_topics)

    p = sub.add_parser("ask", help="Tanya-jawab dokumen")
    p.add_argument("file")
    p.add_argument("-q", required=True, help="pertanyaan")
    p.add_argument("-k", type=int, default=3, help="jumlah kalimat konteks")
    p.add_argument("--llm", action="store_true",
                   help="sintesis jawaban via Claude (butuh ANTHROPIC_API_KEY)")
    p.set_defaults(func=cmd_ask)

    p = sub.add_parser("report", help="Laporan analisis lengkap (Markdown)")
    p.add_argument("file")
    p.add_argument("-o", "--output", help="simpan ke file (default cetak ke layar)")
    p.add_argument("--summary", type=int, default=5)
    p.add_argument("--keywords", type=int, default=15)
    p.add_argument("--topics", type=int, default=5)
    p.set_defaults(func=cmd_report)

    p = sub.add_parser("batch", help="Analisis banyak dokumen -> CSV")
    p.add_argument("sources", nargs="+",
                   help="file, folder, pola glob, atau URL (boleh banyak)")
    p.add_argument("-o", "--output", help="simpan ke CSV (default tabel di layar)")
    p.add_argument("--keywords", type=int, default=10, help="kata kunci per dokumen")
    p.add_argument("-r", "--recursive", action="store_true",
                   help="telusuri folder secara rekursif")
    p.set_defaults(func=cmd_batch)

    p = sub.add_parser("dashboard", help="Jalankan dashboard Streamlit")
    p.set_defaults(func=cmd_dashboard)

    return parser


def main(argv: list[str] | None = None) -> None:
    parser = build_parser()
    args = parser.parse_args(argv)
    args.func(args)


if __name__ == "__main__":
    main()

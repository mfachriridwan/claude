"""Penyusun laporan analisis lengkap dalam format Markdown."""

from __future__ import annotations

from .analyze import document_stats, summarize, extract_keywords, sentiment, topics


def build_report(text: str, title: str = "Dokumen", n_summary: int = 5,
                 n_keywords: int = 15, n_topics: int = 5) -> str:
    """Hasilkan laporan analisis lengkap (Markdown)."""
    stats = document_stats(text)
    sm = summarize(text, n_sentences=n_summary)
    kw = extract_keywords(text, n=n_keywords)
    sent = sentiment(text)
    tp = topics(text, n_topics=n_topics)

    lines: list[str] = []
    lines.append(f"# Laporan Analisis: {title}\n")

    lines.append("## Statistik")
    lines.append("")
    lines.append("| Metrik | Nilai |")
    lines.append("|---|---|")
    for key, val in stats.items():
        lines.append(f"| {key.replace('_', ' ')} | {val} |")
    lines.append("")

    lines.append("## Sentimen")
    lines.append(
        f"- **Label:** {sent['label']} (polaritas {sent['polaritas']})"
    )
    lines.append(
        f"- Kata positif: {sent['kata_positif']}, "
        f"kata negatif: {sent['kata_negatif']}"
    )
    lines.append("")

    lines.append("## Ringkasan")
    lines.append("")
    for s in sm:
        lines.append(f"- {s}")
    lines.append("")

    lines.append("## Kata Kunci (TF-IDF)")
    lines.append("")
    lines.append(", ".join(f"{w} ({score})" for w, score in kw))
    lines.append("")

    lines.append("## Topik")
    lines.append("")
    for i, topic in enumerate(tp, 1):
        lines.append(f"{i}. {', '.join(topic)}")
    lines.append("")

    return "\n".join(lines)

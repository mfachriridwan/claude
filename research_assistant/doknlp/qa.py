"""Tanya-jawab dokumen (Q&A).

Default: ekstraktif & offline — cari kalimat paling relevan via kemiripan
TF-IDF kosinus terhadap pertanyaan. Opsional: kalau ANTHROPIC_API_KEY tersedia
dan SDK terpasang, jawaban bisa disintesis oleh Claude memakai konteks tersebut.
"""

from __future__ import annotations

import math
import os
from collections import Counter

from .preprocess import content_words, split_sentences


def _vectorize(tokens: list[str]) -> Counter:
    return Counter(tokens)


def _cosine(a: Counter, b: Counter) -> float:
    if not a or not b:
        return 0.0
    common = set(a) & set(b)
    dot = sum(a[t] * b[t] for t in common)
    na = math.sqrt(sum(v * v for v in a.values()))
    nb = math.sqrt(sum(v * v for v in b.values()))
    if na == 0 or nb == 0:
        return 0.0
    return dot / (na * nb)


def relevant_sentences(text: str, question: str, k: int = 3) -> list[str]:
    """Ambil `k` kalimat paling relevan dengan pertanyaan (ekstraktif)."""
    sentences = split_sentences(text)
    if not sentences:
        return []
    q_vec = _vectorize(content_words(question))
    scored = []
    for sent in sentences:
        s_vec = _vectorize(content_words(sent))
        scored.append((sent, _cosine(q_vec, s_vec)))
    scored.sort(key=lambda x: x[1], reverse=True)
    return [s for s, score in scored[:k] if score > 0]


def _answer_with_claude(question: str, context: list[str]) -> str | None:
    """Sintesis jawaban via Claude. Return None bila tidak bisa (no key/SDK)."""
    if not os.environ.get("ANTHROPIC_API_KEY"):
        return None
    try:
        import anthropic
    except ImportError:
        return None

    try:
        client = anthropic.Anthropic()
        ctx = "\n".join(f"- {c}" for c in context)
        msg = client.messages.create(
            model="claude-sonnet-4-6",
            max_tokens=512,
            messages=[
                {
                    "role": "user",
                    "content": (
                        "Jawab pertanyaan HANYA berdasarkan konteks berikut. "
                        "Jika tidak ada di konteks, katakan tidak tahu.\n\n"
                        f"Konteks:\n{ctx}\n\nPertanyaan: {question}"
                    ),
                }
            ],
        )
        return "".join(
            block.text for block in msg.content if getattr(block, "type", "") == "text"
        ).strip()
    except Exception:  # pragma: no cover - bergantung jaringan/SDK
        return None


def answer_question(text: str, question: str, k: int = 3, use_llm: bool = False) -> dict:
    """Jawab pertanyaan terhadap dokumen.

    Mengembalikan dict berisi kalimat sumber (`konteks`) dan `jawaban`. Bila
    `use_llm` True dan Claude tersedia, jawaban disintesis; selain itu jawaban
    adalah gabungan kalimat sumber paling relevan.
    """
    context = relevant_sentences(text, question, k=k)
    llm_answer = _answer_with_claude(question, context) if use_llm else None

    if llm_answer is not None:
        answer = llm_answer
    elif context:
        answer = " ".join(context)
    else:
        answer = "Tidak ditemukan bagian dokumen yang relevan dengan pertanyaan."

    return {"jawaban": answer, "konteks": context, "sumber_llm": llm_answer is not None}

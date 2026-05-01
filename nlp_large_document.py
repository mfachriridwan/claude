# ============================================================
# NLP untuk Membaca Dokumen Besar - Python
# Libraries: nltk, spacy, transformers, sklearn, matplotlib
# ============================================================

# Install libraries (jalankan di terminal):
# pip install nltk spacy scikit-learn matplotlib wordcloud PyPDF2 pandas transformers torch

import os
import re
import nltk
import string
import pandas as pd
import matplotlib.pyplot as plt
from collections import Counter
from pathlib import Path

# Download NLTK data
nltk.download('punkt', quiet=True)
nltk.download('stopwords', quiet=True)
nltk.download('wordnet', quiet=True)
nltk.download('vader_lexicon', quiet=True)

from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize, sent_tokenize
from nltk.stem import WordNetLemmatizer
from nltk.sentiment.vader import SentimentIntensityAnalyzer

# ============================================================
# 1. BACA DOKUMEN BESAR SECARA EFISIEN (CHUNK BY CHUNK)
# ============================================================

def read_large_txt(filepath: str, chunk_size: int = 1024 * 1024) -> str:
    """Baca file TXT besar per chunk (default 1MB per chunk)."""
    text = []
    with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
        while True:
            chunk = f.read(chunk_size)
            if not chunk:
                break
            text.append(chunk)
    return " ".join(text)


def read_large_pdf(filepath: str) -> str:
    """Baca file PDF halaman per halaman."""
    try:
        import PyPDF2
    except ImportError:
        os.system("pip install PyPDF2 -q")
        import PyPDF2

    text = []
    with open(filepath, "rb") as f:
        reader = PyPDF2.PdfReader(f)
        for page in reader.pages:
            page_text = page.extract_text()
            if page_text:
                text.append(page_text)
    return " ".join(text)


def read_document(filepath: str) -> str:
    """Auto-detect format file dan baca."""
    ext = Path(filepath).suffix.lower()
    if ext == ".pdf":
        return read_large_pdf(filepath)
    elif ext in [".txt", ".md", ".csv"]:
        return read_large_txt(filepath)
    else:
        raise ValueError(f"Format file tidak didukung: {ext}")


# ============================================================
# 2. PREPROCESSING TEKS
# ============================================================

def preprocess_text(text: str) -> str:
    text = text.lower()
    text = re.sub(r'\d+', '', text)                    # hapus angka
    text = text.translate(str.maketrans('', '', string.punctuation))  # hapus tanda baca
    text = re.sub(r'\s+', ' ', text).strip()           # hapus spasi berlebih
    return text


# ============================================================
# 3. TOKENISASI & HAPUS STOP WORDS
# ============================================================

def tokenize_and_clean(text: str, language: str = "english") -> list:
    stop_words = set(stopwords.words(language))
    lemmatizer = WordNetLemmatizer()

    tokens = word_tokenize(text)
    tokens = [
        lemmatizer.lemmatize(word)
        for word in tokens
        if word not in stop_words and len(word) > 2
    ]
    return tokens


# ============================================================
# 4. FREKUENSI KATA (WORD FREQUENCY)
# ============================================================

def word_frequency(tokens: list, top_n: int = 20) -> pd.DataFrame:
    freq = Counter(tokens)
    df = pd.DataFrame(freq.most_common(top_n), columns=["word", "count"])

    plt.figure(figsize=(10, 6))
    plt.barh(df["word"][::-1], df["count"][::-1], color="steelblue")
    plt.xlabel("Frekuensi")
    plt.title(f"Top {top_n} Kata Terbanyak")
    plt.tight_layout()
    plt.savefig("word_frequency.png", dpi=150)
    plt.show()

    return df


# ============================================================
# 5. WORD CLOUD
# ============================================================

def generate_wordcloud(tokens: list, max_words: int = 100):
    try:
        from wordcloud import WordCloud
    except ImportError:
        os.system("pip install wordcloud -q")
        from wordcloud import WordCloud

    text = " ".join(tokens)
    wc = WordCloud(width=800, height=400, max_words=max_words,
                   background_color="white", colormap="viridis")
    wc.generate(text)

    plt.figure(figsize=(12, 6))
    plt.imshow(wc, interpolation="bilinear")
    plt.axis("off")
    plt.title("Word Cloud")
    plt.tight_layout()
    plt.savefig("wordcloud.png", dpi=150)
    plt.show()


# ============================================================
# 6. ANALISIS SENTIMEN (VADER)
# ============================================================

def sentiment_analysis(text: str) -> dict:
    sia = SentimentIntensityAnalyzer()
    sentences = sent_tokenize(text)

    scores = [sia.polarity_scores(sent) for sent in sentences]
    avg = {
        "positive": sum(s["pos"] for s in scores) / len(scores),
        "negative": sum(s["neg"] for s in scores) / len(scores),
        "neutral":  sum(s["neu"] for s in scores) / len(scores),
        "compound": sum(s["compound"] for s in scores) / len(scores),
    }

    print(f"\nHasil Sentimen:")
    for k, v in avg.items():
        print(f"  {k}: {v:.4f}")

    label = "Positif" if avg["compound"] >= 0.05 else "Negatif" if avg["compound"] <= -0.05 else "Netral"
    print(f"  => Kesimpulan: {label}\n")

    plt.figure(figsize=(6, 4))
    plt.bar(["Positif", "Negatif", "Netral"],
            [avg["positive"], avg["negative"], avg["neutral"]],
            color=["seagreen", "tomato", "steelblue"])
    plt.title("Analisis Sentimen")
    plt.ylabel("Skor Rata-rata")
    plt.tight_layout()
    plt.savefig("sentiment.png", dpi=150)
    plt.show()

    return avg


# ============================================================
# 7. TOPIC MODELING (LDA)
# ============================================================

def topic_modeling(tokens: list, n_topics: int = 5, n_words: int = 10):
    from sklearn.feature_extraction.text import CountVectorizer
    from sklearn.decomposition import LatentDirichletAllocation

    text = " ".join(tokens)
    vectorizer = CountVectorizer(max_df=0.95, min_df=2, max_features=1000)
    dtm = vectorizer.fit_transform([text])

    lda = LatentDirichletAllocation(n_components=n_topics, random_state=42)
    lda.fit(dtm)

    feature_names = vectorizer.get_feature_names_out()
    print(f"\nTopic Modeling ({n_topics} Topik):")
    for idx, topic in enumerate(lda.components_):
        top_words = [feature_names[i] for i in topic.argsort()[:-n_words - 1:-1]]
        print(f"  Topik {idx + 1}: {', '.join(top_words)}")


# ============================================================
# 8. NAMED ENTITY RECOGNITION (NER) dengan spaCy
# ============================================================

def named_entity_recognition(text: str, sample_chars: int = 5000):
    try:
        import spacy
        nlp = spacy.load("en_core_web_sm")
    except Exception:
        os.system("pip install spacy -q && python -m spacy download en_core_web_sm -q")
        import spacy
        nlp = spacy.load("en_core_web_sm")

    sample = text[:sample_chars]
    doc = nlp(sample)

    entities = [(ent.text, ent.label_) for ent in doc.ents]
    df = pd.DataFrame(entities, columns=["Entity", "Type"])

    print("\nNamed Entities (sample 5000 karakter pertama):")
    print(df["Type"].value_counts().to_string())
    return df


# ============================================================
# 9. PIPELINE LENGKAP - JALANKAN SEMUA SEKALIGUS
# ============================================================

def run_nlp_pipeline(filepath: str, language: str = "english",
                     top_words: int = 20, n_topics: int = 5):
    print(">> Membaca dokumen...")
    raw_text = read_document(filepath)
    print(f"   Total karakter: {len(raw_text):,}")

    print(">> Preprocessing...")
    clean_text = preprocess_text(raw_text)

    print(">> Tokenisasi & hapus stop words...")
    tokens = tokenize_and_clean(clean_text, language=language)
    print(f"   Total token: {len(tokens):,} | Unik: {len(set(tokens)):,}")

    print(">> Word frequency...")
    freq_df = word_frequency(tokens, top_n=top_words)
    print(freq_df.head(10).to_string(index=False))

    print(">> Word cloud...")
    generate_wordcloud(tokens)

    print(">> Analisis sentimen...")
    sentiment_analysis(raw_text)

    print(">> Topic modeling (LDA)...")
    topic_modeling(tokens, n_topics=n_topics)

    print(">> Named Entity Recognition...")
    ner_df = named_entity_recognition(raw_text)

    print(">> Selesai! Output disimpan: word_frequency.png, wordcloud.png, sentiment.png")
    return tokens, freq_df, ner_df


# ============================================================
# CONTOH PENGGUNAAN
# ============================================================

if __name__ == "__main__":
    # Untuk file TXT:
    # tokens, freq, ner = run_nlp_pipeline("dokumen_besar.txt")

    # Untuk file PDF:
    # tokens, freq, ner = run_nlp_pipeline("laporan.pdf")

    # Dengan bahasa lain (misal Indonesia - perlu stopwords tambahan):
    # tokens, freq, ner = run_nlp_pipeline("dokumen.txt", language="indonesian")

    print("Siap digunakan. Uncomment baris run_nlp_pipeline() di atas dan sesuaikan path file.")

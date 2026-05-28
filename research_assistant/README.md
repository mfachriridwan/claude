# 📄 Riset — Asisten Analisis Dokumen

Tool serbaguna untuk menganalisis dokumen besar, tersedia sebagai **CLI** dan
**dashboard interaktif**. Dibangun di atas core NLP yang sama
(`doknlp/`), pengembangan dari skrip `nlp_large_document.py` di repo ini.

Fitur:

- 📊 **Statistik** dokumen (jumlah kata/kalimat, keberagaman leksikal, estimasi waktu baca)
- 📝 **Ringkasan ekstraktif** berbasis frekuensi
- 🔑 **Ekstraksi kata kunci** dengan TF-IDF
- 🙂 **Analisis sentimen** (leksikon dwibahasa ID + EN)
- 🧩 **Topic modeling** (LDA via scikit-learn, dengan fallback ko-okurensi)
- 💬 **Tanya-jawab dokumen** (ekstraktif, opsional disintesis Claude)
- 📄 **Laporan Markdown** lengkap dalam satu perintah

## Desain

Core (`doknlp/`) ditulis **murni dengan standard library Python** sehingga jalan
tanpa instalasi apa pun dan mudah dites. Fitur tambahan aktif otomatis bila
library opsional terpasang:

| Fitur | Library opsional | Fallback |
|---|---|---|
| Baca PDF | `pypdf` / `PyPDF2` | error informatif |
| Topic modeling LDA | `scikit-learn` | ko-okurensi keyword |
| Q&A `--llm` | `anthropic` + `ANTHROPIC_API_KEY` | jawaban ekstraktif |
| Dashboard | `streamlit` | — |

## Pemakaian CLI

```bash
cd research_assistant

python riset.py stats dokumen.txt
python riset.py summarize dokumen.txt -n 5
python riset.py keywords dokumen.txt -n 20
python riset.py sentiment dokumen.txt
python riset.py topics dokumen.txt -k 5
python riset.py ask dokumen.txt -q "apa kesimpulan utamanya?"
python riset.py ask dokumen.txt -q "..." --llm        # sintesis via Claude
python riset.py report dokumen.txt -o laporan.md
```

## Dashboard

```bash
pip install streamlit
python riset.py dashboard
# atau: streamlit run dashboard/app.py
```

Upload `.txt`/`.md` atau tempel teks, atur slider, lalu jelajahi tab
Ringkasan / Kata Kunci / Topik / Tanya-Jawab.

## Tes

```bash
cd research_assistant
pytest                # core tidak butuh dependency apa pun
```

## Struktur

```
research_assistant/
├── riset.py              # entry point CLI
├── doknlp/               # core NLP (murni stdlib)
│   ├── loader.py         # baca txt/md/pdf
│   ├── preprocess.py     # tokenisasi, stopword
│   ├── analyze.py        # statistik, ringkasan, keyword, sentimen, topik
│   ├── qa.py             # tanya-jawab dokumen
│   └── report.py         # laporan Markdown
├── dashboard/app.py      # dashboard Streamlit
├── tests/test_doknlp.py  # tes pytest
└── requirements.txt
```

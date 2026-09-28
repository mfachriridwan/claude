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
- 📚 **Batch** banyak dokumen sekaligus → ekspor **CSV**
- 📥 **Banyak format**: `.txt`, `.md`, `.html`, `.docx`, `.pdf`, dan **URL** http(s)
- ♻️ **Paper → Life Cycle Inventory (LCI)**: ekstrak functional unit + aliran
  energi/material/emisi/air/limbah/transport (rule-based, atau Claude bila ada API key)

## Desain

Core (`doknlp/`) ditulis **murni dengan standard library Python** sehingga jalan
tanpa instalasi apa pun dan mudah dites — termasuk membaca **HTML, DOCX, dan
URL** (lewat `html.parser`, `zipfile`, dan `urllib`). Fitur tambahan aktif
otomatis bila library opsional terpasang:

| Fitur | Library opsional | Fallback |
|---|---|---|
| Baca PDF | `pypdf` / `PyPDF2` | error informatif |
| Topic modeling LDA | `scikit-learn` | ko-okurensi keyword |
| Q&A `--llm` | `anthropic` + `ANTHROPIC_API_KEY` | jawaban ekstraktif |
| Wordcloud di dashboard | `wordcloud` + `matplotlib` | grafik batang keyword |
| Unduh URL lebih andal | `requests` | `urllib` (stdlib) |
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

# Sumber boleh berupa file, folder, pola glob, atau URL
python riset.py stats artikel.docx
python riset.py summarize halaman.html -n 5
python riset.py keywords https://contoh.com/artikel -n 20

# Batch: banyak dokumen sekaligus -> CSV
python riset.py batch dokumen/ -o hasil.csv          # semua file dalam folder
python riset.py batch *.pdf laporan.docx -o hasil.csv
python riset.py batch dokumen/ -r                    # rekursif, tabel ke layar
```

Kolom CSV batch: `sumber, kata, kalimat, kata_unik, rata_kata_per_kalimat,
keberagaman_leksikal, estimasi_waktu_baca_menit, sentimen, polaritas,
kata_kunci, error`. Dokumen yang gagal dibaca tetap dicatat dengan pesan di
kolom `error` (proses tidak berhenti).

## Paper → Life Cycle Inventory (LCI)

Ekstrak kandidat data LCI dari paper: **functional unit** + aliran input/output
(energi, material, emisi udara/air, air, limbah, transport, lahan) lengkap
dengan jumlah & satuan.

```bash
python riset.py lci paper.pdf                 # tabel LCI ke layar
python riset.py lci paper.pdf -o lci.csv       # ekspor CSV
python riset.py lci paper.pdf --json lci.json  # ekspor JSON lengkap
python riset.py lci paper.pdf --llm            # ekstraksi via Claude (lebih akurat)
python riset.py lci https://contoh.com/paper   # langsung dari URL
```

Dua mesin ekstraksi:

- **Rule-based** (default): deteksi pola `angka + satuan`, klasifikasi ke
  kategori LCI lewat kata kunci yang paling dekat dengan angka. Murni stdlib,
  jalan offline. Hasilnya **kandidat** yang perlu diverifikasi.
- **Claude** (`--llm`, butuh `ANTHROPIC_API_KEY`): ekstraksi terstruktur
  (nama, tipe input/output, kategori, nilai, satuan, kompartemen) jauh lebih akurat.

> ⚠️ Selalu verifikasi nilai & satuan terhadap paper asli sebelum dipakai
> dalam studi LCA.

## Dashboard

```bash
pip install streamlit
python riset.py dashboard            # dashboard analisis dokumen umum
python riset.py dashboard --lci      # dashboard Paper -> LCI
# atau: streamlit run dashboard/app.py  /  streamlit run dashboard/lci_app.py
```

- **Dashboard umum**: upload `.txt`/`.md`/dll atau tempel teks, jelajahi tab
  Ringkasan / Kata Kunci / Topik / Tanya-Jawab / Batch.
- **Dashboard LCI**: upload paper → functional unit, tabel LCI per kategori,
  grafik, dan ekspor CSV/JSON.

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
│   ├── loader.py         # baca txt/md/html/docx/pdf + URL
│   ├── preprocess.py     # tokenisasi, stopword
│   ├── analyze.py        # statistik, ringkasan, keyword, sentimen, topik
│   ├── qa.py             # tanya-jawab dokumen
│   ├── batch.py          # pemrosesan banyak dokumen + ekspor CSV
│   ├── lci.py            # ekstraksi Life Cycle Inventory (paper -> LCI)
│   └── report.py         # laporan Markdown
├── dashboard/
│   ├── app.py            # dashboard analisis dokumen (wordcloud, topik, batch)
│   └── lci_app.py        # dashboard Paper -> LCI
├── tests/                # tes pytest
└── requirements.txt
```

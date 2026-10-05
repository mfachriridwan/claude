# Audit sumber asumsi hierarki fraksi RIPS

## Status sumber

Workbook menggunakan tiga jenis informasi yang harus dibedakan:

1. **Taksonomi sumber** — nama dan hubungan fraksi dari Edjabou et al. (2015), Table 2.
2. **Rasio turunan Denmark** — rasio yang dihitung dari rata-rata kolom single-family (SF) dan multi-family (MF) pada Table 3 atau Table 4, lalu dinormalisasi di dalam fraksi induk.
3. **Prior pemodelan** — pembagian sementara ketika RIPS dan artikel tidak memberikan rincian yang cukup. Ini bukan hasil pengukuran Indonesia.

## Audit asumsi utama

| ID | Asumsi workbook | Asal sebenarnya | Penilaian |
|---|---|---|---|
| A01 | Organik gabungan: 85% Food, 15% Gardening | Prior pemodelan | Tidak memiliki dasar numerik langsung dari artikel; perlu kalibrasi Indonesia atau analisis sensitivitas. |
| A02 | Kertas/kardus: 55% Paper, 45% Board | Pendekatan kasar terhadap data Denmark | Rata-rata Level I Table 5 lebih dekat ke sekitar 52.7% Paper dan 47.3% Board. Bukan rasio Indonesia. |
| A03 | Lain-lain: 80% Miscellaneous combustibles, 20% Inert | Prior pemodelan | Tidak berasal dari Table 3–4; perlu diperlakukan sebagai parameter skenario. |
| A04 | Food: 80% vegetable, 20% animal-derived | Turunan Table 3 | Rata-rata SF/MF menghasilkan 79.39% dan 20.61%; pembulatan 80/20 dapat ditelusuri. |
| A05 | Gardening: 2% dead animal/excrement, 98% garden waste | Prior pemodelan | Tidak didukung Table 3. Rasio Table 3 adalah sekitar 9.20% dan 90.80%. |
| A06 | Plastik dasar: 35% packaging, 5% non-packaging, 60% film | Turunan Table 3 | Rasio Table 3 sekitar 35.04%, 5.11%, dan 59.85%. Styrofoam RIPS ditambahkan ke packaging/PS sebagai keputusan pemetaan. |
| A07 | Metal: 71% packaging, 29% non-packaging | Turunan Table 3 | Rasio Table 3 sekitar 71.11% dan 28.89%. Aluminium foil pada Table 3 adalah nol. |
| A08 | Glass: 91% packaging, 4.5% table/kitchenware, 4.5% other | Turunan Table 3 | Rasio Table 3 sekitar 90.91%, 4.55%, dan 4.55%. |
| A09 | B3: 20% batteries, 80% other HHW; WEEE langsung | Prior pemodelan | Table 3, jika WEEE dipisahkan, menyiratkan sekitar 28.57% batteries dan 71.43% HHW. Nilai 20/80 tidak langsung berasal dari artikel. |
| A10 | Level III tidak harus berjumlah 100% pada file detail | Aturan implementasi workbook | Benar untuk file yang hanya menyimpan anak Level III yang dirinci, tetapi ini bukan kalimat atau aturan eksplisit dari artikel. |

## Audit prior Level III

| Pembagian | Status |
|---|---|
| Food menjadi tiga bagian sama besar per kelompok vegetable/animal | Prior pemodelan; tidak dicantumkan sebagai angka di Table 2–4. |
| Gardening 5/70/20/5 | Prior pemodelan; tidak sama dengan proporsi Table 4. |
| Tujuh miscellaneous-paper sama besar | Prior netral; Table 4 justru menunjukkan distribusi tidak merata dan didominasi tissue paper. |
| Enam miscellaneous-board sama besar | Prior netral; bukan rasio pengukuran sumber. |
| Resin packaging plastic | Diturunkan dari rata-rata SF/MF Table 4; nilai nol diberi pseudocount kecil agar kategori tetap tersedia. |
| Plastic film 90/10 | Pendekatan terhadap Table 4, yang memberi sekitar 91.5/8.5. |
| Ferrous/non-ferrous 60/40 | Pendekatan terhadap Table 4, sekitar 57.8/42.2 jika seluruh metal digabung. |
| Warna kaca 7.5/87.5/5 | Turunan langsung dari rata-rata Table 4 untuk packaging glass. |
| Diapers/tampons/condoms 90/5/5 | Prior pemodelan; Table 4 tidak memberi rinciannya. |
| Leather/rubber 20/80 | Prior pemodelan; RIPS menggabungkan keduanya. |
| Sepuluh kategori WEEE sama besar | Prior netral. EU Directive hanya mendefinisikan kategorinya, bukan distribusi massanya. |

## Arti label `taxonomy rule`

`taxonomy rule` adalah label metadata buatan dalam workbook. Label itu berarti aturan tersebut mengatur **struktur klasifikasi**, bukan memberi angka komposisi empiris.

Artikel menjelaskan tiered approach sebagai subdivisi berurutan agar studi yang menyortir pada kedalaman berbeda tetap dapat dibandingkan pada level agregat. Namun, Table 2 tidak memberi Level III untuk semua induk: Inert diberi tanda tidak ada rincian, sementara kampanye Special waste hanya disortir sampai Level II karena massanya kecil. Karena file `input_kota_level3_detail.csv` hanya memuat refinemen Level III yang eksplisit/dimodelkan, jumlahnya merupakan cakupan parsial terhadap total aliran. Fraksi Level II yang tidak mempunyai anak Level III tetap ada di Level II dan tidak hilang dari neraca massa.

## Keterbatasan angka 56

Artikel menyatakan 56 fraksi Level III, tetapi Table 2 yang tercetak mengandung notasi dan salah nomor yang membuat rekonstruksi literal tidak tunggal: kode kondom terulang, kategori WEEE tercetak di bawah kode 10.3, notasi plastik/metal memakai indeks `i`, dan Food ditampilkan tanpa daftar Level III. Enam subfraksi Food yang dipakai dalam workbook untuk mencapai katalog 56 adalah interpretasi pemodelan dan belum terverifikasi dari daftar eksplisit artikel. Karena itu, Level III versi saat ini sebaiknya dianggap **provisional taxonomy** dan tidak diklaim sebagai reproduksi literal Table 2 sampai daftar pelengkap atau konfirmasi penulis ditemukan.

## Sumber

- Edjabou et al. (2015), *Municipal solid waste composition: Sampling methodology, statistical analyses, and case study evaluation*, terutama Section 2.4–2.5 dan Tables 2–4: https://backend.orbit.dtu.dk/ws/files/119653490/Manuscript.pdf
- EU WEEE Directive 2012/19/EU: https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=celex%3A02012L0019-20240408
- RIPS masing-masing kota: dicatat pada kolom `source_pdf` dan `source_pages` di keluaran.

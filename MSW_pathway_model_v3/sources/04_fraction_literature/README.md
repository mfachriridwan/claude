# Data literatur untuk fraksi Level II dan III

Paket ini melengkapi basis RIPS dengan dua jenis data: parameter material untuk model dan benchmark persentase hasil pemilahan. Keduanya tidak boleh dipertukarkan.

## Berkas

- `fraction_level2_level3_parameters.csv`: 36 baris Level II dan 56 baris Level III. Memuat moisture, DOC dry, rentang DOC, DOCf, total carbon, fossil share, sumber, dan status nilai. Blank berarti belum tersedia atau tidak berlaku, bukan nol.
- `fraction_level2_level3_composition_benchmarks.csv`: angka pemilahan SF/MF Denmark yang berhasil dicocokkan dengan taksonomi saat ini. Angka merupakan % dari total residual household waste, bukan % dari induk. Kolom mean adalah rerata dua tipe rumah tanpa pembobotan populasi.
- `literature_observations.csv`: nilai tambahan yang terverifikasi dari literatur internasional dan studi plastik Malang. Semua nilai tetap memiliki satuan dan batas penerapan.

## Aturan pemakaian untuk Claude/model

1. Join menggunakan `(level, code)`, bukan nama saja. Jangan mengganti composition RIPS dengan benchmark Denmark.
2. Nilai IPCC pada subfraksi adalah proxy kategori induk. IPCC tidak mengukur 36/56 fraksi taksonomi ini secara terpisah. Simpan status proxy saat nilai dimasukkan ke database.
3. `DOC_dry_fraction` harus dikalikan massa kering. Untuk massa basah: `DOC_wet = (1 - moisture_wet_fraction) * DOC_dry_fraction`. Jangan mengalikan faktor kering dua kali.
4. `DOCf` adalah parameter degradasi anaerobik landfill, bukan potensi methane anaerobic digestion (BMP).
5. Untuk prior pembagian bersyarat: `p(child|parent) = mean_pct(child) / sum(mean_pct(children))`, hanya apabila seluruh anak induk tercakup dan basis pemilahannya sama. Jangan menormalkan subset parsial menjadi 100% tanpa menandai cakupan yang hilang.
6. Pertahankan nol pemilahan yang dilaporkan. Pseudocount untuk Dirichlet, bila dipakai, adalah keputusan pemodelan terpisah, bukan observasi sumber.
7. Moisture IPCC berlaku sebagai default global sebelum pengumpulan. Kertas/plastik terkontaminasi setelah pengumpulan dapat mempunyai moisture berbeda.
8. LHV dan tau dikosongkan di tabel rinci karena nilai subfraksi belum diverifikasi. LHV plastik 30.5 MJ/kgTS dalam observasi tambahan adalah median agregat literatur, bukan nilai untuk semua resin.

## Pemilihan sumber dan keterbatasan

Komposisi lokal RIPS tetap menjadi sumber utama untuk 21 kota/kabupaten. Bukti Indonesia tambahan ditemukan untuk plastik Malang, tetapi Malang berada di luar 21 lokasi penelitian. Studi Padang 2022 ditemukan dan ditinjau: artikel secara eksplisit menyatakan tidak ada data kalor Padang dan memakai nilai dari daerah lain. Karena itu tabel kalor artikel tersebut tidak diperlakukan sebagai hasil uji laboratorium Padang.

Artikel Indonesia tentang pyrolysis plastik juga ditemukan, tetapi tabel unsur yang terlihat mempunyai beberapa jumlah komponen melebihi 100%. Nilainya tidak diadopsi sebagai komposisi unsur terukur sebelum masalah basis dan penjumlahan diselesaikan.

Kategori tanah lembap, mineral, dan limbah campuran tidak dipaksa memakai parameter garden/food. Woody plant dan straw menggunakan proxy wood dengan DOCf 0.10. Garden campuran menggunakan proxy grass dengan catatan harus memisahkan cabang dan tanah. Nappies memakai kategori diapers IPCC; tampon dan kondom tetap kosong karena tidak identik.

Cartons berlapis, composite film, HHW, baterai, dan WEEE memerlukan proporsi bahan penyusunnya. Nilai pure plastic tidak otomatis berlaku untuk material komposit. Textile/leather/rubber campuran juga tetap kosong pada tingkat campuran; nilai baru dapat dihitung setelah pembagian bahannya diketahui.

**Taksonomi Level III saat ini provisional dan tidak mencakup seluruh terminal waste stream.** Enam subfraksi food adalah refinemen model terdahulu. Tidak ditemukan angka pemilahan yang secara langsung mendukung pembagian tiga sama besar pada masing-masing kelompok food. Kode ferrous/non-ferrous saat ini juga melintasi dua parent metal, sehingga tidak diberi satu observasi gabungan yang menghilangkan informasi induk. Table 3 artikel memakai Beverage cartons, sedangkan kode 4.3 lokal mencakup cartons/plates/cups. Observasi tidak disalin ke kode itu karena definisinya lebih luas.

Penelusuran ini menghasilkan data yang bisa digunakan dan daftar gap yang eksplisit. Ini bukan bukti bahwa tidak ada publikasi lain untuk setiap gap; lampiran numerik Riber/Götze dan studi lokal tambahan masih dibutuhkan untuk melengkapi seluruh sifat material.

## Sumber yang ditinjau

- IPCC 2006, Vol. 5 Ch. 2 Table 2.4: https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/5_Volume5/V5_2_Ch2_Waste_Data.pdf
- IPCC 2019 Refinement, Vol. 5 Ch. 3 Table 3.0: https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/5_Volume5/19R_V5_3_Ch03_SWDS.pdf
- Edjabou et al. (2015), DOI 10.1016/j.wasman.2014.11.009, Tables 2–4: https://backend.orbit.dtu.dk/ws/files/119653490/Manuscript.pdf
- Riber et al. (2009), pengukuran kimia 48 fraksi, 61 substansi; tabel numerik belum diperoleh pada penelusuran ini: https://doi.org/10.1016/j.wasman.2008.09.013
- Götze et al. (2016), review karakterisasi fisik-kimia, Section 3.2.3 untuk nilai yang diekstrak: https://doi.org/10.1016/j.wasman.2016.01.008
- Studi Malang (2026), resin plastik; pembanding Indonesia, bukan pengamatan baseline 2025 pada kota target: https://doi.org/10.1007/s11367-026-02751-9
- Raharjo & Ariska (2022), studi WtE Padang: https://ejournal.undip.ac.id/index.php/presipitasi/article/download/42960/pdf
- Studi pyrolysis Indonesia yang tidak diadopsi: https://ijtech.eng.ui.ac.id/article/view/6905

Diakses 5 Oktober 2026. Nilai baru belum diterapkan ke input kota atau kode model agar pengguna dapat meninjau asal dan kesesuaian masing-masing nilai terlebih dahulu.

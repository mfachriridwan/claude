"""Ekstraksi data Life Cycle Inventory (LCI) dari teks paper.

LCI = daftar aliran (flows) input/output suatu sistem produk: energi, material,
emisi, air, limbah, transport, dll. — masing-masing dengan jumlah dan satuan,
relatif terhadap functional unit.

Dua mode:
- Rule-based (default, murni stdlib): deteksi pola `angka + satuan` lalu
  klasifikasikan ke kategori LCI via lexicon kata kunci. Hasilnya adalah
  KANDIDAT yang perlu diverifikasi manusia, bukan LCI tervalidasi.
- LLM (opsional): kalau ANTHROPIC_API_KEY + SDK anthropic tersedia, ekstraksi
  terstruktur dilakukan oleh Claude untuk hasil yang jauh lebih akurat.
"""

from __future__ import annotations

import json
import os
import re
from collections import Counter

from .preprocess import split_sentences

# ---------------------------------------------------------------------------
# Lexicon kategori aliran LCI (Indonesia + Inggris).
# ---------------------------------------------------------------------------
CATEGORY_KEYWORDS: dict[str, list[str]] = {
    "energi": [
        "energy", "energi", "electricity", "listrik", "fuel", "bahan bakar",
        "diesel", "gasoline", "bensin", "natural gas", "gas alam", "heat",
        "panas", "steam", "uap", "power", "daya", "coal", "batubara",
        "petrol", "lpg", "biomass", "biomassa", "kerosene",
    ],
    "material": [
        "steel", "baja", "aluminium", "aluminum", "cement", "semen",
        "concrete", "beton", "plastic", "plastik", "polymer", "polimer",
        "sand", "pasir", "gravel", "kerikil", "timber", "kayu", "wood",
        "glass", "kaca", "copper", "tembaga", "iron", "besi", "limestone",
        "fertilizer", "pupuk", "raw material", "bahan baku", "feedstock",
        "resin", "paper", "kertas", "fiber", "serat",
    ],
    "emisi_udara": [
        "co2", "carbon dioxide", "karbon dioksida", "ch4", "methane", "metana",
        "n2o", "nitrous oxide", "nox", "sox", "so2", "sulfur dioxide", "co",
        "carbon monoxide", "pm", "particulate", "partikulat", "voc", "ghg",
        "greenhouse gas", "gas rumah kaca", "emission", "emisi", "co2-eq",
        "co2eq", "co2e", "global warming", "gwp", "nh3", "ammonia",
    ],
    "emisi_air": [
        "cod", "bod", "wastewater", "air limbah", "effluent", "nitrogen",
        "phosphorus", "fosfor", "fosfat", "phosphate", "eutrophication",
        "eutrofikasi", "discharge to water", "tss", "heavy metal", "logam berat",
    ],
    "air": [
        "water consumption", "konsumsi air", "water use", "penggunaan air",
        "freshwater", "air tawar", "water withdrawal", "water footprint",
        "irrigation", "irigasi", "blue water", "water demand",
    ],
    "limbah": [
        "waste", "limbah", "landfill", "tpa", "solid waste", "limbah padat",
        "hazardous waste", "limbah b3", "sludge", "lumpur", "tailings",
        "by-product", "produk samping", "scrap", "residue", "residu",
    ],
    "transport": [
        "transport", "transportasi", "freight", "angkutan", "shipping",
        "pengiriman", "trucking", "road transport", "rail", "kereta",
        "ton-km", "tonne-km", "tkm", "logistics", "logistik", "distance",
        "jarak tempuh",
    ],
    "lahan": [
        "land use", "penggunaan lahan", "land occupation", "okupasi lahan",
        "area", "luas lahan", "deforestation", "deforestasi",
    ],
}

# Satuan -> dimensi. Dipakai untuk membantu klasifikasi & validasi.
_UNIT_DIMENSION: dict[str, str] = {}
for _u in ["wh", "kwh", "mwh", "gwh", "twh", "j", "kj", "mj", "gj", "tj",
           "kcal", "btu"]:
    _UNIT_DIMENSION[_u] = "energi"
for _u in ["mg", "g", "kg", "t", "ton", "tonne", "tonnes", "tons", "mg",
           "kt", "mt", "gg"]:
    _UNIT_DIMENSION[_u] = "massa"
for _u in ["ml", "l", "litre", "liter", "litres", "liters", "m3", "dm3",
           "cm3", "kl"]:
    _UNIT_DIMENSION[_u] = "volume"
for _u in ["km", "mi", "mile", "miles"]:
    _UNIT_DIMENSION[_u] = "jarak"
for _u in ["tkm", "pkm"]:
    _UNIT_DIMENSION[_u] = "transport"
for _u in ["ha", "m2", "km2", "acre", "acres"]:
    _UNIT_DIMENSION[_u] = "lahan"

# Daftar satuan untuk regex (urut dari yang panjang ke pendek supaya greedy benar).
_UNIT_TOKENS = sorted(
    set(_UNIT_DIMENSION.keys()) | {
        "co2", "co2-eq", "co2eq", "co2e", "kgco2eq",
    },
    key=len,
    reverse=True,
)

# Angka: 1, 1.5, 1,234.5, 12 000 (spasi pemisah ribuan jarang -> abaikan), 3.2e3
_NUMBER = r"\d{1,3}(?:[.,]\d{3})*(?:\.\d+)?(?:[eE][+-]?\d+)?|\d+(?:\.\d+)?"

# Satuan komposit emisi seperti "kg CO2-eq", "t CO2 eq", "g CO2e", "kg CO2-eq/kg".
_EMISSION_UNIT = re.compile(
    r"\b(mg|g|kg|t|tonne|tonnes|ton|tons)\s*"
    r"(co2(?:[\s-]?eq|e)?|ch4|n2o|so2|nox|co2)\b",
    re.IGNORECASE,
)

# Satuan umum (mass/energy/volume/...): angka diikuti satuan, opsional "/unit".
_UNIT_ALT = "|".join(re.escape(u) for u in _UNIT_TOKENS)
_QTY = re.compile(
    rf"(?P<num>{_NUMBER})\s*"
    rf"(?P<unit>(?:{_UNIT_ALT}))"
    rf"(?P<per>\s*/\s*(?:{_UNIT_ALT}|kg|unit|fu|year|yr|ha|capita))?\b",
    re.IGNORECASE,
)

# Functional unit patterns.
_FU_PATTERNS = [
    re.compile(r"functional\s+unit\s*(?:is|was|:|of|,)?\s*([^.;\n]{3,80})", re.I),
    re.compile(r"unit\s+fungsional\s*(?:adalah|:|yaitu)?\s*([^.;\n]{3,80})", re.I),
    re.compile(r"\bper\s+(kg|tonne|t|kwh|mj|gj|m3|km|tkm|unit|capita|ha)\s+of\s+([^.;\n]{2,50})", re.I),
    re.compile(r"\b1\s*(kg|tonne|t|kwh|mj|m3)\s+of\s+([^.;\n]{2,50})", re.I),
    re.compile(r"\bper\s+(kg|tonne|ton|kwh|mj|gj|m3|km|tkm|unit)\b", re.I),
]


def _to_float(num: str) -> float | None:
    """Konversi string angka ke float, menangani pemisah ribuan ','."""
    s = num.strip()
    try:
        if "," in s and "." in s:
            s = s.replace(",", "")          # 1,234.5 -> 1234.5
        elif "," in s:
            # Ambigu: '1,5' (desimal Eropa) vs '1,500' (ribuan). Heuristik:
            # 3 digit setelah koma -> ribuan, selainnya -> desimal.
            if re.match(r"^\d{1,3},\d{3}$", s):
                s = s.replace(",", "")
            else:
                s = s.replace(",", ".")
        return float(s)
    except ValueError:
        return None


def _keyword_distance(ctx: str, kw: str, pos: int | None) -> int | None:
    """Jarak terkecil dari posisi angka ke kemunculan kata kunci di ctx.

    Jarak diukur ke tepi terdekat span kata kunci. Return None bila kw tak ada.
    Bila pos None, semua kemunculan dianggap berjarak 0 (mode tanpa posisi).
    """
    idx = ctx.find(kw)
    if idx == -1:
        return None
    if pos is None:
        return 0
    best: int | None = None
    while idx != -1:
        start, end = idx, idx + len(kw)
        if pos < start:
            dist = start - pos
        elif pos > end:
            dist = pos - end
        else:
            dist = 0
        if best is None or dist < best:
            best = dist
        idx = ctx.find(kw, idx + 1)
    return best


def _classify(unit: str, context: str, pos: int | None = None) -> tuple[str, str | None]:
    """Tentukan (kategori, substansi) dari satuan + konteks.

    Bila `pos` (posisi angka dalam konteks) diberikan, kategori dipilih
    berdasarkan kata kunci yang PALING DEKAT dengan angka — supaya beberapa
    angka di kalimat yang sama tidak tertarik ke satu kategori dominan.
    """
    u = unit.lower()
    ctx = context.lower()

    # Satuan emisi eksplisit selalu emisi udara.
    if "co2" in u or "ch4" in u or "n2o" in u:
        return "emisi_udara", u

    # Cari kata kunci terdekat lintas semua kategori.
    best_cat: str | None = None
    best_dist: int | None = None
    best_kw: str | None = None
    for cat, kws in CATEGORY_KEYWORDS.items():
        for kw in kws:
            dist = _keyword_distance(ctx, kw, pos)
            if dist is None:
                continue
            if best_dist is None or dist < best_dist:
                best_dist, best_cat, best_kw = dist, cat, kw

    dim = _UNIT_DIMENSION.get(u)

    # Satuan energi: kalau kategori energi termasuk kandidat & cukup dekat,
    # utamakan energi (mis. "energy" sedikit lebih jauh dari kata lain).
    if dim == "energi" and best_cat != "energi":
        e_dist = min(
            (d for d in (_keyword_distance(ctx, kw, pos)
                         for kw in CATEGORY_KEYWORDS["energi"]) if d is not None),
            default=None,
        )
        if e_dist is not None and (best_dist is None or e_dist <= best_dist + 5):
            return "energi", None

    if best_cat is not None:
        return best_cat, best_kw

    # Tanpa kata kunci: jatuhkan ke kategori berdasarkan dimensi satuan.
    if dim == "energi":
        return "energi", None
    if dim == "transport":
        return "transport", None
    if dim == "lahan":
        return "lahan", None
    if dim in ("massa", "volume"):
        return "material", None
    return "lainnya", None


def detect_functional_unit(text: str) -> str | None:
    """Coba deteksi functional unit dari teks."""
    head = text[:8000]  # biasanya disebut di awal (abstract/metodologi)
    for pat in _FU_PATTERNS:
        m = pat.search(head)
        if m:
            return m.group(0).strip().rstrip(".,;")
    return None


def extract_flows_rulebased(text: str, max_per_category: int = 0) -> list[dict]:
    """Ekstrak kandidat aliran LCI berbasis aturan.

    Mengembalikan daftar dict: {nilai, satuan, per, kategori, substansi, konteks}.
    """
    flows: list[dict] = []
    seen: set[tuple] = set()

    for sentence in split_sentences(text):
        # (num, unit, per, posisi) — posisi dipakai untuk jendela klasifikasi lokal.
        matches: list[tuple[str, str, str, int]] = []

        # Satuan emisi komposit lebih dulu (mis. "12 kg CO2-eq").
        emisi_pos: set[int] = set()
        for m in _EMISSION_UNIT.finditer(sentence):
            # Cari angka tepat sebelum match.
            prefix = sentence[: m.start()]
            num_m = re.search(rf"({_NUMBER})\s*$", prefix)
            if num_m:
                emisi_pos.add(num_m.start(1))
                matches.append((num_m.group(1),
                                f"{m.group(1)} {m.group(2)}".lower(), "",
                                num_m.start(1)))

        for m in _QTY.finditer(sentence):
            # Lewati bila angka ini sudah tercakup match emisi komposit.
            if m.start() in emisi_pos:
                continue
            matches.append((m.group("num"), m.group("unit"),
                            (m.group("per") or "").replace(" ", ""), m.start()))

        for num, unit, per, pos in matches:
            value = _to_float(num)
            if value is None:
                continue
            # Klasifikasi pakai kalimat penuh + posisi angka, sehingga kata
            # kunci kategori yang paling dekat dengan angka yang menang.
            kategori, substansi = _classify(unit, sentence, pos)
            key = (round(value, 4), unit.lower(), kategori)
            if key in seen:
                continue
            seen.add(key)
            flows.append({
                "nilai": value,
                "satuan": unit + (f" {per}" if per else ""),
                "kategori": kategori,
                "substansi": substansi or "",
                "konteks": sentence.strip()[:300],
            })

    if max_per_category > 0:
        by_cat: dict[str, list[dict]] = {}
        for f in flows:
            by_cat.setdefault(f["kategori"], []).append(f)
        flows = []
        for cat_flows in by_cat.values():
            flows.extend(cat_flows[:max_per_category])

    return flows


# ---------------------------------------------------------------------------
# Ekstraksi via Claude (opsional).
# ---------------------------------------------------------------------------
_LLM_PROMPT = """Anda adalah asisten LCA. Ekstrak data Life Cycle Inventory \
(LCI) dari teks paper berikut. Kembalikan HANYA JSON valid dengan struktur:

{{
  "functional_unit": "string atau null",
  "system_boundary": "string atau null",
  "flows": [
    {{
      "nama": "nama aliran/substansi",
      "tipe": "input | output",
      "kategori": "energi | material | emisi_udara | emisi_air | air | limbah | transport | lahan | lainnya",
      "nilai": angka,
      "satuan": "string",
      "kompartemen": "string atau null"
    }}
  ]
}}

Hanya sertakan data kuantitatif yang benar-benar disebut di teks. Jangan mengarang.
Jika tidak ada angka, kembalikan flows: [].

TEKS:
{text}
"""


def extract_flows_llm(text: str, model: str = "claude-sonnet-4-6",
                      max_chars: int = 100_000) -> dict | None:
    """Ekstraksi LCI terstruktur via Claude. Return None bila tidak tersedia."""
    if not os.environ.get("ANTHROPIC_API_KEY"):
        return None
    try:
        import anthropic
    except ImportError:
        return None

    try:
        client = anthropic.Anthropic()
        msg = client.messages.create(
            model=model,
            max_tokens=4096,
            messages=[{
                "role": "user",
                "content": _LLM_PROMPT.format(text=text[:max_chars]),
            }],
        )
        raw = "".join(
            b.text for b in msg.content if getattr(b, "type", "") == "text"
        ).strip()
        # Buang pagar kode ```json bila ada.
        raw = re.sub(r"^```(?:json)?|```$", "", raw, flags=re.MULTILINE).strip()
        return json.loads(raw)
    except Exception:  # pragma: no cover - bergantung jaringan/SDK
        return None


def extract_lci(text: str, use_llm: bool = False) -> dict:
    """Ekstrak LCI dari teks. Pakai Claude bila use_llm & tersedia, selain itu
    rule-based.

    Return dict: {functional_unit, system_boundary, metode, flows, ringkasan}.
    """
    llm_result = extract_flows_llm(text) if use_llm else None

    if llm_result is not None:
        flows = []
        for f in llm_result.get("flows", []):
            flows.append({
                "nilai": f.get("nilai"),
                "satuan": f.get("satuan", ""),
                "kategori": f.get("kategori", "lainnya"),
                "substansi": f.get("nama", ""),
                "tipe": f.get("tipe", ""),
                "kompartemen": f.get("kompartemen") or "",
                "konteks": "",
            })
        fu = llm_result.get("functional_unit") or detect_functional_unit(text)
        boundary = llm_result.get("system_boundary")
        metode = "llm"
    else:
        flows = extract_flows_rulebased(text)
        fu = detect_functional_unit(text)
        boundary = None
        metode = "rule-based"

    ringkasan = Counter(f["kategori"] for f in flows)
    return {
        "functional_unit": fu,
        "system_boundary": boundary,
        "metode": metode,
        "flows": flows,
        "ringkasan_kategori": dict(ringkasan),
        "jumlah_flow": len(flows),
    }


# ---------------------------------------------------------------------------
# Ekspor.
# ---------------------------------------------------------------------------
LCI_CSV_FIELDS = ["kategori", "substansi", "nilai", "satuan", "tipe",
                  "kompartemen", "konteks"]


def write_lci_csv(flows: list[dict], output_path: str) -> None:
    """Tulis daftar flow LCI ke CSV."""
    import csv

    with open(output_path, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=LCI_CSV_FIELDS,
                                extrasaction="ignore")
        writer.writeheader()
        for flow in flows:
            row = {k: flow.get(k, "") for k in LCI_CSV_FIELDS}
            writer.writerow(row)

"""Tes untuk ekstraksi Life Cycle Inventory (rule-based)."""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from doknlp import (  # noqa: E402
    extract_lci,
    extract_flows_rulebased,
    detect_functional_unit,
    write_lci_csv,
    LCI_CSV_FIELDS,
)
from doknlp.lci import _to_float, _classify  # noqa: E402

PAPER = (
    "The functional unit is 1 kg of produced steel. "
    "System boundary is cradle-to-gate. "
    "Production of 1 kg steel requires 20 MJ of energy and 1.5 kg of iron ore. "
    "Electricity consumption was 4.2 kWh per kg. "
    "The process emits 1.8 kg CO2-eq per kg of steel. "
    "It also releases 0.05 kg CH4 to the atmosphere. "
    "Water consumption is 12 L and 0.3 kg of solid waste is generated. "
    "Transport of raw materials accounts for 150 tkm."
)


def test_to_float_variants():
    assert _to_float("1.5") == 1.5
    assert _to_float("1,234.5") == 1234.5
    assert _to_float("1,500") == 1500.0   # ribuan
    assert _to_float("3,5") == 3.5         # desimal Eropa
    assert _to_float("abc") is None


def test_detect_functional_unit():
    fu = detect_functional_unit(PAPER)
    assert fu is not None
    assert "kg" in fu.lower()


def test_extract_flows_finds_energy():
    flows = extract_flows_rulebased(PAPER)
    energi = [f for f in flows if f["kategori"] == "energi"]
    units = {f["satuan"].split()[0].lower() for f in energi}
    assert "mj" in units or "kwh" in units


def test_extract_flows_classifies_emissions():
    flows = extract_flows_rulebased(PAPER)
    emisi = [f for f in flows if f["kategori"] == "emisi_udara"]
    assert any("co2" in f["satuan"].lower() for f in emisi)


def test_extract_flows_water_and_waste_and_transport():
    flows = extract_flows_rulebased(PAPER)
    cats = {f["kategori"] for f in flows}
    assert "air" in cats          # water consumption 12 L
    assert "limbah" in cats       # solid waste
    assert "transport" in cats    # 150 tkm


def test_classify_emission_unit():
    cat, subst = _classify("kg co2-eq", "emits per kg of steel")
    assert cat == "emisi_udara"


def test_classify_energy_with_context():
    cat, _ = _classify("kwh", "electricity consumption was high")
    assert cat == "energi"


def test_extract_lci_structure():
    result = extract_lci(PAPER)
    assert result["metode"] == "rule-based"
    assert result["jumlah_flow"] == len(result["flows"])
    assert result["jumlah_flow"] > 0
    assert isinstance(result["ringkasan_kategori"], dict)
    assert result["functional_unit"]


def test_extract_lci_empty_text():
    result = extract_lci("Tidak ada angka kuantitatif di sini sama sekali.")
    assert result["jumlah_flow"] == 0
    assert result["flows"] == []


def test_write_lci_csv(tmp_path):
    flows = extract_flows_rulebased(PAPER)
    out = tmp_path / "lci.csv"
    write_lci_csv(flows, str(out))
    lines = out.read_text(encoding="utf-8").strip().splitlines()
    assert lines[0].split(",")[0] == LCI_CSV_FIELDS[0]  # header kategori
    assert len(lines) == len(flows) + 1


def test_no_duplicate_flows():
    # Nilai+satuan+kategori yang sama tidak digandakan.
    text = "Energy use is 20 MJ. Again, 20 MJ of energy is needed."
    flows = extract_flows_rulebased(text)
    keys = [(f["nilai"], f["satuan"], f["kategori"]) for f in flows]
    assert len(keys) == len(set(keys))

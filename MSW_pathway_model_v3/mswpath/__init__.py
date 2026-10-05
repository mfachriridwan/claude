"""mswpath: provenance-aware screening of MSW recovery pathways for Indonesian cities (v3.2)."""
from .core import MSWModel, read_input, PW, NAMES, SCENARIOS, INDICATORS, CARBON_VALUES

__version__ = "3.2.0"
__all__ = ["MSWModel", "read_input", "PW", "NAMES", "SCENARIOS", "INDICATORS", "CARBON_VALUES", "__version__"]

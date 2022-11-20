from dataclasses import dataclass

import simple_dataclass as SimpleDataclass

@dataclass(frozen=True, eq=True)
class DataclassWithImport:
    simple_dataclass: SimpleDataclass.SimpleDataclass


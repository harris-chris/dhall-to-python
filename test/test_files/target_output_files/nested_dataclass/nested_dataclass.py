from dataclasses import dataclass

@dataclass(frozen=True, eq=True)
class Inner:
    var_str: str
    var_nat: int

@dataclass(frozen=True, eq=True)
class Nested:
    var_dbl: float
    inner: Inner

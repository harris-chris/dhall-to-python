from dataclasses import dataclass

@dataclass(frozen=True, eq=True)
class SimpleDataclass:
    var_nat: int
    var_dbl: float

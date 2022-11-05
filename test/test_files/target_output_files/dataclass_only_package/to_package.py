from dataclasses import dataclass

@dataclass(frozen=True, eq=True)
class ToDataclass:
    var_nat: int
    var_dbl: float

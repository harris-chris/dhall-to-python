let Inner = {
    var_str : Text
    , var_nat : Natural
}

let Nested = {
    var_dbl : Double
    , inner : Inner
}

let NestedDataclass = {
    Nested
} in NestedDataclass


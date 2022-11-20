let SimpleDataclass = ./simple_dataclass.dhall

let DataclassWithImport = {
    simple_dataclass : SimpleDataclass.SimpleDataclass
}

let DataclassWithImport = {
    DataclassWithImport
} in DataclassWithImport

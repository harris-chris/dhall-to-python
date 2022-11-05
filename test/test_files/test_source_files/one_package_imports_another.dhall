let SimpleDataclass = ./dataclass_only_package.dhall

let DataclassWithImport = {
    simple_data_class : SimpleDataclass.SimpleDataclass
}

let DataclassWithImport = {
    DataclassWithImport
} in DataclassWithImport

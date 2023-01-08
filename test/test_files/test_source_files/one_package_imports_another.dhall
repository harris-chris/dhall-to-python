let SimpleDataclass = ./simple_dataclass.dhall

let DataclassWithImport = {
    simple_dataclass : SimpleDataclass.SimpleDataclass
}

let DataclassWithImportPackage = {
    DataclassWithImport
} in DataclassWithImportPackage

let DataclassWithImportPackage = ./one_package_imports_another.dhall

let DataclassWithimportWithinImport = {
    DataclassWithImport : DataclassWithImportPackage.DataclassWithImport
} in DataclassWithimportWithinImport


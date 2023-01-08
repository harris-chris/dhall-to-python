
The dhall structure is file-system agnostic, but the python output is not
We want to go:
Dhall file -> DhallExprExpanded (not IO) -- has no imports
DhallExprExpanded -> PythonPackage (not IO)
write PythonPackage

Does the dhall file structure actually matter, is that what we want to base the python file structure on? Instead we could be looking for records of types (think this is the `Record` type).
Start at end-expression point
What is a dataclass? This also is a record of types
So how to distinguish between packages and data classes?
Do we say that the end-expression Record is a Package, and

-- str_package.dhall:
let StrDataclass = {
    str : Text
} -- end-1 expression `Record`, must be a dataclass

let StrPackage = {
    StrDataclass
} -- end-0 expression `Record`, must be a package

-- end_package.dhall file:
let StrPackage = ./str_package.dhall -- end-2 expression `Record`, this must be an import

let IntStrDataclass = {
    int : Natural
    str_class : StrPackage.StrDataclass
} -- end-1 expression `Record`, this must be a dataclass

let EndPackage = {
    MyDataclass
} -- end-0 expression `Record`, this must be a package



we need some distinction between a package, as in a record, and a package as in a file which contains other stuff
Maybe best to have an 'Import' type that has filename, main expression, and other ParsedObjs in that file.

Note that if we parse the files that are found by import statements as we come across them, the whole program is going to be in an IO Monad. Not sure to what extent that's a problem.
The alternative would be to go through the entire file structure and record it as some intermediary state, like a tree of Expr s a. This tree would represent the dhall file tree.
That sounds neater.

Are we sure there isn't some way to have Dhall do that itself?
Yes - there is the load function, though it returns IOs.
Can we just clear out those IOs

So we go through

What's going on here:
- In the dhall code, the objects are nested within one another.
- If there's an deepest let binding, this is our "package declaration"
- Other innermost objects, not clear how to treat them yet.
- A dataclass is a let binding with a record as its value
- There should only ever be one non-let-binding object per dhall file (is this true?)
- So handling let bindings is the mainh part of the work here. To handle let bindings:
  - Ignore functions
  - Ignore and warn on literals
  - Do collect:
    - Records ( = dataclasses)
    - Imports ( = packages )
    -
  - Recursively bring in imports, each import should return a PythonObj which is likely a pythonPackage. If we are to re-create the original file structure, we need to keep a record of where a python package was imported.
So our end data structure is like:
- A single python package, which contains
  - records/dataclasses,
  - other python packages (do we keep a record of whether they were imported?)
  - maybe some simple types


**Nix flake for an example haskell package**

To get started:
- In the `package.yaml`, set any names beginning with `example` to your chosen name - for example, change the `name` setting on the first line from `example-package` to the nameof your new package.
- Run `hpack` from the terminal; this will generate a cabal file for your project.
- Run `cabal test` or `cabal run ${executable-name}`, where `${executable-name}` is the name of your executable, defined under `executables` in the `package.yaml`.

**Overview of what's here**
This flake has a simple executable, a library that that executable depends on, and a test suite which tests the library.

It uses cabal to build, hpack as the format for the cabal project specification, and hspec to run tests.

Remember to run hpack after any changes to the `package.yaml`, in order to re-create the `${project-name}.cabal` file, as well as the first time you try to use this.

The executable can be run via cabal using `cabal run example-executable`, whereas tests can be run via `cabal test`.


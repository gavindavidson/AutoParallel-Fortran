# AutoParallel-Fortran
Gavin Davidson's Masters computing science project. Domain specific, automatically parallelising fortran compiler that takes scientific Fortran as input and produces paralell Fortran/OpenCL.

Language-Fortran, a Haskell based Fortran parser, is used by this project. The original parser is available at https://github.com/dagit/language-fortran.

## Installation

This project requires a Haskell compiler that is at least GHC version 6.0. Otherwise, installation is very simple:

```bash
cd compiler
make
```

## Use

The compiler is a purely command line tool and offers to graphical interface. There are a number of command line arguments that may be provided but only one is necessary: the path to the target file. For example, to run the compiler on a file named `target.f95`, located in the same directory as the compiler itself:

```bash
cd compiler
./AutoParallel-Fortran target.f95
```

The `target.f95` could be replaced with an absolute or relative path if the file was located elsewhere.

The optional command line flags are are follows:
- *-out* defines the filename for the output host program. The name of the kernel file is derived from this filename. Not including this argument results in a default filename being used.
- *-lfb* defines a value for the loop fusion bound. That is, the difference in iterator end value that is allowed for two loops to be fused. Not including this argument results in there not being a bound for loop fusion, and therefore all pairs of loops that meet the other conditions are fused.

For example, to compile `target.f95` with a loop fusion bound of 10 and an output filename of `out.f95`:

```bash
cd compiler
./AutoParallel-Fortran target.f95 -out out.f95 -lfb 10
```

## Modification

The parser used by this project was generated from a grammar file using the Happy parser generator. Therefore, making changes to the parser (Parser.y or Lexer.x) may require the installation of happy. Please refer to the readme in the original Language-Fortran repo for details on the installation of happy.

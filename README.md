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

The compiler is a purely command line tool and offers no graphical interface. There are a number of command line arguments that may be provided but only one is necessary: the path to the target file(s). For example, to run the compiler on a file named `target.f95`, located in the same directory as the compiler itself:

```bash
cd compiler
./AutoParallel-Fortran target.f95
```

The `target.f95` could be replaced with an absolute or relative path if the file was located elsewhere. Supplying more than one filename to the compiler as arguments will cause it to consider both as part of the same program and produce a super kernel style program, combining the kernels that would be produced by each source file into one super kernel.

The optional command line flags are are follows:
- *-out* defines the directory where the output program will be saved. The name of the kernel file is derived from the original filenames. Not including this argument results in a the output being saved in the current directory.
- *-lfb* defines a value for the loop fusion bound. That is, the difference in iterator end value that is allowed for two loops to be fused. Not including this argument results in there not being a bound for loop fusion, and therefore all pairs of loops that meet the other conditions are fused.
- *-D* defines a list of c preprocessor (cpp) macros that are to be defined.
- *-v* enables verbose mode in which obstacles to parallelisation are reported to the user.
- *-ffixed-form* enforces that input lines must be no more than 72 characters long. Output is also formatted as fixed for (6 leading spaces on each line and no more than 72 characters per line).

## Modification

The parser used by this project was generated from a grammar file using the Happy parser generator. Therefore, making changes to the parser (Parser.y or Lexer.x) may require the installation of happy. Please refer to the readme in the original Language-Fortran repo for details on the installation of happy.

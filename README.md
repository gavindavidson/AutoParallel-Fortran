# AutoParallel-Fortran
Gavin Davidson's Masters computing science project. Domain specific, automatically parallelising fortran compiler that augments fortran code aimed at large eddy simulation with OpenCL.

Language-Fortran, a Haskell based Fortran parser, is used by this project. The original parser is available at https://github.com/dagit/language-fortran

==	EXTENSIONS TO LANGUAGE-FORTRAN 	==
	- SELECT CASE statements are now supported. However, there is a bug whereby the first CASE statement must have whitespace in front of it.
ghc Transformer.hs -i../language-fortran/src/ -XTemplateHaskell
./Transformer ../testFiles/adam.f95 > treeWalker.out
python ../utils/astFormatter.py treeWalker.out > treeWalker.ast
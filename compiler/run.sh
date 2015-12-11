ghc Transformer.hs -i../language-fortran/src/ -XTemplateHaskell
./Transformer > treeWalker.out
python ../utils/astFormatter.py treeWalker.out > treeWalker.ast
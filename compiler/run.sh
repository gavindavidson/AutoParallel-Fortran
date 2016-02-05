folder="../testFiles"
filename="press.f95"

originalfile=$folder"/"$filename
parfile=$folder"/par_"$filename

echo "Running on "$originalfile

ghc Transformer.hs -i../language-fortran/src/ -XTemplateHaskell
./Transformer $originalfile
python ../utils/astFormatter.py $parfile > treeWalker.ast
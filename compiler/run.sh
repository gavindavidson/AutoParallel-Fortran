folder="../testFiles"
filename="arrayLoop.f95"

originalfile=$folder"/"$filename
parfile=$folder"/par_"$filename

ghc Transformer.hs -i../language-fortran/src/ -XTemplateHaskell

echo ""
echo "Running on "$originalfile
echo ""
./Transformer $originalfile
python ../utils/astFormatter.py $parfile > treeWalker.ast
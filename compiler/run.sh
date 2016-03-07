folder="../testFiles"
filename="arrayLoop.f95"
outfilename="arrayLoop_out.f95"

originalfile=$folder"/"$filename
outfile=$folder"/"$outfilename
# parfile=$folder"/par_"$filename

echo ""
echo "Running on "$originalfile
echo ""
./Transformer $originalfile $outfile
# ./Transformer $originalfile $outfile > treeWalker.out
# python ../utils/astFormatter.py treeWalker.out > treeWalker.ast
# python ../utils/astFormatter.py $parfile > treeWalker.ast
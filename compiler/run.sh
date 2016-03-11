folder="../testFiles"
filename="press.f95"
outfilename="press_out.f95"

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
folder="../codeBase_tests"
filename="velnw.f95"
# filename="press.f95"
# ../codeBase_tests/adam.f95  
#../codeBase_tests/bondv1.f95  
#../codeBase_tests/feedbf.f95	
#../codeBase_tests/les.f95  
#../codeBase_tests/press.f95	
#../codeBase_tests/velFG.f95  
#../codeBase_tests/velnw.f95

originalfile=$folder"/"$filename
outfile=$folder"/"$outfilename
# parfile=$folder"/par_"$filename

echo ""
echo "Running on "$originalfile
echo ""
# ./Transformer $originalfile
./Transformer $originalfile
# ./Transformer +RTS -h -RTS $originalfile $outfile > treeWalker.out
# hp2ps -c Transformer.hp
# python changes.py $originalfile $outfile
# python ../utils/astFormatter.py treeWalker.out > treeWalker.ast
# python ../utils/astFormatter.py $parfile > treeWalker.ast
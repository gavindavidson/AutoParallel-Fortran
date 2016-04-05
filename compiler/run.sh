folder="../testFiles"
# filename="params_common_sn.f95"
filename="press.f95"
# # ../codeBase_tests/adam.f95  
# #../codeBase_tests/bondv1.f95  
# #../codeBase_tests/feedbf.f95	
# #../codeBase_tests/les.f95  
# #../codeBase_tests/press.f95	
# #../codeBase_tests/velFG.f95  
# #../codeBase_tests/velnw.f95

originalfile=$folder"/"$filename
outfile=$folder"/"$outfilename
# parfile=$folder"/par_"$filename

echo ""
echo "Running on "$originalfile
echo ""
# ./Transformer $originalfile +RTS -h -RTS
# ./Transformer $originalfile
./Transformer $originalfile > treeWalker.out
# ./Transformer +RTS -h -RTS $originalfile $outfile > treeWalker.out
# hp2ps -c Transformer.hp
# python changes.py $originalfile $outfile
python ../utils/astFormatter.py treeWalker.out > treeWalker.ast
# python ../utils/astFormatter.py $parfile > treeWalker.ast

# ../codeBase_tests/adam.f95  
#../codeBase_tests/bondv1.f95  
#../codeBase_tests/feedbf.f95	
#../codeBase_tests/les.f95  
#../codeBase_tests/press.f95	
#../codeBase_tests/velFG.f95  
#../codeBase_tests/velnw.f95
#../codeBase_tests/vel2.f95

# filename="../codeBase_tests/adam.f95"
# outfilename="../codeBase_tests/par_adam.f95"
# echo ""
# echo "Running on "$filename
# ./Transformer $filename $outfilename > nowhere
# python changes.py $filename $outfilename

# filename="../codeBase_tests/bondv1.f95"
# outfilename="../codeBase_tests/par_bondv1.f95"
# echo ""
# echo "Running on "$filename
# ./Transformer $filename $outfilename > nowhere
# python changes.py $filename $outfilename

# filename="../codeBase_tests/feedbf.f95"
# outfilename="../codeBase_tests/par_feedbf.f95"
# echo ""
# echo "Running on "$filename
# ./Transformer $filename $outfilename > nowhere
# python changes.py $filename $outfilename

# filename="../codeBase_tests/les.f95"
# outfilename="../codeBase_tests/par_les.f95"
# echo ""
# echo "Running on "$filename
# ./Transformer $filename $outfilename > nowhere
# python changes.py $filename $outfilename

# filename="../codeBase_tests/press.f95"
# outfilename="../codeBase_tests/par_press.f95"
# echo ""
# echo "Running on "$filename
# ./Transformer $filename $outfilename > nowhere
# python changes.py $filename $outfilename

# filename="../codeBase_tests/velFG.f95"
# outfilename="../codeBase_tests/par_velFG.f95"
# echo ""
# echo "Running on "$filename
# ./Transformer $filename $outfilename > nowhere
# python changes.py $filename $outfilename

# filename="../codeBase_tests/velnw.f95"
# outfilename="../codeBase_tests/par_velnw.f95"
# echo ""
# echo "Running on "$filename
# ./Transformer $filename $outfilename > nowhere
# python changes.py $filename $outfilename

# filename="../codeBase_tests/vel2.f95"
# outfilename="../codeBase_tests/par_vel2.f95"
# echo ""
# echo "Running on "$filename
# ./Transformer $filename $outfilename > nowhere
# python changes.py $filename $outfilename
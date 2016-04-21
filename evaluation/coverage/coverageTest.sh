#	TEST SCRIPT FOR LES CODEBASE

COMPILER="../../compiler/AutoParallel-Fortran"
CHANGESPY="changes.py"
RESULTDIR="compilerResult"
RESULTFILE="$RESULTDIR/compilerResults.txt"
OUTEXT=".console"

mkdir $RESULTDIR
echo "---" >> "$RESULTFILE"
date >> "$RESULTFILE"
python "$CHANGESPY" "-h" >> "$RESULTFILE"
for f in *.f95
do
	echo -e "Processing: $f"
	OUTFILE="par_$f"
	$COMPILER "$f" "-out" "$RESULTDIR/$OUTFILE" "-D" "NO_IO" > "$RESULTDIR/$OUTFILE$OUTEXT"
	if [ -e "$RESULTDIR/$OUTFILE" ]
		then
		# python "$CHANGESPY" "$f" "$RESULTDIR/$OUTFILE" >> "$RESULTFILE"
		python "$CHANGESPY" "$f" "$RESULTDIR/$OUTFILE" "-csv" >> "$RESULTFILE"
		#echo "---" >> "$RESULTFILE"
	fi
done
echo "---" >> "$RESULTFILE"

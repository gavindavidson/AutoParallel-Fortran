#	TEST SCRIPT FOR LES CODEBASE

COMPILER="../compiler/Transformer"
CHANGESPY="../compiler/changes.py"
RESULTDIR="compilerResult"
RESULTFILE="$RESULTDIR/compilerResults.txt"
OUTEXT=".console"

mkdir $RESULTDIR
for f in *.f95
do
	echo -e "Processing: $f"
	OUTFILE="par_$f"
	$COMPILER "$f" "-out" "$RESULTDIR/$OUTFILE" > "$RESULTDIR/$OUTFILE$OUTEXT"
	if [ -e "$RESULTDIR/$OUTFILE" ]
		then
		python "$CHANGESPY" "$f" "$RESULTDIR/$OUTFILE" >> "$RESULTFILE"
		# python "$CHANGESPY" "$f" "$RESULTDIR/$OUTFILE"
		echo "---" >> "$RESULTFILE"
	fi
done
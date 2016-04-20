small_test=(1024 2048 4096 8192 10240 12288 14336 16384 18432 20480)
big_test=(10240 20480 40960 81920 102400 122880 143360 163840 184320 204800)
huge_test=(1024000 2048000 4096000 8192000 10240000 12288000 14336000 16384000 18432000 20480000)
runs=20

echo "" >> testScript_sum_out.txt
echo "---" >> testScript_sum_out.txt

echo "" >> testScript_mult_out.txt
echo "---" >> testScript_mult_out.txt
date >> testScript_sum_out.txt
date >> testScript_mult_out.txt

echo "Small tests started at " `date`
for test_case in ${small_test[@]}
do
	echo -e "\tTest: " $test_case
	for i in `seq 1 $runs`
	do
		./multiply_host $test_case 	>> testScript_mult_out.txt
		./sum_host $test_case		>> testScript_sum_out.txt
	done
	echo "" >> testScript_mult_out.txt
	echo "" >> testScript_sum_out.txt
done

echo "Big tests started at " `date`
for test_case in ${big_test[@]}
do
	echo -e "\tTest: " $test_case
	for i in `seq 1 $runs`
	do
		./multiply_host $test_case 	>> testScript_mult_out.txt
		./sum_host $test_case		>> testScript_sum_out.txt
	done
	echo "" >> testScript_mult_out.txt
        echo "" >> testScript_sum_out.txt

done

echo "Huge tests started at " `date`
for test_case in ${huge_test[@]}
do
	echo -e "\tTest: " $test_case
	for i in `seq 1 $runs`
	do
		./multiply_host $test_case 	>> testScript_mult_out.txt
		./sum_host $test_case		>> testScript_sum_out.txt
	done
	echo "" >> testScript_mult_out.txt
        echo "" >> testScript_sum_out.txt
done

echo "---" >> testScript_sum_out.txt
echo "" >> testScript_sum_out.txt

echo "---" >> testScript_mult_out.txt
echo "" >> testScript_mult_out.txt

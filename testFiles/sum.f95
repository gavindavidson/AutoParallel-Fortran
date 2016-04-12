#define size 512
program sum

	integer, dimension(size) :: a

	integer :: total

	do i = 1, size
		a(i) = i-1
	end do

	total = 0
	do i = 1, size
		total = total + a(i)
	end do
	a = total

	print *, "values: ", size
	print *, "total: ", total

end program sum
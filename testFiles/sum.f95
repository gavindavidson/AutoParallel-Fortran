<<<<<<< HEAD
module sum
 contains
	subroutine fortransum(input_array, tsize, result_out) bind(C,name='fortransum')

		integer :: tsize
		integer :: result_out
		integer, dimension(tsize) :: input_array
		result_out = 0
		do i = 1,tsize
			result_out = result_out + input_array(i)
		end do
	end subroutine fortransum

end module sum

! program sum
! 	integer, dimension(size) :: a

! 	integer :: total

! 	do i = 1, size
! 		a(i) = i-1
! 	end do
! 	total = 0
! 	do i = 1, size
! 		total = total + a(i)
! 	end do
! 	a = total

! 	print *, "values: ", size
! 	print *, "total: ", total
! end program sum

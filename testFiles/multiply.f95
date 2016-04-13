module multiply
 contains
	subroutine fortranmultiply(input_array, factor, tsize, result_array) bind(C,name='fortranmultiply')

		integer :: tsize
		integer :: factor
		integer, dimension(tsize) :: input_array
		integer, dimension(tsize) :: result_array
		do i = 1,tsize
			result_array(i) = input_array(i)*factor
		end do
	end subroutine fortranmultiply

end module multiply
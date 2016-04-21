module module_kernels_par_multiply

    contains

subroutine map_result_array_8(input_array,factor,tsize,result_array)

    integer, Dimension(tsize), Intent(In) :: input_array
    integer, Intent(In) :: factor
    integer, Intent(In) :: tsize
    integer, Dimension(tsize), Intent(Out) :: result_array
    integer :: global_id
    call get_global_id(global_id,0)

    ! ParallelFortran: Synthesised loop variables
    i_rel = global_id
    i = (i_rel + 1)


    ! ParallelFortran: Original code
   result_array(i) = input_array(i)*factor

end subroutine map_result_array_8


end module module_kernels_par_multiply
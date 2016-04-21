module module_kernels_par_sum

    contains

subroutine reduce_result_out_8(input_array,tsize,global_result_out_array)

    integer :: chunk_size
    integer :: local_id
    integer :: local_id_fortran
    integer :: group_id
    integer :: group_id_fortran
    integer :: global_id
    integer :: r_iter
    integer :: local_chunk_size
    integer :: start_position

    integer, Dimension(tsize), Intent(In) :: input_array
    integer, Intent(In) :: tsize

    ! Arrays prefixed with 'local_' should be declared using the '__local' modifier in C kernel version
    integer, Dimension(1:NTH) :: local_result_out_array
    integer, Dimension(1:NUNITS), Intent(Out) :: global_result_out_array
    integer :: local_result_out

    call get_local_id(local_id,0)
    call get_group_id(group_id,0)
    call get_global_id(global_id,0)

    ! local_id_fortran and group_id_fortran are used to reconcile the fact that fortran arrays are referenced from 1
    ! not 0 like other OpenCL supporting languages
    local_id_fortran = (local_id + 1)
    group_id_fortran = (group_id + 1)
    local_chunk_size = ((tsize / NTH) / NUNITS)
    start_position = local_chunk_size * global_id
    local_result_out = 0

    do r_iter=start_position, ((start_position + local_chunk_size) - 1)
        i_rel = r_iter
        i = (i_rel + 1)
        local_result_out = (local_result_out + input_array(i))
    end do

    local_result_out_array(local_id_fortran) = local_result_out

    call barrier(CLK_LOCAL_MEM_FENCE)

    local_result_out = 0
    do r_iter=1, NTH
        local_result_out = (local_result_out + local_result_out_array(r_iter))
    end do
    global_result_out_array(group_id_fortran) = local_result_out

end subroutine reduce_result_out_8


end module module_kernels_par_sum
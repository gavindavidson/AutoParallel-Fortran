

__kernel void reduce_result_out_8(__global int *input_array, int tsize,__global int *global_result_out_array) {
  // int tsize=*tsize__G;

  int local_id;
  int group_id;
  int global_id;
  int local_id_fortran;
  int group_id_fortran;
  int local_chunk_size;
  int start_position;
  int local_result_out;
  int r_iter;
  int i_rel;
  int i;
  __local int local_result_out_array [64];

  // float CLK_LOCAL_MEM_FENCE;
    local_id = get_local_id(0);
    group_id = get_group_id(0);
    global_id = get_global_id(0);


  local_id_fortran = (local_id + 1);
  group_id_fortran = (group_id + 1);
  local_chunk_size = ((tsize / 64) / 16);
  start_position = local_chunk_size * global_id;
  local_result_out = 0;
  for (r_iter=start_position;r_iter<=((start_position + local_chunk_size) - 1);r_iter++) {
    i_rel = r_iter;
    i = (i_rel + 1);
    local_result_out = (local_result_out + input_array[i-1]);
  }
  local_result_out_array[local_id_fortran-1] = local_result_out;
  barrier( CLK_LOCAL_MEM_FENCE );
  local_result_out = 0;
  for (r_iter=1;r_iter<=64;r_iter++) {
    local_result_out = (local_result_out + local_result_out_array[r_iter-1]);
  }
  global_result_out_array[group_id_fortran-1] = local_result_out;

}

__kernel void reduce_total_41(__global int *a,__global int *global_total_array) {
  int local_id;
  int group_id;
  int global_id;
  int local_id_fortran;
  int group_id_fortran;
  int local_chunk_size;
  int start_position;
  int local_total;
  int r_iter;
  int i_rel;
  int i;
  __local int local_total_array [8];


    local_id = get_local_id(0);
    group_id = get_group_id(0);
    global_id = get_global_id(0);


  local_id_fortran = (local_id + 1);
  group_id_fortran = (group_id + 1);
  local_chunk_size = ((512 / 8) / 16);
  start_position = local_chunk_size * global_id;
  local_total = 0;
  for (r_iter=start_position;r_iter<=((start_position + local_chunk_size) - 1);r_iter++) {
    i_rel = r_iter;
    i = (i_rel + 1);
    local_total = (local_total + a[i-1]);
  }
  local_total_array[local_id_fortran-1] = local_total;
  barrier( CLK_LOCAL_MEM_FENCE );
  local_total = 0;
  for (r_iter=1;r_iter<=8;r_iter++) {
    local_total = (local_total + local_total_array[r_iter-1]);
  }
  global_total_array[group_id_fortran-1] = local_total;

}

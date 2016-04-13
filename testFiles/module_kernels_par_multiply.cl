// __kernel void map_result_array_8(__global int *input_array,__global int *factor__G,__global int *tsize__G,__global int *result_array) {
//   int factor=*factor__G;
//   int tsize=*tsize__G;
__kernel void map_result_array_8(__global int *input_array, int factor, int tsize, __global int *result_array) {
  int global_id;
  int i_rel;
  int i;
   
  global_id = get_global_id(0);

  i_rel = global_id;
  i = (i_rel + 1);
  
  result_array[i-1] = input_array[i-1] * factor;

}

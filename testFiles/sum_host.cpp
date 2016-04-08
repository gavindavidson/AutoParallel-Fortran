#include <iostream>
#include <cstdlib>
#include <vector>
#include <new>
// #include <sstream>
// #include <ctime>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <chrono>

#include "cl.hpp"
#include "util.hpp"

#define nth 2
#define nunits 2

using std::cout;
using std::endl;
using std::string;
using std::vector;

cl_int err;
cl::Buffer input_buffer, result_buffer, local_buffer;
int *input_array, *result_array, *local_array;

cl::Kernel sum_kernel;
cl::CommandQueue command_queue;

inline void
    checkErr(cl_int err, const char * name)
    {
    if (err != CL_SUCCESS) {
    std::cerr << "ERROR: " << name
    << " (" << err << ")" << std::endl;
    exit(EXIT_FAILURE);
    };
}

int main(int argc, char* argv[]){
	vector<cl::Platform> platforms;
	string platform_name;
	cl::Platform::get(&platforms);

	platforms[0].getInfo((cl_platform_info)CL_PLATFORM_VENDOR, &platform_name);

	cl_context_properties context_props[3] = {CL_CONTEXT_PLATFORM, (cl_context_properties)(platforms[0])(), 0};
	
	// Try CPU context
	cl::Context device_context;
	device_context = cl::Context(
		CL_DEVICE_TYPE_CPU,
		context_props,
		NULL,
		NULL,
		&err);

	if (err == CL_SUCCESS){
		cout << "Initialised for CPU" << endl;
	}
	else {
		// Try GPU context
		device_context = cl::Context(
			CL_DEVICE_TYPE_GPU,
			context_props,
			NULL,
			NULL,
			&err);

		if (err == CL_SUCCESS){
			cout << "Initialised for GPU" << endl;
		}
	}
	vector<cl::Device> devices;
	devices = device_context.getInfo<CL_CONTEXT_DEVICES>();
	checkErr(devices.size() > 0 ? CL_SUCCESS : -1, "devices.size() > 0");

	string device_name;
	devices[0].getInfo(CL_DEVICE_NAME, &device_name);
	checkErr(err, "device_context()");
	cout << "Running on: " << device_name << endl;

	input_array = (int *)malloc(sizeof(int)*512);
	for (int i = 0; i < 512; i++){
		input_array[i] = i;
	}

	result_array = (int *)malloc(sizeof(int)*nunits);
	for (int i = 0; i < nunits; i++){
		result_array[i] = 0;
	}

	local_array = (int *)malloc(sizeof(int)*nth);
	for (int i = 0; i < nth; i++){
		local_array[i] = 0;
	}

	input_buffer = cl::Buffer(device_context, CL_MEM_READ_ONLY | CL_MEM_USE_HOST_PTR,
		sizeof(int)*512, input_array, &err);
	checkErr(err, "input_buffer");
	result_buffer = cl::Buffer(device_context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR,
		sizeof(int)*nunits, result_array, &err);
	checkErr(err, "result_buffer");
	// local_buffer = cl::Buffer(device_context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR,
	// 	sizeof(int)*nth, local_array, &err);
	// checkErr(err, "result_buffer");
	cl::LocalSpaceArg local_buffer = cl::Local(nth * sizeof(int));

	// Load source file
	std::ifstream kernel_file("module_kernels_par_sum.c");
	checkErr(kernel_file.is_open() ? CL_SUCCESS:-1, "module_kernels_par_sum.c");
	string kernel_code(std::istreambuf_iterator<char>(kernel_file), (std::istreambuf_iterator<char>()));

	// Build source for device
	cl::Program::Sources kernel_source(1, std::make_pair(kernel_code.c_str(), kernel_code.length()+1));
	cl::Program kernel_prog(device_context, kernel_source);
	err = kernel_prog.build(devices, "");
	checkErr(err, "Program::build(): kernel_prog");

	// Build kernel object
	sum_kernel = cl::Kernel(kernel_prog, "reduce_total_41");
	checkErr(err, "sum_kernel");	


	command_queue = cl::CommandQueue(device_context, devices[0],0,&err);
	checkErr(err, "command_queue()");

	cl::Event end_event;
	err = command_queue.enqueueWriteBuffer(result_buffer, CL_TRUE, 0, sizeof(int)*nunits, result_array);
	checkErr(err, "enqueueWriteBuffer(): result_buffer");
	err = command_queue.enqueueWriteBuffer(input_buffer, CL_TRUE, 0, sizeof(int)*512, input_array);
	checkErr(err, "enqueueWriteBuffer(): input_buffer");
	
	cout << "[" << result_array[0];
	for (int i = 1; i < nunits; i++){
		cout << ", " << result_array[i];
	}
	cout << "]" << endl;

	// Assign arguments
	sum_kernel.setArg(0, input_buffer);
	checkErr(err, "sum_kernel: kernel(0)");
	sum_kernel.setArg(1, result_buffer);
	checkErr(err, "sum_kernel: kernel(1)");
	// sum_kernel.setArg(2, local_buffer);
	// checkErr(err, "sum_kernel: kernel(2)");
	// sum_kernel.setArg(3, input_vector_length);
	// checkErr(err, "sum_kernel: kernel(3)");

	// cl::Event end_event;
	// err = command_queue.enqueueNDRangeKernel(sum_kernel, cl::NullRange, cl::NDRange(512), cl::NDRange(512/(nunits*nth))	, NULL, &end_event);
	err = command_queue.enqueueNDRangeKernel(sum_kernel, cl::NullRange, cl::NDRange(nunits*nth), cl::NDRange(nth)	, NULL, &end_event);
	checkErr(err, "sum_kernel: enqueueNDRangeKernel()");
	end_event.wait();

	err = command_queue.enqueueReadBuffer(result_buffer, CL_TRUE, 0,
		sizeof(int)*nunits, result_array);
	checkErr(err, "result_buffer: enqueueReadBuffer()");

	err = command_queue.enqueueReadBuffer(input_buffer, CL_TRUE, 0,
		sizeof(int)*512, input_array);
	checkErr(err, "input_buffer: enqueueReadBuffer()");

	cout << "[" << input_array[0];
	for (int i = 1; i < 512; i++){
		cout << ", " << input_array[i];
	}
	cout << "]" << endl;

	cout << "[" << result_array[0];
	int final_total = result_array[0];
	for (int i = 1; i < nunits; i++){
		cout << ", " << result_array[i];
		final_total = final_total + result_array[i];
	}
	cout << "]" << endl;
	cout << "Final total: " << final_total << endl;

}

	// err = command_queue.enqueueNDRangeKernel(sum_kernel, cl::NullRange, cl::NDRange(map_side_size*map_side_size), cl::NullRange, NULL, &end_event);
	// checkErr(err, "sum_kernel: enqueueNDRangeKernel()");

	// // Wait for completion
	// end_event.wait();
	// end = std::chrono::high_resolution_clock::now();

	// Read the results from the min_distance 
	// err = command_queue.enqueueReadBuffer(winner_distance_array_buffer, CL_TRUE, 0,
	// 	sizeof(float)*compute_units, winner_distance_array);
	// checkErr(err, "winner_distance_array_buffer: enqueueReadBuffer()");
	// err = command_queue.enqueueReadBuffer(winner_index_array_buffer, CL_TRUE, 0,
	// 	sizeof(float)*compute_units, winner_index_array);
	// checkErr(err, "winner_index_array_buffer: enqueueReadBuffer()");

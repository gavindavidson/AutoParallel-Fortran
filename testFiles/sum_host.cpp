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
#define debug 0
// #define tsize 1024

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;

extern"C"{
	void fortransum(int *array, int *size, int *result);
	// void test_(int *array);
}

cl_int err;
cl::Buffer input_buffer, result_buffer;
int *input_array, *result_array;
int tsize;

std::chrono::high_resolution_clock::time_point start, end;
std::chrono::duration<double> time_span;

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
	tsize = atoi(argv[1]);

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
	if (err == CL_SUCCESS && debug){
		cout << "Initialised for CPU" << endl;
	}

	if ((err != CL_SUCCESS)){
		// Try GPU context
		device_context = cl::Context(
			CL_DEVICE_TYPE_GPU,
			context_props,
			NULL,
			NULL,
			&err);
			if ((err == CL_SUCCESS) && debug){
				cout << "Initialised for GPU" << endl;
			}
	}


	// if ((err == CL_SUCCESS)){
	// 	cerr << "Initialised for CPU" << endl;
	// }
	// else {
	// 	// Try GPU context
	// 	device_context = cl::Context(
	// 		CL_DEVICE_TYPE_GPU,
	// 		context_props,
	// 		NULL,
	// 		NULL,
	// 		&err);

	// 	if ((err == CL_SUCCESS)){
	// 		cerr << "Initialised for GPU" << endl;
	// 	}
	// }
	vector<cl::Device> devices;
	devices = device_context.getInfo<CL_CONTEXT_DEVICES>();
	checkErr(devices.size() > 0 ? CL_SUCCESS : -1, "devices.size() > 0");

	string device_name;
	devices[0].getInfo(CL_DEVICE_NAME, &device_name);
	int compute_units;
	devices[0].getInfo(CL_DEVICE_MAX_COMPUTE_UNITS, &compute_units);
	int threads;
	devices[0].getInfo(CL_DEVICE_MAX_WORK_GROUP_SIZE, &threads);
	checkErr(err, "device_context()");
	if (debug){
		cout << "Running on: " << device_name << endl;
		cout << "\tCompute Units: " << compute_units << endl;
		cout << "\tThreads per unit: " << threads << endl;
	}

	input_array = (int *)malloc(sizeof(int)*tsize);
	for (int i = 0; i < tsize; i++){
		input_array[i] = rand()%100;
	}

	result_array = (int *)malloc(sizeof(int)*nunits);
	for (int i = 0; i < nunits; i++){
		result_array[i] = 0;
	}

	input_buffer = cl::Buffer(device_context, CL_MEM_READ_ONLY | CL_MEM_USE_HOST_PTR,
		sizeof(int)*tsize, input_array, &err);
	checkErr(err, "input_buffer");
	result_buffer = cl::Buffer(device_context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR,
		sizeof(int)*nunits, result_array, &err);
	checkErr(err, "result_buffer");

	// Load source file
	// std::ifstream kernel_file("module_kernels_par_sum.c");
	std::ifstream kernel_file("module_kernels_par_sum.cl");
	checkErr(kernel_file.is_open() ? CL_SUCCESS:-1, "module_kernels_par_sum.cl");
	string kernel_code(std::istreambuf_iterator<char>(kernel_file), (std::istreambuf_iterator<char>()));

	// Build source for device
	cl::Program::Sources kernel_source(1, std::make_pair(kernel_code.c_str(), kernel_code.length()+1));
	cl::Program kernel_prog(device_context, kernel_source);
	err = kernel_prog.build(devices, "");
	checkErr(err, "Program::build(): kernel_prog");

	// Build kernel object
	sum_kernel = cl::Kernel(kernel_prog, "reduce_result_out_8");
	checkErr(err, "sum_kernel");	


	command_queue = cl::CommandQueue(device_context, devices[0],0,&err);
	checkErr(err, "command_queue()");

	cl::Event end_event;
	err = command_queue.enqueueWriteBuffer(result_buffer, CL_TRUE, 0, sizeof(int)*nunits, result_array);
	checkErr(err, "enqueueWriteBuffer(): result_buffer");
	err = command_queue.enqueueWriteBuffer(input_buffer, CL_TRUE, 0, sizeof(int)*tsize, input_array);
	checkErr(err, "enqueueWriteBuffer(): input_buffer");
	
	if (debug){
		cout << "[" << result_array[0];
		for (int i = 1; i < nunits; i++){
			cout << ", " << result_array[i];
		}
		cout << "]" << endl;
	}

	// Assign arguments
	sum_kernel.setArg(0, input_buffer);
	checkErr(err, "sum_kernel: kernel(0)");
	sum_kernel.setArg(1, tsize);
	checkErr(err, "sum_kernel: kernel(1)");
	sum_kernel.setArg(2, result_buffer);
	checkErr(err, "sum_kernel: kernel(2)");

	start = std::chrono::high_resolution_clock::now();

	err = command_queue.enqueueNDRangeKernel(sum_kernel, cl::NullRange, cl::NDRange(nunits*nth), cl::NDRange(nth)	, NULL, &end_event);
	checkErr(err, "sum_kernel: enqueueNDRangeKernel()");
	end_event.wait();

	err = command_queue.enqueueReadBuffer(result_buffer, CL_TRUE, 0,
		sizeof(int)*nunits, result_array);
	checkErr(err, "result_buffer: enqueueReadBuffer()");

	if (debug){
		int *debug_input_array = (int *)malloc(sizeof(int)*tsize);
		err = command_queue.enqueueReadBuffer(input_buffer, CL_TRUE, 0,
			sizeof(int)*tsize, debug_input_array);
		checkErr(err, "debug_input_array: enqueueReadBuffer()");
		cout << "[" << input_array[0];
		for (int i = 1; i < tsize; i++){
			cout << ", " << input_array[i];
		}
		cout << "]" << endl;
		cout << "[" << result_array[0];
		for (int i = 1; i < nunits; i++){
			cout << ", " << result_array[i];
		}
		cout << "]" << endl;
	}

	int ocl_total = result_array[0];	
	for (int i = 1; i < nunits; i++){
		ocl_total = ocl_total + result_array[i];
	}
	end = std::chrono::high_resolution_clock::now();
	time_span = std::chrono::duration_cast<std::chrono::duration<double>>(end - start);
	double ocl_runtime = time_span.count();

	if (debug){
		cout << "Values: " << tsize << endl;
		cout << "OpenCL run time: " << ocl_runtime << endl;
		cout << "OpenCL final total: " << ocl_total << endl << endl;
	}

	start = std::chrono::high_resolution_clock::now();
	int seq_total;
	fortransum(input_array, &tsize, &seq_total);
	end = std::chrono::high_resolution_clock::now();
	time_span = std::chrono::duration_cast<std::chrono::duration<double>>(end - start);
	double seq_runtime = time_span.count();

	if (debug){
		cout << "Fortran run time: "  << time_span.count() << endl;
		cout << "Fortran final total: " << seq_total << endl;
	}
	else{
		string test_str = "Failed";
		if (seq_total == ocl_total){
			test_str = "Passed";
		}
		cout << device_name 
			<< "\tValues: " << tsize
			<< "\tTest: " << test_str 
			<< "\tOcl: " << ocl_runtime 
			<< "\tSeq: " << seq_runtime << endl;
	}

	if (seq_total != ocl_total){
		cerr << "Test fails: " << seq_total << " != " << ocl_total << endl;
	}

}
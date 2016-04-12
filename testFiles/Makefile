all: sum_host

sum_host: sum_host.o
	g++ -o sum_host sum_host.o -lrt -lOpenCL
	#-lOpenCL
	#/opt/AMDAPP/lib/x86_64/libOpenCL.so

sum_host.o: sum_host.cpp
	g++ -c -std=c++11 -Wno-deprecated-declarations sum_host.cpp -I/opt/AMDAPP/include
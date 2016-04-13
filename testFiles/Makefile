all: sum_host multiply_host

sum_host: sum_host.o sum.o
	g++ -o sum_host sum_host.o sum.o -lrt -lOpenCL -lgfortran
	#-lOpenCL
	#/opt/AMDAPP/lib/x86_64/libOpenCL.so

sum_host.o: sum_host.cpp
	#g++ -c -std=c++11 -Wno-deprecated-declarations sum_host.cpp -I/opt/AMDAPP/include
	#KombuchaShroomz
	/usr/bin/X11/g++-4.8 -c -std=c++11 -Wno-deprecated-declarations sum_host.cpp -I/opt/AMDAPP/include
	#Curieuse

sum.o: sum.f95
	gfortran -ffree-form -c sum.f95 

multiply_host: multiply_host.o multiply.o
	g++ -o multiply_host multiply_host.o multiply.o -lrt -lOpenCL -lgfortran
	#-lOpenCL
	#/opt/AMDAPP/lib/x86_64/libOpenCL.so

multiply_host.o: multiply_host.cpp
	#g++ -c -std=c++11 -Wno-deprecated-declarations multiply_host.cpp -I/opt/AMDAPP/include
	#KombuchaShroomz
	/usr/bin/X11/g++-4.8 -c -std=c++11 -Wno-deprecated-declarations multiply_host.cpp -I/opt/AMDAPP/include
	#Curieuse

multiply.o: multiply.f95
	gfortran -ffree-form -c multiply.f95 
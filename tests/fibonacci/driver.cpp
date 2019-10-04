#include <iostream>
#include <cstdio>

// clang++ driver.cpp fibonacci.ll -o fib

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT int print_int(int X) {
  fprintf(stderr, "%d\n", X);
  return 0;
}

extern "C" DLLEXPORT float print_float(float X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

extern "C" {
    int fibonacci(int n);
}


int main() {
	if(fibonacci(10) == 88) 
      std::cout << "PASSED Result: " << fibonacci(10) << std::endl;
  	else 
  	  std::cout << "FALIED Result: " << fibonacci(10) << std::endl;

    
}
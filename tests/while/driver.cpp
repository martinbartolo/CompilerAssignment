#include <iostream>
#include <cstdio>

// clang++ driver.cpp While.l -o while

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
    int While(int n);
}

int main() {
    if (While(1) == 10) {
    	std::cout << "PASSED Result: " << While(1) << std::endl;
    }
    else {
    	std::cout << "FAILED Result: " << While(1) << std::endl;
    }
    
}
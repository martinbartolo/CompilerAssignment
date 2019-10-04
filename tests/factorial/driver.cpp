#include <iostream>
#include <cstdio>

// clang++ driver.cpp factorial.ll -o fact


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
    int factorial(int n);
}

int main() {
    
    if(factorial(10) == 3628800) 
      std::cout << "PASSED Result: " << factorial(10) << std::endl;
  	else 
  	  std::cout << "FALIED Result: " << factorial(10) << std::endl;
}
#include <iostream>
#include <cstdio>
#include <math.h> 

// clang++ driver.cpp recurse.ll -o recurse

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
    int recursion_driver(int x);
}

int main() {

  int result = recursion_driver(20);
  if( result == 210)    
    std::cout << "PASSED Result: " << result << std::endl;
  else 
    std::cout << "FALIED Result: " << result << std::endl;    
  
}
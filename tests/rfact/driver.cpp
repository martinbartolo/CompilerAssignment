#include <iostream>
#include <cstdio>

// clang++ driver.cpp rfactorial.ll -o rfact


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
    int rfact(int n);
}

int main() {
    
    if(rfact(10) == 3628800) 
      std::cout << "PASSED Result: " << rfact(10) << std::endl;
    else 
      std::cout << "FALIED Result: " << rfact(10) << std::endl;
}
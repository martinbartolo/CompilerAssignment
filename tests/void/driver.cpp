#include <iostream>
#include <cstdio>

// clang++ driver.cpp While.ll -o while

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
    void Void();
}

int main() {

  Void();
  std::cout << "PASSED Result " << std::endl;
    
}
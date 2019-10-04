#include <iostream>
#include <cstdio>
#include <math.h> 

// clang++ driver.cpp cosine.ll -o cosine

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
    float unary(int n, float m);
}

bool essentiallyEqual(float a, float b, float epsilon) {
    return fabs(a - b) <= ( (fabs(a) > fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

int main() {

  float x = 3.0; // pi
  int y = 2;
  if(essentiallyEqual(unary(y,x),4.0,0.001))
    std::cout << "Result: PASSED" <<std::endl;
  else
    std::cout << "Result: FALIED" <<std::endl;
  
  
}

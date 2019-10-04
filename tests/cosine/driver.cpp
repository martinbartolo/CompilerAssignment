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
    float cosine(float x);
}

bool essentiallyEqual(float a, float b, float epsilon)
{
    return fabs(a - b) <= ( (fabs(a) > fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

int main() {

  float x = 3.14159; // pi

  
  if(  essentiallyEqual(cosine(x),-1.0f, 0.00001f) // pi
    && essentiallyEqual(cosine(x/3.0),0.5f, 0.00001f) //pi/3
    && essentiallyEqual(cosine(2*x/3),-0.5f, 0.00001f) //2pi/3 
    )  
    std::cout << "PASSED Result: " << std::endl;
  else 
    std::cout << "FALIED Result: " << std::endl;    
  
}
#include <iostream>
#include <cstdio>
#include <math.h> 

// clang++ driver.cpp pi.ll -o pi

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
  float pi();
}

bool essentiallyEqual(float a, float b, float epsilon)
{
    return fabs(a - b) <= ( (fabs(a) > fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

int main() {
    float PI = pi();

    if(essentiallyEqual(PI,3.141592f, 0.000001f)) 
    	printf("PASSED Result for PI is : %0.6f\n\n", PI);
    else 
    	printf("FAILED Result for PI is : %0.6f\n\n", PI);
}
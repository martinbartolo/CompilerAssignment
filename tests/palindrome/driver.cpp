#include <iostream>
#include <cstdio>
#include <math.h> 

// clang++ driver.cpp palindrome.ll -o palindrome

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
    bool palindrome(int number);
}


int main() {

  float x = 3.14159; // pi

  
  if(palindrome(12321) && palindrome(45677654) && !palindrome(123786))
    std::cout << "PASSED Result: " << std::endl;
  else 
    std::cout << "FALIED Result: " << std::endl;    
  
}
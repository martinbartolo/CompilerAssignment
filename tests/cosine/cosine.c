// MiniC program to compute the cosine of x to within tolerance eps
// use an alternating series

extern float print_float(float X);

float cosine (float x) {

  float cos;  
  float n; 
  float term; 
  float eps;
  float alt;
  
  eps = 0.000001;
  n = 1.0;
  cos = 1.0;
  term = 1.0;
  alt = -1.0;

  while (term > eps) { 
    term = term * x * x / n / (n+1);
    cos = cos + alt * term;
    alt = -alt;
    n = n + 2;
    
  }

  print_float(cos);
  return cos;
  
}

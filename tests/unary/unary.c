// MiniC program to test unary operator (-)
extern int print_int(int X);
extern float print_float(float X);

float unary(int n, float m){
        float result;
        float sum;
       
        sum = 0.0;
	
	result = n + m;  
	print_float(result);
        sum = sum + result;

	result = n + -m;  
	print_float(result);
        sum = sum + result;

	result = n + --m;  
	print_float(result);
        sum = sum + result;

	result = -n + -m;  
	print_float(result);
        sum = sum + result;

  return sum;
}


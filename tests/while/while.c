// MiniC program to test while loop

extern int print_int(int X);

int test;
float f;
bool b;

int While(int n){
  int result;
  test = 11 + 1;
  result = 0;
  print_int(test); 
  while(result < 10)
    result = result + 1;  
    //print_int(result);    
   
  return result;
}
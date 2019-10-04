// MiniC program to test a void return function

extern int print_int(int X);

void Void(void){
  int result;
  result = 0;
  print_int(result);
  while(result < 10) {
    result = result + 1;
    print_int(result);
  }

  return;
}
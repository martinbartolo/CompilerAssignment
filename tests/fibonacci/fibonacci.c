
// MiniC program to compute the fibonachi numbers

extern int print_int(int X);

int fibonacci(int n)
{
  int first;
  int second;
  int next;
  int c;
  int total;

  print_int(n);
  first = 0;
  second = 1;  
  c = 1;
  total = 0;

  while(c < n) {
    if (c <= 1) {
      next = c;
    }
    else {
      next = first + second;
      first = second;
      second = next;
    }    
    print_int(next);    
    c = c + 1;
    total = total + next;
  }
  print_int(total); 
  return total;
}
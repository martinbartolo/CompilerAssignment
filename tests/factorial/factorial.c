// MiniC program to compute the factorial of a number
int factorial(int n)
{
    int i;
    int factorial;

    factorial = 1;
    i = 1;

    while (i<=n) {
      factorial = factorial * i;              // factorial = factorial*i;
      i = i + 1 ;
    }
    
    return factorial;
}
// mini-c program to compute the Factorial of a Number Using Recursion

int multiplyNumbers(int n) {
    int result;
    result = 0;

    if (n >= 1) {
        result = n*multiplyNumbers(n-1);
    }
    else {
        result = 1;
    }
    return result;
}

int rfact(int n) {   
    return multiplyNumbers(n);
}

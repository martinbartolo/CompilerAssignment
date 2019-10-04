// MiniC program to calculate Sum of Natural Numbers Using Recursion

extern int print_int(int X);

int addNumbers(int n)
{
    int result;
    result = 0;

    if(n != 0) {

        result = n + addNumbers(n-1);
    }
    else {
        result  = n;
    }
    print_int(result);
    return result;
}

int recursion_driver(int num) {    
    return addNumbers(num);    
}


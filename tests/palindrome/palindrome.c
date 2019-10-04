// MiniC program to see if a number is a palindrome

bool palindrome(int number) {

   int t;
   int rev;
   int rmndr;
   bool result;

   rev = 0;   
   result = false;

   t = number;
 
   while (number > 0)
   {
      rmndr = number%10;
      rev = rev*10 + rmndr;
      number = number/10;
   }
   
   if(t == rev) {
      result = true;
   }
   else {
      result = false;
   }
   return result;
}
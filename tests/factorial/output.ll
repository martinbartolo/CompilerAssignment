; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @factorial(i32 %n) {
block:
  %factorial = alloca i32
  %i = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %i
  store i32 0, i32* %factorial
  %factorial2 = load i32, i32* %factorial
  store i32 1, i32* %factorial
  %i3 = load i32, i32* %i
  store i32 1, i32* %i
  br label %condition

condition:                                        ; preds = %whileloop, %block
  %i4 = load i32, i32* %i
  %n5 = load i32, i32* %n1
  %letmp = icmp ule i32 %i4, %n5
  %booltmp = sitofp i1 %letmp to float
  %loopcondition = fcmp one float %booltmp, 0.000000e+00
  br i1 %loopcondition, label %whileloop, label %postloop

whileloop:                                        ; preds = %condition
  %factorial6 = load i32, i32* %factorial
  %i7 = load i32, i32* %i
  %multmp = mul i32 %factorial6, %i7
  %factorial8 = load i32, i32* %factorial
  store i32 %multmp, i32* %factorial
  %i9 = load i32, i32* %i
  %addtmp = add i32 %i9, 1
  %i10 = load i32, i32* %i
  store i32 %addtmp, i32* %i
  br label %condition

postloop:                                         ; preds = %condition
  %factorial11 = load i32, i32* %factorial
  ret i32 %factorial11
}

; ModuleID = 'mini-c'
source_filename = "mini-c"

define i1 @palindrome(i32 %number) {
block:
  %result = alloca i1
  %rmndr = alloca i32
  %rev = alloca i32
  %t = alloca i32
  %number1 = alloca i32
  store i32 %number, i32* %number1
  store i32 0, i32* %t
  store i32 0, i32* %rev
  store i32 0, i32* %rmndr
  store i1 false, i1* %result
  %rev2 = load i32, i32* %rev
  store i32 0, i32* %rev
  %result3 = load i1, i1* %result
  store i1 false, i1* %result
  %number4 = load i32, i32* %number1
  %t5 = load i32, i32* %t
  store i32 %number4, i32* %t
  br label %condition

condition:                                        ; preds = %whileloop, %block
  %number6 = load i32, i32* %number1
  %gttmp = icmp ugt i32 %number6, 0
  %booltmp = sitofp i1 %gttmp to float
  %loopcondition = fcmp one float %booltmp, 0.000000e+00
  br i1 %loopcondition, label %whileloop, label %postloop

whileloop:                                        ; preds = %condition
  %number7 = load i32, i32* %number1
  %remtmp = srem i32 %number7, 10
  %rmndr8 = load i32, i32* %rmndr
  store i32 %remtmp, i32* %rmndr
  %rev9 = load i32, i32* %rev
  %multmp = mul i32 %rev9, 10
  %rmndr10 = load i32, i32* %rmndr
  %addtmp = add i32 %multmp, %rmndr10
  %rev11 = load i32, i32* %rev
  store i32 %addtmp, i32* %rev
  %number12 = load i32, i32* %number1
  %divtmp = sdiv i32 %number12, 10
  %number13 = load i32, i32* %number1
  store i32 %divtmp, i32* %number1
  br label %condition

postloop:                                         ; preds = %condition
  %t14 = load i32, i32* %t
  %rev15 = load i32, i32* %rev
  %eqtmp = icmp eq i32 %t14, %rev15
  %booltmp16 = sitofp i1 %eqtmp to float
  %"ifcondition\0A" = fcmp one float %booltmp16, 0.000000e+00
  br i1 %"ifcondition\0A", label %ifblock, label %elseblock

ifblock:                                          ; preds = %postloop
  %result17 = load i1, i1* %result
  store i1 true, i1* %result
  br label %postif

elseblock:                                        ; preds = %postloop
  %result18 = load i1, i1* %result
  store i1 false, i1* %result
  br label %postif

postif:                                           ; preds = %elseblock, %ifblock
  %iftmp = phi i1 [ true, %ifblock ], [ false, %elseblock ]
  %result19 = load i1, i1* %result
  ret i1 %result19
}

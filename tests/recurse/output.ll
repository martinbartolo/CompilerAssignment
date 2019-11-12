; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @addNumbers(i32 %n) {
block:
  %result = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %result
  %result2 = load i32, i32* %result
  store i32 0, i32* %result
  %n3 = load i32, i32* %n1
  %netmp = icmp ne i32 %n3, 0
  %booltmp = sitofp i1 %netmp to float
  %"ifcondition\0A" = fcmp one float %booltmp, 0.000000e+00
  br i1 %"ifcondition\0A", label %ifblock, label %elseblock

ifblock:                                          ; preds = %block
  %n4 = load i32, i32* %n1
  %n5 = load i32, i32* %n1
  %subtmp = sub i32 %n5, 1
  %calltmp = call i32 @addNumbers(i32 %subtmp)
  %addtmp = add i32 %n4, %calltmp
  %result6 = load i32, i32* %result
  store i32 %addtmp, i32* %result
  br label %postif

elseblock:                                        ; preds = %block
  %n7 = load i32, i32* %n1
  %result8 = load i32, i32* %result
  store i32 %n7, i32* %result
  br label %postif

postif:                                           ; preds = %elseblock, %ifblock
  %iftmp = phi i32 [ %addtmp, %ifblock ], [ %n7, %elseblock ]
  %result9 = load i32, i32* %result
  %calltmp10 = call i32 @print_int(i32 %result9)
  %result11 = load i32, i32* %result
  ret i32 %result11
}

define i32 @recursion_driver(i32 %num) {
block:
  %num1 = alloca i32
  store i32 %num, i32* %num1
  %num2 = load i32, i32* %num1
  %calltmp = call i32 @addNumbers(i32 %num2)
  ret i32 %calltmp
}

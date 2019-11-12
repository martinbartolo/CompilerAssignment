; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @multiplyNumbers(i32 %n) {
block:
  %result = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %result
  %result2 = load i32, i32* %result
  store i32 0, i32* %result
  %n3 = load i32, i32* %n1
  %getmp = icmp uge i32 %n3, 1
  %booltmp = sitofp i1 %getmp to float
  %"ifcondition\0A" = fcmp one float %booltmp, 0.000000e+00
  br i1 %"ifcondition\0A", label %ifblock, label %elseblock

ifblock:                                          ; preds = %block
  %n4 = load i32, i32* %n1
  %n5 = load i32, i32* %n1
  %subtmp = sub i32 %n5, 1
  %calltmp = call i32 @multiplyNumbers(i32 %subtmp)
  %multmp = mul i32 %n4, %calltmp
  %result6 = load i32, i32* %result
  store i32 %multmp, i32* %result
  br label %postif

elseblock:                                        ; preds = %block
  %result7 = load i32, i32* %result
  store i32 1, i32* %result
  br label %postif

postif:                                           ; preds = %elseblock, %ifblock
  %iftmp = phi i32 [ %multmp, %ifblock ], [ 1, %elseblock ]
  %result8 = load i32, i32* %result
  ret i32 %result8
}

define i32 @rfact(i32 %n) {
block:
  %n1 = alloca i32
  store i32 %n, i32* %n1
  %n2 = load i32, i32* %n1
  %calltmp = call i32 @multiplyNumbers(i32 %n2)
  ret i32 %calltmp
}

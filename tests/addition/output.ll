; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @addition(i32 %n, i32 %m) {
block:
  %result = alloca i32
  %m2 = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 %m, i32* %m2
  store i32 0, i32* %result
  %n3 = load i32, i32* %n1
  %m4 = load i32, i32* %m2
  %addtmp = add i32 %n3, %m4
  %result5 = load i32, i32* %result
  store i32 %addtmp, i32* %result
  %n6 = load i32, i32* %n1
  %eqtmp = icmp eq i32 %n6, 4
  %booltmp = sitofp i1 %eqtmp to float
  %"ifcondition\0A" = fcmp one float %booltmp, 0.000000e+00
  br i1 %"ifcondition\0A", label %ifblock, label %elseblock

ifblock:                                          ; preds = %block
  %n7 = load i32, i32* %n1
  %m8 = load i32, i32* %m2
  %addtmp9 = add i32 %n7, %m8
  %calltmp = call i32 @print_int(i32 %addtmp9)
  br label %postif

elseblock:                                        ; preds = %block
  %n10 = load i32, i32* %n1
  %m11 = load i32, i32* %m2
  %multmp = mul i32 %n10, %m11
  %calltmp12 = call i32 @print_int(i32 %multmp)
  br label %postif

postif:                                           ; preds = %elseblock, %ifblock
  %iftmp = phi i32 [ %calltmp, %ifblock ], [ %calltmp12, %elseblock ]
  %result13 = load i32, i32* %result
  ret i32 %result13
}

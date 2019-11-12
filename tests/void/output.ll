; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define void @Void() {
block:
  %result = alloca i32
  store i32 0, i32* %result
  %result1 = load i32, i32* %result
  store i32 0, i32* %result
  %result2 = load i32, i32* %result
  %calltmp = call i32 @print_int(i32 %result2)
  br label %condition

condition:                                        ; preds = %whileloop, %block
  %result3 = load i32, i32* %result
  %lttmp = icmp ult i32 %result3, 10
  %booltmp = sitofp i1 %lttmp to float
  %loopcondition = fcmp one float %booltmp, 0.000000e+00
  br i1 %loopcondition, label %whileloop, label %postloop

whileloop:                                        ; preds = %condition
  %result4 = load i32, i32* %result
  %addtmp = add i32 %result4, 1
  %result5 = load i32, i32* %result
  store i32 %addtmp, i32* %result
  %result6 = load i32, i32* %result
  %calltmp7 = call i32 @print_int(i32 %result6)
  br label %condition

postloop:                                         ; preds = %condition
  ret void
}

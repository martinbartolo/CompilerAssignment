; ModuleID = 'mini-c'
source_filename = "mini-c"

@test = common global i32 0
@f = common global float 0.000000e+00
@b = common global i1 false

declare i32 @print_int(i32)

define i32 @While(i32 %n) {
block:
  %result = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %result
  %test = load i32, i32* @test
  store i32 12, i32* @test
  %result2 = load i32, i32* %result
  store i32 0, i32* %result
  %test3 = load i32, i32* @test
  %calltmp = call i32 @print_int(i32 %test3)
  br label %condition

condition:                                        ; preds = %whileloop, %block
  %result4 = load i32, i32* %result
  %lttmp = icmp ult i32 %result4, 10
  %booltmp = sitofp i1 %lttmp to float
  %loopcondition = fcmp one float %booltmp, 0.000000e+00
  br i1 %loopcondition, label %whileloop, label %postloop

whileloop:                                        ; preds = %condition
  %result5 = load i32, i32* %result
  %addtmp = add i32 %result5, 1
  %result6 = load i32, i32* %result
  store i32 %addtmp, i32* %result
  br label %condition

postloop:                                         ; preds = %condition
  %result7 = load i32, i32* %result
  ret i32 %result7
}

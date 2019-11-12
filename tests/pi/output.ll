; ModuleID = 'mini-c'
source_filename = "mini-c"

define float @pi() {
block:
  %i = alloca i32
  %PI = alloca float
  %flag = alloca i1
  store i1 false, i1* %flag
  store float 0.000000e+00, float* %PI
  store i32 0, i32* %i
  %flag1 = load i1, i1* %flag
  store i1 true, i1* %flag
  %PI2 = load float, float* %PI
  store float 3.000000e+00, float* %PI
  %i3 = load i32, i32* %i
  store i32 2, i32* %i
  br label %condition

condition:                                        ; preds = %postif, %block
  %i4 = load i32, i32* %i
  %lttmp = icmp ult i32 %i4, 100
  %booltmp = sitofp i1 %lttmp to float
  %loopcondition = fcmp one float %booltmp, 0.000000e+00
  br i1 %loopcondition, label %whileloop, label %postloop

whileloop:                                        ; preds = %condition
  %flag5 = load i1, i1* %flag
  %upgradebooltoFloat = sitofp i1 %flag5 to float
  %"ifcondition\0A" = fcmp one float %upgradebooltoFloat, 0.000000e+00
  br i1 %"ifcondition\0A", label %ifblock, label %elseblock

postloop:                                         ; preds = %condition
  %PI30 = load float, float* %PI
  ret float %PI30

ifblock:                                          ; preds = %whileloop
  %PI6 = load float, float* %PI
  %i7 = load i32, i32* %i
  %i8 = load i32, i32* %i
  %addtmp = add i32 %i8, 1
  %i9 = load i32, i32* %i
  %addtmp10 = add i32 %i9, 2
  %multmp = mul i32 %addtmp, %addtmp10
  %multmp11 = mul i32 %i7, %multmp
  %upgradeRHStoFloat = sitofp i32 %multmp11 to float
  %divtmp = fdiv float 4.000000e+00, %upgradeRHStoFloat
  %addtmp12 = fadd float %PI6, %divtmp
  %PI13 = load float, float* %PI
  store float %addtmp12, float* %PI
  br label %postif

elseblock:                                        ; preds = %whileloop
  %PI14 = load float, float* %PI
  %i15 = load i32, i32* %i
  %i16 = load i32, i32* %i
  %addtmp17 = add i32 %i16, 1
  %i18 = load i32, i32* %i
  %addtmp19 = add i32 %i18, 2
  %multmp20 = mul i32 %addtmp17, %addtmp19
  %multmp21 = mul i32 %i15, %multmp20
  %upgradeRHStoFloat22 = sitofp i32 %multmp21 to float
  %divtmp23 = fdiv float 4.000000e+00, %upgradeRHStoFloat22
  %subtmp = fsub float %PI14, %divtmp23
  %PI24 = load float, float* %PI
  store float %subtmp, float* %PI
  br label %postif

postif:                                           ; preds = %elseblock, %ifblock
  %iftmp = phi float [ %addtmp12, %ifblock ], [ %subtmp, %elseblock ]
  %flag25 = load i1, i1* %flag
  %nottemp = xor i1 %flag25, true
  %flag26 = load i1, i1* %flag
  store i1 %nottemp, i1* %flag
  %i27 = load i32, i32* %i
  %addtmp28 = add i32 %i27, 2
  %i29 = load i32, i32* %i
  store i32 %addtmp28, i32* %i
  br label %condition
}

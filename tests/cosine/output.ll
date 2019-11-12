; ModuleID = 'mini-c'
source_filename = "mini-c"

declare float @print_float(float)

define float @cosine(float %x) {
block:
  %alt = alloca float
  %eps = alloca float
  %term = alloca float
  %n = alloca float
  %cos = alloca float
  %x1 = alloca float
  store float %x, float* %x1
  store float 0.000000e+00, float* %cos
  store float 0.000000e+00, float* %n
  store float 0.000000e+00, float* %term
  store float 0.000000e+00, float* %eps
  store float 0.000000e+00, float* %alt
  %eps2 = load float, float* %eps
  store float 0x3EB0C6F7A0000000, float* %eps
  %n3 = load float, float* %n
  store float 1.000000e+00, float* %n
  %cos4 = load float, float* %cos
  store float 1.000000e+00, float* %cos
  %term5 = load float, float* %term
  store float 1.000000e+00, float* %term
  %alt6 = load float, float* %alt
  store float -1.000000e+00, float* %alt
  br label %condition

condition:                                        ; preds = %whileloop, %block
  %term7 = load float, float* %term
  %eps8 = load float, float* %eps
  %gttmp = fcmp ugt float %term7, %eps8
  %booltmp = sitofp i1 %gttmp to float
  %loopcondition = fcmp one float %booltmp, 0.000000e+00
  br i1 %loopcondition, label %whileloop, label %postloop

whileloop:                                        ; preds = %condition
  %term9 = load float, float* %term
  %x10 = load float, float* %x1
  %x11 = load float, float* %x1
  %n12 = load float, float* %n
  %n13 = load float, float* %n
  %addtmp = fadd float %n13, 1.000000e+00
  %divtmp = fdiv float %n12, %addtmp
  %divtmp14 = fdiv float %x11, %divtmp
  %multmp = fmul float %x10, %divtmp14
  %multmp15 = fmul float %term9, %multmp
  %term16 = load float, float* %term
  store float %multmp15, float* %term
  %cos17 = load float, float* %cos
  %alt18 = load float, float* %alt
  %term19 = load float, float* %term
  %multmp20 = fmul float %alt18, %term19
  %addtmp21 = fadd float %cos17, %multmp20
  %cos22 = load float, float* %cos
  store float %addtmp21, float* %cos
  %alt23 = load float, float* %alt
  %negtmp = fsub float -0.000000e+00, %alt23
  %alt24 = load float, float* %alt
  store float %negtmp, float* %alt
  %n25 = load float, float* %n
  %addtmp26 = fadd float %n25, 2.000000e+00
  %n27 = load float, float* %n
  store float %addtmp26, float* %n
  br label %condition

postloop:                                         ; preds = %condition
  %cos28 = load float, float* %cos
  %calltmp = call float @print_float(float %cos28)
  %cos29 = load float, float* %cos
  ret float %cos29
}

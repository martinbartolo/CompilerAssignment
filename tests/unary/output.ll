; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

declare float @print_float(float)

define float @unary(i32 %n, float %m) {
block:
  %sum = alloca float
  %result = alloca float
  %m2 = alloca float
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store float %m, float* %m2
  store float 0.000000e+00, float* %result
  store float 0.000000e+00, float* %sum
  %sum3 = load float, float* %sum
  store float 0.000000e+00, float* %sum
  %n4 = load i32, i32* %n1
  %m5 = load float, float* %m2
  %upgradeLHStoFloat = sitofp i32 %n4 to float
  %addtmp = fadd float %upgradeLHStoFloat, %m5
  %result6 = load float, float* %result
  store float %addtmp, float* %result
  %result7 = load float, float* %result
  %calltmp = call float @print_float(float %result7)
  %sum8 = load float, float* %sum
  %result9 = load float, float* %result
  %addtmp10 = fadd float %sum8, %result9
  %sum11 = load float, float* %sum
  store float %addtmp10, float* %sum
  %n12 = load i32, i32* %n1
  %m13 = load float, float* %m2
  %negtmp = fsub float -0.000000e+00, %m13
  %upgradeLHStoFloat14 = sitofp i32 %n12 to float
  %addtmp15 = fadd float %upgradeLHStoFloat14, %negtmp
  %result16 = load float, float* %result
  store float %addtmp15, float* %result
  %result17 = load float, float* %result
  %calltmp18 = call float @print_float(float %result17)
  %sum19 = load float, float* %sum
  %result20 = load float, float* %result
  %addtmp21 = fadd float %sum19, %result20
  %sum22 = load float, float* %sum
  store float %addtmp21, float* %sum
  %n23 = load i32, i32* %n1
  %m24 = load float, float* %m2
  %negtmp25 = fsub float -0.000000e+00, %m24
  %negtmp26 = fsub float -0.000000e+00, %negtmp25
  %upgradeLHStoFloat27 = sitofp i32 %n23 to float
  %addtmp28 = fadd float %upgradeLHStoFloat27, %negtmp26
  %result29 = load float, float* %result
  store float %addtmp28, float* %result
  %result30 = load float, float* %result
  %calltmp31 = call float @print_float(float %result30)
  %sum32 = load float, float* %sum
  %result33 = load float, float* %result
  %addtmp34 = fadd float %sum32, %result33
  %sum35 = load float, float* %sum
  store float %addtmp34, float* %sum
  %n36 = load i32, i32* %n1
  %upgradeRHStoFloat = sitofp i32 %n36 to float
  %negtmp37 = fsub float -0.000000e+00, %upgradeRHStoFloat
  %upgradeRHStoFloat38 = fptosi float %negtmp37 to i32
  %m39 = load float, float* %m2
  %negtmp40 = fsub float -0.000000e+00, %m39
  %upgradeLHStoFloat41 = sitofp i32 %upgradeRHStoFloat38 to float
  %addtmp42 = fadd float %upgradeLHStoFloat41, %negtmp40
  %result43 = load float, float* %result
  store float %addtmp42, float* %result
  %result44 = load float, float* %result
  %calltmp45 = call float @print_float(float %result44)
  %sum46 = load float, float* %sum
  %result47 = load float, float* %result
  %addtmp48 = fadd float %sum46, %result47
  %sum49 = load float, float* %sum
  store float %addtmp48, float* %sum
  %sum50 = load float, float* %sum
  ret float %sum50
}

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
  store double 0.000000e+00, float* %cos
  store double 0.000000e+00, float* %n
  store double 0.000000e+00, float* %term
  store double 0.000000e+00, float* %eps
  store double 0.000000e+00, float* %alt
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

condition:                                        ; preds = %block
  ret void

whileloop:                                        ; No predecessors!

postloop:                                         ; No predecessors!
}

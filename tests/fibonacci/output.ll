; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @fibonacci(i32 %n) {
block:
  %total = alloca i32
  %c = alloca i32
  %next = alloca i32
  %second = alloca i32
  %first = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %first
  store i32 0, i32* %second
  store i32 0, i32* %next
  store i32 0, i32* %c
  store i32 0, i32* %total
  %n2 = load i32, i32* %n1
  %calltmp = call i32 @print_int(i32 %n2)
  %first3 = load i32, i32* %first
  store i32 0, i32* %first
  %second4 = load i32, i32* %second
  store i32 1, i32* %second
  %c5 = load i32, i32* %c
  store i32 1, i32* %c
  %total6 = load i32, i32* %total
  store i32 0, i32* %total
  br label %condition

condition:                                        ; preds = %postif, %block
  %c7 = load i32, i32* %c
  %n8 = load i32, i32* %n1
  %lttmp = icmp ult i32 %c7, %n8
  %booltmp = sitofp i1 %lttmp to float
  %loopcondition = fcmp one float %booltmp, 0.000000e+00
  br i1 %loopcondition, label %whileloop, label %postloop

whileloop:                                        ; preds = %condition
  %c9 = load i32, i32* %c
  %letmp = icmp ule i32 %c9, 1
  %booltmp10 = sitofp i1 %letmp to float
  %"ifcondition\0A" = fcmp one float %booltmp10, 0.000000e+00
  br i1 %"ifcondition\0A", label %ifblock, label %elseblock

postloop:                                         ; preds = %condition
  %total29 = load i32, i32* %total
  %calltmp30 = call i32 @print_int(i32 %total29)
  %total31 = load i32, i32* %total
  ret i32 %total31

ifblock:                                          ; preds = %whileloop
  %c11 = load i32, i32* %c
  %next12 = load i32, i32* %next
  store i32 %c11, i32* %next
  br label %postif

elseblock:                                        ; preds = %whileloop
  %first13 = load i32, i32* %first
  %second14 = load i32, i32* %second
  %addtmp = add i32 %first13, %second14
  %next15 = load i32, i32* %next
  store i32 %addtmp, i32* %next
  %second16 = load i32, i32* %second
  %first17 = load i32, i32* %first
  store i32 %second16, i32* %first
  %next18 = load i32, i32* %next
  %second19 = load i32, i32* %second
  store i32 %next18, i32* %second
  br label %postif

postif:                                           ; preds = %elseblock, %ifblock
  %iftmp = phi i32 [ %c11, %ifblock ], [ %next18, %elseblock ]
  %next20 = load i32, i32* %next
  %calltmp21 = call i32 @print_int(i32 %next20)
  %c22 = load i32, i32* %c
  %addtmp23 = add i32 %c22, 1
  %c24 = load i32, i32* %c
  store i32 %addtmp23, i32* %c
  %total25 = load i32, i32* %total
  %next26 = load i32, i32* %next
  %addtmp27 = add i32 %total25, %next26
  %total28 = load i32, i32* %total
  store i32 %addtmp27, i32* %total
  br label %condition
}

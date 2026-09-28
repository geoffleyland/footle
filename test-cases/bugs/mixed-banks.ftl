argument b, x
local s = sin(x)
local u = sin(x + 1)
return s, u, true, b

#( expected schedule

  I0: argument
  I1: argument
  I4: ldr K0
  I2: ldr sin
  I7: mov #1
  I5: fadd I1 I4
  I3: blr I2 I1
  I6: blr I2 I5
  I8: ret I3 I6 I7 I0
  K0: 1.0

#)

#( expected assembly bool f64

  0x1000: stp x19, x20, [sp, #-0x10]!
  0x1004: str x21, [sp, #-0x10]!
  0x1008: stp d8, d9, [sp, #-0x10]!
  0x100c: ldr d16, #0x1090
  0x1010: ldr x21, #0x1098
  0x1014: mov x19, #1
  0x1018: fadd d8, d0, d16
  0x101c: mov x20, x0
  0x1020: str x30, [sp, #-0x10]!
  0x1024: blr x21
  0x1028: ldr x30, [sp], #0x10
  0x102c: fmov d9, d8
  0x1030: fmov d8, d0
  0x1034: fmov d0, d9
  0x1038: str x30, [sp, #-0x10]!
  0x103c: blr x21
  0x1040: ldr x30, [sp], #0x10
  0x1044: mov x0, x19
  0x1048: mov x1, x20
  0x104c: fmov d1, d0
  0x1050: fmov d0, d8
  0x1054: ldp d8, d9, [sp], #0x10
  0x1058: ldr x21, [sp], #0x10
  0x105c: ldp x19, x20, [sp], #0x10
  0x1060: ret
  0x1064: mov x16, x0
  0x1068: stp x1, x30, [sp, #-0x10]!
  0x106c: ldr x0, [x16]
  0x1070: ldr d0, [x16, #8]
  0x1074: bl #0x1000
  0x1078: ldp x16, x30, [sp], #0x10
  0x107c: str d0, [x16]
  0x1080: str d1, [x16, #8]
  0x1084: str x0, [x16, #0x10]
  0x1088: str x1, [x16, #0x18]
  0x108c: ret
  0x1090: 1.0
  0x1098: sin

#)

#( expected assembly f64 f64

  0x1000: stp x19, x20, [sp, #-0x10]!
  0x1004: stp d8, d9, [sp, #-0x10]!
  0x1008: str d10, [sp, #-0x10]!
  0x100c: ldr d16, #0x1098
  0x1010: ldr x20, #0x10a0
  0x1014: mov x19, #1
  0x1018: fadd d8, d1, d16
  0x101c: fmov d9, d0
  0x1020: fmov d0, d1
  0x1024: str x30, [sp, #-0x10]!
  0x1028: blr x20
  0x102c: ldr x30, [sp], #0x10
  0x1030: fmov d10, d8
  0x1034: fmov d8, d0
  0x1038: fmov d0, d10
  0x103c: str x30, [sp, #-0x10]!
  0x1040: blr x20
  0x1044: ldr x30, [sp], #0x10
  0x1048: mov x0, x19
  0x104c: fmov d1, d0
  0x1050: fmov d0, d8
  0x1054: fmov d2, d9
  0x1058: ldr d10, [sp], #0x10
  0x105c: ldp d8, d9, [sp], #0x10
  0x1060: ldp x19, x20, [sp], #0x10
  0x1064: ret
  0x1068: mov x16, x0
  0x106c: stp x1, x30, [sp, #-0x10]!
  0x1070: ldr d0, [x16]
  0x1074: ldr d1, [x16, #8]
  0x1078: bl #0x1000
  0x107c: ldp x16, x30, [sp], #0x10
  0x1080: str d0, [x16]
  0x1084: str d1, [x16, #8]
  0x1088: str x0, [x16, #0x10]
  0x108c: str d2, [x16, #0x18]
  0x1090: ret
  0x1098: 1.0
  0x10a0: sin

#)

#( expected results

  true 1 -> 0.8414709848078965 0.9092974268256817 true true
  1 2 -> 0.9092974268256817 0.1411200080598672 true 1

#)

argument b, x
local s = sin(x)
local u = sin(x + 1)
return s, u, true, b

#( expected schedule aarch64

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

#( expected assembly aarch64 bool f64

  0x1000: stp x19, x20, [sp, #-0x10]!
  0x1004: stp x21, x30, [sp, #-0x10]!
  0x1008: str d8, [sp, #-0x10]!
  0x100c: ldr d16, #0x1080
  0x1010: ldr x21, #0x1088
  0x1014: mov x19, #1
  0x1018: fadd d8, d0, d16
  0x101c: mov x20, x0
  0x1020: blr x21
  0x1024: fmov d16, d8
  0x1028: fmov d8, d0
  0x102c: fmov d0, d16
  0x1030: blr x21
  0x1034: fmov d1, d0
  0x1038: fmov d0, d8
  0x103c: mov x0, x19
  0x1040: mov x1, x20
  0x1044: ldr d8, [sp], #0x10
  0x1048: ldp x21, x30, [sp], #0x10
  0x104c: ldp x19, x20, [sp], #0x10
  0x1050: ret
  0x1054: mov x16, x0
  0x1058: stp x1, x30, [sp, #-0x10]!
  0x105c: ldr x0, [x16]
  0x1060: ldr d0, [x16, #8]
  0x1064: bl #0x1000
  0x1068: ldp x16, x30, [sp], #0x10
  0x106c: str d0, [x16]
  0x1070: str d1, [x16, #8]
  0x1074: str x0, [x16, #0x10]
  0x1078: str x1, [x16, #0x18]
  0x107c: ret
  0x1080: 1.0
  0x1088: sin

#)

#( expected assembly aarch64 f64 f64

  0x1000: stp x19, x20, [sp, #-0x10]!
  0x1004: str x30, [sp, #-0x10]!
  0x1008: stp d8, d9, [sp, #-0x10]!
  0x100c: ldr d16, #0x1088
  0x1010: ldr x20, #0x1090
  0x1014: mov x19, #1
  0x1018: fadd d8, d1, d16
  0x101c: fmov d9, d0
  0x1020: fmov d0, d1
  0x1024: blr x20
  0x1028: fmov d16, d8
  0x102c: fmov d8, d0
  0x1030: fmov d0, d16
  0x1034: blr x20
  0x1038: fmov d1, d0
  0x103c: fmov d0, d8
  0x1040: mov x0, x19
  0x1044: fmov d2, d9
  0x1048: ldp d8, d9, [sp], #0x10
  0x104c: ldr x30, [sp], #0x10
  0x1050: ldp x19, x20, [sp], #0x10
  0x1054: ret
  0x1058: mov x16, x0
  0x105c: stp x1, x30, [sp, #-0x10]!
  0x1060: ldr d0, [x16]
  0x1064: ldr d1, [x16, #8]
  0x1068: bl #0x1000
  0x106c: ldp x16, x30, [sp], #0x10
  0x1070: str d0, [x16]
  0x1074: str d1, [x16, #8]
  0x1078: str x0, [x16, #0x10]
  0x107c: str d2, [x16, #0x18]
  0x1080: ret
  0x1088: 1.0
  0x1090: sin

#)

#( expected results

  true 1 -> 0.8414709848078965 0.9092974268256817 true true
  1 2 -> 0.9092974268256817 0.1411200080598672 true 1

#)

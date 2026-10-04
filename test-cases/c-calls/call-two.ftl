argument a
return sin(a) ^ a

#( expected schedule aarch64

  I0: argument
  I1: ldr sin
  I3: ldr pow
  I2: blr I1 I0
  I4: blr I3 I2 I0
  I5: ret I4

#)

#( expected assembly aarch64 f64

  0x1000: str x19, [sp, #-0x10]!
  0x1004: str d8, [sp, #-0x10]!
  0x1008: ldr x9, #0x1058
  0x100c: ldr x19, #0x1060
  0x1010: fmov d8, d0
  0x1014: str x30, [sp, #-0x10]!
  0x1018: blr x9
  0x101c: ldr x30, [sp], #0x10
  0x1020: fmov d1, d8
  0x1024: str x30, [sp, #-0x10]!
  0x1028: blr x19
  0x102c: ldr x30, [sp], #0x10
  0x1030: ldr d8, [sp], #0x10
  0x1034: ldr x19, [sp], #0x10
  0x1038: ret
  0x103c: mov x16, x0
  0x1040: stp x1, x30, [sp, #-0x10]!
  0x1044: ldr d0, [x16]
  0x1048: bl #0x1000
  0x104c: ldp x16, x30, [sp], #0x10
  0x1050: str d0, [x16]
  0x1054: ret
  0x1058: sin
  0x1060: pow

#)

#( expected results

    0 -> 1
    0.5 -> 0.6924056171090779

#)

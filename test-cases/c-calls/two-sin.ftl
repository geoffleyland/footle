argument a, b
return sin(b), sin(a)

#( expected statements

  argument a, b
  return sin(b), sin(a)

#)

#( expected vir

  argument I0
  argument I1
  local I2 = sin(I1)
  local I3 = sin(I0)
  return I2, I3

#)

#( expected schedule

  I0: argument
  I1: argument
  I2: ldr sin
  I4: blr I2 I0
  I3: blr I2 I1
  I5: ret I3 I4

#)

#( expected assembly

  0x1000: str x19, [sp, #-0x10]!
  0x1004: stp d8, d9, [sp, #-0x10]!
  0x1008: ldr x19, #0x106c
  0x100c: fmov d8, d1
  0x1010: str x30, [sp, #-0x10]!
  0x1014: blr x19
  0x1018: ldr x30, [sp], #0x10
  0x101c: fmov d9, d8
  0x1020: fmov d8, d0
  0x1024: fmov d0, d9
  0x1028: str x30, [sp, #-0x10]!
  0x102c: blr x19
  0x1030: ldr x30, [sp], #0x10
  0x1034: fmov d1, d8
  0x1038: ldp d8, d9, [sp], #0x10
  0x103c: ldr x19, [sp], #0x10
  0x1040: ret
  0x1044: mov x16, x0
  0x1048: stp x1, x30, [sp, #-0x10]!
  0x104c: ldr d0, [x16]
  0x1050: ldr d1, [x16, #8]
  0x1054: bl #0x1000
  0x1058: ldp x16, x30, [sp], #0x10
  0x105c: str d0, [x16]
  0x1060: str d1, [x16, #8]
  0x1064: ret
  0x106c: sin

#)

#( expected results
  0 0 -> 0 0
  1.57079632679 0 -> 0 1
#)

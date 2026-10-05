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

#( expected schedule aarch64

  I0: argument
  I1: argument
  I2: ldr sin
  I4: blr I2 I0
  I3: blr I2 I1
  I5: ret I3 I4

#)

#( expected assembly aarch64 f64 f64

  0x1000: stp x19, x30, [sp, #-0x10]!
  0x1004: str d8, [sp, #-0x10]!
  0x1008: ldr x19, #0x1058
  0x100c: fmov d8, d1
  0x1010: blr x19
  0x1014: fmov d16, d8
  0x1018: fmov d8, d0
  0x101c: fmov d0, d16
  0x1020: blr x19
  0x1024: fmov d1, d8
  0x1028: ldr d8, [sp], #0x10
  0x102c: ldp x19, x30, [sp], #0x10
  0x1030: ret
  0x1034: mov x16, x0
  0x1038: stp x1, x30, [sp, #-0x10]!
  0x103c: ldr d0, [x16]
  0x1040: ldr d1, [x16, #8]
  0x1044: bl #0x1000
  0x1048: ldp x16, x30, [sp], #0x10
  0x104c: str d0, [x16]
  0x1050: str d1, [x16, #8]
  0x1054: ret
  0x1058: sin

#)

#( expected results
  0 0 -> 0 0
  1.57079632679 0 -> 0 1
#)

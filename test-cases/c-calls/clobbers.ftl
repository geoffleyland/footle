argument a, b
local c = a * sin(b)
return a, b, c

#( expected statements

  argument a, b
  local c = (a * sin(b))
  return a, b, c

#)

#( expected vir

  argument I0
  argument I1
  local I2 = sin(I1)
  local I3 = I0 * I2
  return I0, I1, I3

#)

#( expected schedule

  I0: argument
  I1: argument
  I2: ldr sin
  I3: blr I2 I1
  I4: fmul I0 I3
  I5: ret I0 I1 I4

#)

#( expected assembler

  0x1000: stp d8, d9, [sp, #-0x10]!
  0x1004: ldr x16, #0x105c
  0x1008: fmov d8, d0
  0x100c: fmov d0, d1
  0x1010: fmov d9, d1
  0x1014: str x30, [sp, #-0x10]!
  0x1018: blr x16
  0x101c: ldr x30, [sp], #0x10
  0x1020: fmul d2, d8, d0
  0x1024: fmov d0, d8
  0x1028: fmov d1, d9
  0x102c: ldp d8, d9, [sp], #0x10
  0x1030: ret
  0x1034: mov x16, x0
  0x1038: stp x1, x30, [sp, #-0x10]!
  0x103c: ldr d0, [x16]
  0x1040: ldr d1, [x16, #8]
  0x1044: bl #0x1000
  0x1048: ldp x16, x30, [sp], #0x10
  0x104c: str d0, [x16]
  0x1050: str d1, [x16, #8]
  0x1054: str d2, [x16, #0x10]
  0x1058: ret
  0x105c: sin

#)

#( expected results
  0 1 -> 0 1 0
  1 0 -> 1 0 0
  1 1 -> 1 1 0.8414709848078965
#)

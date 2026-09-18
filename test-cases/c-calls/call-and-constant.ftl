argument a

return sin(a * 1.57079632679)

#( expected statements

  argument a
  return sin((a * 1.57079632679))

#)

#( expected vir

  argument I0
  local I1 = 1.57079632679
  local I2 = I0 * I1
  local I3 = sin(I2)
  return I3

#)

#( expected schedule

  I0: argument
  I1: ldr K0
  I3: ldr sin
  I2: fmul I0 I1
  I4: blr I3 I2
  I5: ret I4
  K0: 1.57079632679

#)

#( expected assembler

  0x1000: ldr d16, #0x103c
  0x1004: ldr x9, #0x1044
  0x1008: fmul d0, d0, d16
  0x100c: str x30, [sp, #-0x10]!
  0x1010: blr x9
  0x1014: ldr x30, [sp], #0x10
  0x1018: ret
  0x101c: mov x16, x0
  0x1020: stp x1, x30, [sp, #-0x10]!
  0x1024: ldr d0, [x16]
  0x1028: bl #0x1000
  0x102c: ldp x16, x30, [sp], #0x10
  0x1030: str d0, [x16]
  0x1034: ret
  0x103c: 1.57079632679
  0x1044: sin

#)

#( expected results
 1 -> 1
 2 -> 0.000000000009793177720293495
#)

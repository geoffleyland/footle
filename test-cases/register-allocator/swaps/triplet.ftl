argument a, b, c
return c, a, b

#( expected statements

  argument a, b, c
  return c, a, b

#)

#( expected vir

  argument I0
  argument I1
  argument I2
  return I2, I0, I1

#)

#( expected schedule

  I0: argument
  I1: argument
  I2: argument
  I3: ret I2 I0 I1

#)

#( expected assembler

  0x1000: fmov d16, d2
  0x1004: fmov d2, d1
  0x1008: fmov d1, d0
  0x100c: fmov d0, d16
  0x1010: ret
  0x1014: mov x16, x0
  0x1018: stp x1, x30, [sp, #-0x10]!
  0x101c: ldr d0, [x16]
  0x1020: ldr d1, [x16, #8]
  0x1024: ldr d2, [x16, #0x10]
  0x1028: bl #0x1000
  0x102c: ldp x16, x30, [sp], #0x10
  0x1030: str d0, [x16]
  0x1034: str d1, [x16, #8]
  0x1038: str d2, [x16, #0x10]
  0x103c: ret

#)

#( expected results

  1 2 3 -> 3 1 2

#)

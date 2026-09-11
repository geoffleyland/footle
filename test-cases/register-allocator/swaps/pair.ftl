argument a, b
return b, a

#( expected statements

  argument a, b
  return b, a

#)

#( expected vir

  argument I0
  argument I1
  return I1, I0

#)

#( expected schedule

  I0: argument
  I1: argument
  I2: ret I1 I0

#)

#( expected assembler

  0x1000: fmov d16, d1
  0x1004: fmov d1, d0
  0x1008: fmov d0, d16
  0x100c: ret
  0x1010: mov x16, x0
  0x1014: stp x1, x30, [sp, #-0x10]!
  0x1018: ldr d0, [x16]
  0x101c: ldr d1, [x16, #8]
  0x1020: bl #0x1000
  0x1024: ldp x16, x30, [sp], #0x10
  0x1028: str d0, [x16]
  0x102c: str d1, [x16, #8]
  0x1030: ret

#)

#( expected results

  1 2 -> 2 1
  2 3 -> 3 2
  2 1 -> 1 2

#)

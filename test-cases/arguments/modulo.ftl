argument a, b
return b % a

#( expected statements

  argument a, b
  return (b % a)

#)

#( expected vir

  argument I0
  argument I1
  local I2 = I1 % I0
  return I2

#)

#( expected schedule

  I0: argument
  I1: argument
  I2: fdiv I1 I0
  I3: frintz I2
  I4: fmsub I3 I0 I1
  I5: ret I4

#)

#( expected assembler

  0x1000: fdiv d16, d1, d0
  0x1004: frintz d16, d16
  0x1008: fmsub d0, d16, d0, d1
  0x100c: ret
  0x1010: mov x16, x0
  0x1014: stp x1, x30, [sp, #-0x10]!
  0x1018: ldr d0, [x16]
  0x101c: ldr d1, [x16, #8]
  0x1020: bl #0x1000
  0x1024: ldp x16, x30, [sp], #0x10
  0x1028: str d0, [x16]
  0x102c: ret

#)

#( expected results
    3 7 -> 1
#)

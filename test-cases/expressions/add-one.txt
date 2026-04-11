argument a
return a + 1

#( expected statements

  argument a
  return (a + 1)

#)

#( expected vir

  argument I0
  local I1 = 1
  local I2 = I0 + I1
  return I2

#)

#( expected schedule

  I0: argument
  I1: ldr K0
  I2: fadd I0 I1
  I3: ret I2
  K0: 1.0

#)

#( expected assembler

  0x1000: ldr d16, #0x102c
  0x1004: fadd d0, d0, d16
  0x1008: ret
  0x100c: mov x16, x0
  0x1010: stp x1, x30, [sp, #-0x10]!
  0x1014: ldr d0, [x16]
  0x1018: bl #0x1000
  0x101c: ldp x16, x30, [sp], #0x10
  0x1020: str d0, [x16]
  0x1024: ret
  0x102c: 1.0

#)

#( expected results

  1 -> 2
  2 -> 3
  3 -> 4

#)

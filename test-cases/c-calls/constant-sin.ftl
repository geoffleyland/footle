return sin(1.57079632679)

#( expected statements

  return sin(1.57079632679)

#)

#( expected vir

  local I0 = 1
  return I0

#)

#( expected schedule

  I0: ldr K0
  I1: ret I0
  K0: 1.0

#)

#( expected assembler

  0x1000: ldr d0, #0x1024
  0x1004: ret
  0x1008: mov x16, x0
  0x100c: stp x1, x30, [sp, #-0x10]!
  0x1010: bl #0x1000
  0x1014: ldp x16, x30, [sp], #0x10
  0x1018: str d0, [x16]
  0x101c: ret
  0x1024: 1.0

#)

#( expected results
  -> 1
#)

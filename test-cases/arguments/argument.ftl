argument a, b
return a, b

#( expected statements

    argument a, b
    return a, b

#)

#( expected vir

    argument I0
    argument I1
    return I0, I1

#)

#( expected schedule

  I0: argument
  I1: argument
  I2: ret I0 I1

#)

#( expected assembler

  0x1000: ret
  0x1004: mov x16, x0
  0x1008: stp x1, x30, [sp, #-0x10]!
  0x100c: ldr d0, [x16]
  0x1010: ldr d1, [x16, #8]
  0x1014: bl #0x1000
  0x1018: ldp x16, x30, [sp], #0x10
  0x101c: str d0, [x16]
  0x1020: str d1, [x16, #8]
  0x1024: ret

#)

#( expected results

  1 2 -> 1 2
  2 3 -> 2 3
  2 1 -> 2 1

#)

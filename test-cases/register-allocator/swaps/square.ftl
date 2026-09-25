argument a
return a * a, a

#( expected schedule

  I0: argument
  I1: fmul I0 I0
  I2: ret I1 I0

#)

#( expected assembly f64

  0x1000: fmul d16, d0, d0
  0x1004: fmov d1, d0
  0x1008: fmov d0, d16
  0x100c: ret
  0x1010: mov x16, x0
  0x1014: stp x1, x30, [sp, #-0x10]!
  0x1018: ldr d0, [x16]
  0x101c: bl #0x1000
  0x1020: ldp x16, x30, [sp], #0x10
  0x1024: str d0, [x16]
  0x1028: str d1, [x16, #8]
  0x102c: ret

#)

#( expected results
  3 -> 9 3
#)

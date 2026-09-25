argument a, b, c, d, e, f, g
return b, c, a, g, d, e

#( expected schedule

  I0: argument
  I1: argument
  I2: argument
  I3: argument
  I4: argument
  I5: argument
  I6: argument
  I7: ret I1 I2 I0 I6 I3 I4

#)

#( expected assembly

  0x1000: fmov d5, d4
  0x1004: fmov d4, d3
  0x1008: fmov d3, d6
  0x100c: fmov d16, d1
  0x1010: fmov d1, d2
  0x1014: fmov d2, d0
  0x1018: fmov d0, d16
  0x101c: ret
  0x1020: mov x16, x0
  0x1024: stp x1, x30, [sp, #-0x10]!
  0x1028: ldr d0, [x16]
  0x102c: ldr d1, [x16, #8]
  0x1030: ldr d2, [x16, #0x10]
  0x1034: ldr d3, [x16, #0x18]
  0x1038: ldr d4, [x16, #0x20]
  0x103c: ldr d5, [x16, #0x28]
  0x1040: ldr d6, [x16, #0x30]
  0x1044: bl #0x1000
  0x1048: ldp x16, x30, [sp], #0x10
  0x104c: str d0, [x16]
  0x1050: str d1, [x16, #8]
  0x1054: str d2, [x16, #0x10]
  0x1058: str d3, [x16, #0x18]
  0x105c: str d4, [x16, #0x20]
  0x1060: str d5, [x16, #0x28]
  0x1064: ret

#)

#( expected results
  1 2 3 4 5 6 7 -> 2 3 1 7 4 5
#)

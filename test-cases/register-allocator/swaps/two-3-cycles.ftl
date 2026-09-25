argument a, b, c, d, e, f
return b, c, a, f, d, e

#( expected schedule

  I0: argument
  I1: argument
  I2: argument
  I3: argument
  I4: argument
  I5: argument
  I6: ret I1 I2 I0 I5 I3 I4

#)

#( expected assembly f64 f64 f64 f64 f64 f64

  0x1000: fmov d16, d1
  0x1004: fmov d1, d2
  0x1008: fmov d2, d0
  0x100c: fmov d0, d16
  0x1010: fmov d16, d5
  0x1014: fmov d5, d4
  0x1018: fmov d4, d3
  0x101c: fmov d3, d16
  0x1020: ret
  0x1024: mov x16, x0
  0x1028: stp x1, x30, [sp, #-0x10]!
  0x102c: ldr d0, [x16]
  0x1030: ldr d1, [x16, #8]
  0x1034: ldr d2, [x16, #0x10]
  0x1038: ldr d3, [x16, #0x18]
  0x103c: ldr d4, [x16, #0x20]
  0x1040: ldr d5, [x16, #0x28]
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
  1 2 3 4 5 6 -> 2 3 1 6 4 5
#)

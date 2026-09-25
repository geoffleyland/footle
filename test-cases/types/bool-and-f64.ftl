return false, 1.3

#( expected schedule

  I1: ldr K0
  I0: mov #0
  I2: ret I0 I1
  K0: 1.3

#)

#( expected assembly

  0x1000: ldr d0, #0x102c
  0x1004: mov x0, #0
  0x1008: ret
  0x100c: mov x16, x0
  0x1010: stp x1, x30, [sp, #-0x10]!
  0x1014: bl #0x1000
  0x1018: ldp x16, x30, [sp], #0x10
  0x101c: str x0, [x16]
  0x1020: str d0, [x16, #8]
  0x1024: ret
  0x102c: 1.3

#)

#( expected results
    -> false 1.3
#)

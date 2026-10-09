argument a
return a * a, a

#( expected schedule aarch64

  I0: argument
  I1: fmul I0 I0
  I2: ret I1 I0

#)

#( expected schedule riscv64

  I0: argument
  I1: fmul.d I0 I0
  I2: ret I1 I0

#)

#( expected assembly aarch64 f64

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

#( expected assembly riscv64 f64

  0x1000: fmul.d ft0, fa0, fa0
  0x1004: fmv.d fa1, fa0
  0x1008: fmv.d fa0, ft0
  0x100c: ret
  0x1010: mv t6, a0
  0x1014: addi sp, sp, -0x10
  0x1018: sd a1, 0(sp)
  0x101c: sd ra, 8(sp)
  0x1020: fld fa0, 0(t6)
  0x1024: jal -0x24
  0x1028: ld t6, 0(sp)
  0x102c: ld ra, 8(sp)
  0x1030: addi sp, sp, 0x10
  0x1034: fsd fa0, 0(t6)
  0x1038: fsd fa1, 8(t6)
  0x103c: ret

#)

#( expected results
  3 -> 9 3
#)

argument a, b, c, d
return d, c, a, b

#( expected schedule

  I0: argument
  I1: argument
  I2: argument
  I3: argument
  I4: ret I3 I2 I0 I1

#)

#( expected assembly aarch64 f64 f64 f64 f64

  0x1000: fmov d16, d3
  0x1004: fmov d3, d1
  0x1008: fmov d1, d2
  0x100c: fmov d2, d0
  0x1010: fmov d0, d16
  0x1014: ret
  0x1018: mov x16, x0
  0x101c: stp x1, x30, [sp, #-0x10]!
  0x1020: ldr d0, [x16]
  0x1024: ldr d1, [x16, #8]
  0x1028: ldr d2, [x16, #0x10]
  0x102c: ldr d3, [x16, #0x18]
  0x1030: bl #0x1000
  0x1034: ldp x16, x30, [sp], #0x10
  0x1038: str d0, [x16]
  0x103c: str d1, [x16, #8]
  0x1040: str d2, [x16, #0x10]
  0x1044: str d3, [x16, #0x18]
  0x1048: ret

#)

#( expected assembly riscv64 f64 f64 f64 f64

  0x1000: fmv.d ft0, fa3
  0x1004: fmv.d fa3, fa1
  0x1008: fmv.d fa1, fa2
  0x100c: fmv.d fa2, fa0
  0x1010: fmv.d fa0, ft0
  0x1014: ret
  0x1018: mv t6, a0
  0x101c: addi sp, sp, -0x10
  0x1020: sd a1, 0(sp)
  0x1024: sd ra, 8(sp)
  0x1028: fld fa0, 0(t6)
  0x102c: fld fa1, 8(t6)
  0x1030: fld fa2, 0x10(t6)
  0x1034: fld fa3, 0x18(t6)
  0x1038: jal -0x38
  0x103c: ld t6, 0(sp)
  0x1040: ld ra, 8(sp)
  0x1044: addi sp, sp, 0x10
  0x1048: fsd fa0, 0(t6)
  0x104c: fsd fa1, 8(t6)
  0x1050: fsd fa2, 0x10(t6)
  0x1054: fsd fa3, 0x18(t6)
  0x1058: ret

#)

#( expected results
  1 2 3 4 -> 4 3 1 2
#)

argument a, b, c, d
return d, a, c, b

#( expected schedule

  I0: argument
  I1: argument
  I2: argument
  I3: argument
  I4: ret I3 I0 I2 I1

#)

#( expected assembly aarch64 f64 f64 f64 f64

  0x1000: fmov d16, d3
  0x1004: fmov d3, d1
  0x1008: fmov d1, d0
  0x100c: fmov d0, d16
  0x1010: ret
  0x1014: mov x16, x0
  0x1018: stp x1, x30, [sp, #-0x10]!
  0x101c: ldr d0, [x16]
  0x1020: ldr d1, [x16, #8]
  0x1024: ldr d2, [x16, #0x10]
  0x1028: ldr d3, [x16, #0x18]
  0x102c: bl #0x1000
  0x1030: ldp x16, x30, [sp], #0x10
  0x1034: str d0, [x16]
  0x1038: str d1, [x16, #8]
  0x103c: str d2, [x16, #0x10]
  0x1040: str d3, [x16, #0x18]
  0x1044: ret

#)

#( expected assembly riscv64 f64 f64 f64 f64

  0x1000: fmv.d ft0, fa3
  0x1004: fmv.d fa3, fa1
  0x1008: fmv.d fa1, fa0
  0x100c: fmv.d fa0, ft0
  0x1010: ret
  0x1014: mv t6, a0
  0x1018: addi sp, sp, -0x10
  0x101c: sd a1, 0(sp)
  0x1020: sd ra, 8(sp)
  0x1024: fld fa0, 0(t6)
  0x1028: fld fa1, 8(t6)
  0x102c: fld fa2, 0x10(t6)
  0x1030: fld fa3, 0x18(t6)
  0x1034: jal -0x34
  0x1038: ld t6, 0(sp)
  0x103c: ld ra, 8(sp)
  0x1040: addi sp, sp, 0x10
  0x1044: fsd fa0, 0(t6)
  0x1048: fsd fa1, 8(t6)
  0x104c: fsd fa2, 0x10(t6)
  0x1050: fsd fa3, 0x18(t6)
  0x1054: ret

#)

#( expected results

    1 2 3 4 -> 4 1 3 2

#)

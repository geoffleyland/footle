argument a, b
return b, a, b

#( expected statements

  argument a, b
  return b, a, b

#)

#( expected vir

  argument I0
  argument I1
  return I1, I0, I1

#)

#( expected schedule

  I0: argument
  I1: argument
  I2: ret I1 I0 I1

#)

#( expected assembly aarch64 f64 f64

  0x1000: fmov d2, d1
  0x1004: fmov d1, d0
  0x1008: fmov d0, d2
  0x100c: ret
  0x1010: mov x16, x0
  0x1014: stp x1, x30, [sp, #-0x10]!
  0x1018: ldr d0, [x16]
  0x101c: ldr d1, [x16, #8]
  0x1020: bl #0x1000
  0x1024: ldp x16, x30, [sp], #0x10
  0x1028: str d0, [x16]
  0x102c: str d1, [x16, #8]
  0x1030: str d2, [x16, #0x10]
  0x1034: ret

#)

#( expected assembly riscv64 f64 f64

  0x1000: fmv.d fa2, fa1
  0x1004: fmv.d fa1, fa0
  0x1008: fmv.d fa0, fa2
  0x100c: ret
  0x1010: mv t6, a0
  0x1014: addi sp, sp, -0x10
  0x1018: sd a1, 0(sp)
  0x101c: sd ra, 8(sp)
  0x1020: fld fa0, 0(t6)
  0x1024: fld fa1, 8(t6)
  0x1028: jal -0x28
  0x102c: ld t6, 0(sp)
  0x1030: ld ra, 8(sp)
  0x1034: addi sp, sp, 0x10
  0x1038: fsd fa0, 0(t6)
  0x103c: fsd fa1, 8(t6)
  0x1040: fsd fa2, 0x10(t6)
  0x1044: ret

#)

#( expected results
  1 2 -> 2 1 2
#)

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

#( expected assembly aarch64 f64 f64 f64 f64 f64 f64

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

#( expected assembly aarch64 f64 f64 f64 bool bool bool

  0x1000: fmov d16, d1
  0x1004: fmov d1, d2
  0x1008: fmov d2, d0
  0x100c: fmov d0, d16
  0x1010: mov x9, x2
  0x1014: mov x2, x1
  0x1018: mov x1, x0
  0x101c: mov x0, x9
  0x1020: ret
  0x1024: mov x16, x0
  0x1028: stp x1, x30, [sp, #-0x10]!
  0x102c: ldr d0, [x16]
  0x1030: ldr d1, [x16, #8]
  0x1034: ldr d2, [x16, #0x10]
  0x1038: ldr x0, [x16, #0x18]
  0x103c: ldr x1, [x16, #0x20]
  0x1040: ldr x2, [x16, #0x28]
  0x1044: bl #0x1000
  0x1048: ldp x16, x30, [sp], #0x10
  0x104c: str d0, [x16]
  0x1050: str d1, [x16, #8]
  0x1054: str d2, [x16, #0x10]
  0x1058: str x0, [x16, #0x18]
  0x105c: str x1, [x16, #0x20]
  0x1060: str x2, [x16, #0x28]
  0x1064: ret

#)

#( expected assembly aarch64 bool bool bool f64 f64 f64

  0x1000: mov x9, x1
  0x1004: mov x1, x2
  0x1008: mov x2, x0
  0x100c: mov x0, x9
  0x1010: fmov d16, d2
  0x1014: fmov d2, d1
  0x1018: fmov d1, d0
  0x101c: fmov d0, d16
  0x1020: ret
  0x1024: mov x16, x0
  0x1028: stp x1, x30, [sp, #-0x10]!
  0x102c: ldr x0, [x16]
  0x1030: ldr x1, [x16, #8]
  0x1034: ldr x2, [x16, #0x10]
  0x1038: ldr d0, [x16, #0x18]
  0x103c: ldr d1, [x16, #0x20]
  0x1040: ldr d2, [x16, #0x28]
  0x1044: bl #0x1000
  0x1048: ldp x16, x30, [sp], #0x10
  0x104c: str x0, [x16]
  0x1050: str x1, [x16, #8]
  0x1054: str x2, [x16, #0x10]
  0x1058: str d0, [x16, #0x18]
  0x105c: str d1, [x16, #0x20]
  0x1060: str d2, [x16, #0x28]
  0x1064: ret

#)

#( expected assembly riscv64 f64 f64 f64 f64 f64 f64

  0x1000: fmv.d ft0, fa1
  0x1004: fmv.d fa1, fa2
  0x1008: fmv.d fa2, fa0
  0x100c: fmv.d fa0, ft0
  0x1010: fmv.d ft0, fa5
  0x1014: fmv.d fa5, fa4
  0x1018: fmv.d fa4, fa3
  0x101c: fmv.d fa3, ft0
  0x1020: ret
  0x1024: mv t6, a0
  0x1028: addi sp, sp, -0x10
  0x102c: sd a1, 0(sp)
  0x1030: sd ra, 8(sp)
  0x1034: fld fa0, 0(t6)
  0x1038: fld fa1, 8(t6)
  0x103c: fld fa2, 0x10(t6)
  0x1040: fld fa3, 0x18(t6)
  0x1044: fld fa4, 0x20(t6)
  0x1048: fld fa5, 0x28(t6)
  0x104c: jal -0x4c
  0x1050: ld t6, 0(sp)
  0x1054: ld ra, 8(sp)
  0x1058: addi sp, sp, 0x10
  0x105c: fsd fa0, 0(t6)
  0x1060: fsd fa1, 8(t6)
  0x1064: fsd fa2, 0x10(t6)
  0x1068: fsd fa3, 0x18(t6)
  0x106c: fsd fa4, 0x20(t6)
  0x1070: fsd fa5, 0x28(t6)
  0x1074: ret

#)

#( expected assembly riscv64 f64 f64 f64 bool bool bool

  0x1000: fmv.d ft0, fa1
  0x1004: fmv.d fa1, fa2
  0x1008: fmv.d fa2, fa0
  0x100c: fmv.d fa0, ft0
  0x1010: mv t0, a2
  0x1014: mv a2, a1
  0x1018: mv a1, a0
  0x101c: mv a0, t0
  0x1020: ret
  0x1024: mv t6, a0
  0x1028: addi sp, sp, -0x10
  0x102c: sd a1, 0(sp)
  0x1030: sd ra, 8(sp)
  0x1034: fld fa0, 0(t6)
  0x1038: fld fa1, 8(t6)
  0x103c: fld fa2, 0x10(t6)
  0x1040: ld a0, 0x18(t6)
  0x1044: ld a1, 0x20(t6)
  0x1048: ld a2, 0x28(t6)
  0x104c: jal -0x4c
  0x1050: ld t6, 0(sp)
  0x1054: ld ra, 8(sp)
  0x1058: addi sp, sp, 0x10
  0x105c: fsd fa0, 0(t6)
  0x1060: fsd fa1, 8(t6)
  0x1064: fsd fa2, 0x10(t6)
  0x1068: sd a0, 0x18(t6)
  0x106c: sd a1, 0x20(t6)
  0x1070: sd a2, 0x28(t6)
  0x1074: ret

#)

#( expected assembly riscv64 bool bool bool f64 f64 f64

  0x1000: mv t0, a1
  0x1004: mv a1, a2
  0x1008: mv a2, a0
  0x100c: mv a0, t0
  0x1010: fmv.d ft0, fa2
  0x1014: fmv.d fa2, fa1
  0x1018: fmv.d fa1, fa0
  0x101c: fmv.d fa0, ft0
  0x1020: ret
  0x1024: mv t6, a0
  0x1028: addi sp, sp, -0x10
  0x102c: sd a1, 0(sp)
  0x1030: sd ra, 8(sp)
  0x1034: ld a0, 0(t6)
  0x1038: ld a1, 8(t6)
  0x103c: ld a2, 0x10(t6)
  0x1040: fld fa0, 0x18(t6)
  0x1044: fld fa1, 0x20(t6)
  0x1048: fld fa2, 0x28(t6)
  0x104c: jal -0x4c
  0x1050: ld t6, 0(sp)
  0x1054: ld ra, 8(sp)
  0x1058: addi sp, sp, 0x10
  0x105c: sd a0, 0(t6)
  0x1060: sd a1, 8(t6)
  0x1064: sd a2, 0x10(t6)
  0x1068: fsd fa0, 0x18(t6)
  0x106c: fsd fa1, 0x20(t6)
  0x1070: fsd fa2, 0x28(t6)
  0x1074: ret

#)

#( expected results
  1 2 3 4 5 6 -> 2 3 1 6 4 5
  1 2 3 true false true -> 2 3 1 true true false
  true false true 4 5 6 -> false true true 6 4 5
#)

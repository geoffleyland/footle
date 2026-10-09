argument b, x
local s = sin(x)
local u = sin(x + 1)
return s, u, true, b

#( expected schedule aarch64

  I0: argument
  I1: argument
  I4: ldr K0
  I2: ldr sin
  I7: mov #1
  I5: fadd I1 I4
  I3: blr I2 I1
  I6: blr I2 I5
  I8: ret I3 I6 I7 I0
  K0: 1.0

#)

#( expected schedule riscv64

  I0: argument
  I1: argument
  I5: auipc K0
  I2: auipc sin
  I9: li #1
  I6: fld I5 K0
  I3: ld I2 sin
  I7: fadd.d I1 I6
  I4: jalr I3 I1
  I8: jalr I3 I7
  I10: ret I4 I8 I9 I0
  K0: 1.0

#)

#( expected assembly aarch64 bool f64

  0x1000: stp x19, x20, [sp, #-0x10]!
  0x1004: stp x21, x30, [sp, #-0x10]!
  0x1008: str d8, [sp, #-0x10]!
  0x100c: ldr d16, #0x1080
  0x1010: ldr x21, #0x1088
  0x1014: mov x19, #1
  0x1018: fadd d8, d0, d16
  0x101c: mov x20, x0
  0x1020: blr x21
  0x1024: fmov d16, d8
  0x1028: fmov d8, d0
  0x102c: fmov d0, d16
  0x1030: blr x21
  0x1034: fmov d1, d0
  0x1038: fmov d0, d8
  0x103c: mov x0, x19
  0x1040: mov x1, x20
  0x1044: ldr d8, [sp], #0x10
  0x1048: ldp x21, x30, [sp], #0x10
  0x104c: ldp x19, x20, [sp], #0x10
  0x1050: ret
  0x1054: mov x16, x0
  0x1058: stp x1, x30, [sp, #-0x10]!
  0x105c: ldr x0, [x16]
  0x1060: ldr d0, [x16, #8]
  0x1064: bl #0x1000
  0x1068: ldp x16, x30, [sp], #0x10
  0x106c: str d0, [x16]
  0x1070: str d1, [x16, #8]
  0x1074: str x0, [x16, #0x10]
  0x1078: str x1, [x16, #0x18]
  0x107c: ret
  0x1080: 1.0
  0x1088: sin

#)

#( expected assembly aarch64 f64 f64

  0x1000: stp x19, x20, [sp, #-0x10]!
  0x1004: str x30, [sp, #-0x10]!
  0x1008: stp d8, d9, [sp, #-0x10]!
  0x100c: ldr d16, #0x1088
  0x1010: ldr x20, #0x1090
  0x1014: mov x19, #1
  0x1018: fadd d8, d1, d16
  0x101c: fmov d9, d0
  0x1020: fmov d0, d1
  0x1024: blr x20
  0x1028: fmov d16, d8
  0x102c: fmov d8, d0
  0x1030: fmov d0, d16
  0x1034: blr x20
  0x1038: fmov d1, d0
  0x103c: fmov d0, d8
  0x1040: mov x0, x19
  0x1044: fmov d2, d9
  0x1048: ldp d8, d9, [sp], #0x10
  0x104c: ldr x30, [sp], #0x10
  0x1050: ldp x19, x20, [sp], #0x10
  0x1054: ret
  0x1058: mov x16, x0
  0x105c: stp x1, x30, [sp, #-0x10]!
  0x1060: ldr d0, [x16]
  0x1064: ldr d1, [x16, #8]
  0x1068: bl #0x1000
  0x106c: ldp x16, x30, [sp], #0x10
  0x1070: str d0, [x16]
  0x1074: str d1, [x16, #8]
  0x1078: str x0, [x16, #0x10]
  0x107c: str d2, [x16, #0x18]
  0x1080: ret
  0x1088: 1.0
  0x1090: sin

#)

#( expected assembly riscv64 bool f64

  0x1000: addi sp, sp, -0x30
  0x1004: sd ra, 0(sp)
  0x1008: sd s1, 8(sp)
  0x100c: sd s2, 0x10(sp)
  0x1010: sd s3, 0x18(sp)
  0x1014: fsd fs0, 0x20(sp)
  0x1018: auipc t0, 0
  0x101c: auipc t1, 0
  0x1020: addi s1, zero, 1
  0x1024: fld ft0, 0x98(t0)
  0x1028: ld s3, 0x9c(t1)
  0x102c: fadd.d fs0, fa0, ft0
  0x1030: mv s2, a0
  0x1034: jalr s3
  0x1038: fmv.d ft0, fs0
  0x103c: fmv.d fs0, fa0
  0x1040: fmv.d fa0, ft0
  0x1044: jalr s3
  0x1048: fmv.d fa1, fa0
  0x104c: fmv.d fa0, fs0
  0x1050: mv a0, s1
  0x1054: mv a1, s2
  0x1058: ld ra, 0(sp)
  0x105c: ld s1, 8(sp)
  0x1060: ld s2, 0x10(sp)
  0x1064: ld s3, 0x18(sp)
  0x1068: fld fs0, 0x20(sp)
  0x106c: addi sp, sp, 0x30
  0x1070: ret
  0x1074: mv t6, a0
  0x1078: addi sp, sp, -0x10
  0x107c: sd a1, 0(sp)
  0x1080: sd ra, 8(sp)
  0x1084: ld a0, 0(t6)
  0x1088: fld fa0, 8(t6)
  0x108c: jal -0x8c
  0x1090: ld t6, 0(sp)
  0x1094: ld ra, 8(sp)
  0x1098: addi sp, sp, 0x10
  0x109c: fsd fa0, 0(t6)
  0x10a0: fsd fa1, 8(t6)
  0x10a4: sd a0, 0x10(t6)
  0x10a8: sd a1, 0x18(t6)
  0x10ac: ret
  0x10b0: 1.0
  0x10b8: sin

#)

#( expected assembly riscv64 f64 f64

  0x1000: addi sp, sp, -0x30
  0x1004: sd ra, 0(sp)
  0x1008: sd s1, 8(sp)
  0x100c: sd s2, 0x10(sp)
  0x1010: fsd fs0, 0x18(sp)
  0x1014: fsd fs1, 0x20(sp)
  0x1018: auipc t0, 0
  0x101c: auipc t1, 0
  0x1020: addi s1, zero, 1
  0x1024: fld ft0, 0xa0(t0)
  0x1028: ld s2, 0xa4(t1)
  0x102c: fadd.d fs0, fa1, ft0
  0x1030: fmv.d fs1, fa0
  0x1034: fmv.d fa0, fa1
  0x1038: jalr s2
  0x103c: fmv.d ft0, fs0
  0x1040: fmv.d fs0, fa0
  0x1044: fmv.d fa0, ft0
  0x1048: jalr s2
  0x104c: fmv.d fa1, fa0
  0x1050: fmv.d fa0, fs0
  0x1054: mv a0, s1
  0x1058: fmv.d fa2, fs1
  0x105c: ld ra, 0(sp)
  0x1060: ld s1, 8(sp)
  0x1064: ld s2, 0x10(sp)
  0x1068: fld fs0, 0x18(sp)
  0x106c: fld fs1, 0x20(sp)
  0x1070: addi sp, sp, 0x30
  0x1074: ret
  0x1078: mv t6, a0
  0x107c: addi sp, sp, -0x10
  0x1080: sd a1, 0(sp)
  0x1084: sd ra, 8(sp)
  0x1088: fld fa0, 0(t6)
  0x108c: fld fa1, 8(t6)
  0x1090: jal -0x90
  0x1094: ld t6, 0(sp)
  0x1098: ld ra, 8(sp)
  0x109c: addi sp, sp, 0x10
  0x10a0: fsd fa0, 0(t6)
  0x10a4: fsd fa1, 8(t6)
  0x10a8: sd a0, 0x10(t6)
  0x10ac: fsd fa2, 0x18(t6)
  0x10b0: ret
  0x10b8: 1.0
  0x10c0: sin

#)

#( expected results

  true 1 -> 0.8414709848078965 0.9092974268256817 true true
  1 2 -> 0.9092974268256817 0.1411200080598672 true 1

#)

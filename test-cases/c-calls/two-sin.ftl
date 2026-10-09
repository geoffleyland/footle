argument a, b
return sin(b), sin(a)

#( expected statements

  argument a, b
  return sin(b), sin(a)

#)

#( expected vir

  argument I0
  argument I1
  local I2 = sin(I1)
  local I3 = sin(I0)
  return I2, I3

#)

#( expected schedule aarch64

  I0: argument
  I1: argument
  I2: ldr sin
  I4: blr I2 I0
  I3: blr I2 I1
  I5: ret I3 I4

#)

#( expected schedule riscv64

  I0: argument
  I1: argument
  I2: auipc sin
  I3: ld I2 sin
  I5: jalr I3 I0
  I4: jalr I3 I1
  I6: ret I4 I5

#)

#( expected assembly aarch64 f64 f64

  0x1000: stp x19, x30, [sp, #-0x10]!
  0x1004: str d8, [sp, #-0x10]!
  0x1008: ldr x19, #0x1058
  0x100c: fmov d8, d1
  0x1010: blr x19
  0x1014: fmov d16, d8
  0x1018: fmov d8, d0
  0x101c: fmov d0, d16
  0x1020: blr x19
  0x1024: fmov d1, d8
  0x1028: ldr d8, [sp], #0x10
  0x102c: ldp x19, x30, [sp], #0x10
  0x1030: ret
  0x1034: mov x16, x0
  0x1038: stp x1, x30, [sp, #-0x10]!
  0x103c: ldr d0, [x16]
  0x1040: ldr d1, [x16, #8]
  0x1044: bl #0x1000
  0x1048: ldp x16, x30, [sp], #0x10
  0x104c: str d0, [x16]
  0x1050: str d1, [x16, #8]
  0x1054: ret
  0x1058: sin

#)

#( expected assembly riscv64 f64 f64

  0x1000: addi sp, sp, -0x20
  0x1004: sd ra, 0(sp)
  0x1008: sd s1, 8(sp)
  0x100c: fsd fs0, 0x10(sp)
  0x1010: auipc t0, 0
  0x1014: ld s1, 0x70(t0)
  0x1018: fmv.d fs0, fa1
  0x101c: jalr s1
  0x1020: fmv.d ft0, fs0
  0x1024: fmv.d fs0, fa0
  0x1028: fmv.d fa0, ft0
  0x102c: jalr s1
  0x1030: fmv.d fa1, fs0
  0x1034: ld ra, 0(sp)
  0x1038: ld s1, 8(sp)
  0x103c: fld fs0, 0x10(sp)
  0x1040: addi sp, sp, 0x20
  0x1044: ret
  0x1048: mv t6, a0
  0x104c: addi sp, sp, -0x10
  0x1050: sd a1, 0(sp)
  0x1054: sd ra, 8(sp)
  0x1058: fld fa0, 0(t6)
  0x105c: fld fa1, 8(t6)
  0x1060: jal -0x60
  0x1064: ld t6, 0(sp)
  0x1068: ld ra, 8(sp)
  0x106c: addi sp, sp, 0x10
  0x1070: fsd fa0, 0(t6)
  0x1074: fsd fa1, 8(t6)
  0x1078: ret
  0x1080: sin

#)

#( expected results
  0 0 -> 0 0
  1.57079632679 0 -> 0 1
#)

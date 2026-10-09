argument a, b
local c = a * sin(b)
return a, b, c

#( expected statements

  argument a, b
  local c = (a * sin(b))
  return a, b, c

#)

#( expected vir

  argument I0
  argument I1
  local I2 = sin(I1)
  local I3 = I0 * I2
  return I0, I1, I3

#)

#( expected schedule aarch64

  I0: argument
  I1: argument
  I2: ldr sin
  I3: blr I2 I1
  I4: fmul I0 I3
  I5: ret I0 I1 I4

#)

#( expected schedule riscv64

  I0: argument
  I1: argument
  I2: auipc sin
  I3: ld I2 sin
  I4: jalr I3 I1
  I5: fmul.d I0 I4
  I6: ret I0 I1 I5

#)

#( expected assembly aarch64 f64 f64

  0x1000: str x30, [sp, #-0x10]!
  0x1004: stp d8, d9, [sp, #-0x10]!
  0x1008: ldr x9, #0x1060
  0x100c: fmov d8, d0
  0x1010: fmov d0, d1
  0x1014: fmov d9, d1
  0x1018: blr x9
  0x101c: fmul d2, d8, d0
  0x1020: fmov d0, d8
  0x1024: fmov d1, d9
  0x1028: ldp d8, d9, [sp], #0x10
  0x102c: ldr x30, [sp], #0x10
  0x1030: ret
  0x1034: mov x16, x0
  0x1038: stp x1, x30, [sp, #-0x10]!
  0x103c: ldr d0, [x16]
  0x1040: ldr d1, [x16, #8]
  0x1044: bl #0x1000
  0x1048: ldp x16, x30, [sp], #0x10
  0x104c: str d0, [x16]
  0x1050: str d1, [x16, #8]
  0x1054: str d2, [x16, #0x10]
  0x1058: ret
  0x1060: sin

#)

#( expected assembly riscv64 f64 f64

  0x1000: addi sp, sp, -0x20
  0x1004: sd ra, 0(sp)
  0x1008: fsd fs0, 8(sp)
  0x100c: fsd fs1, 0x10(sp)
  0x1010: auipc t0, 0
  0x1014: ld t0, 0x70(t0)
  0x1018: fmv.d fs0, fa0
  0x101c: fmv.d fa0, fa1
  0x1020: fmv.d fs1, fa1
  0x1024: jalr t0
  0x1028: fmul.d fa2, fs0, fa0
  0x102c: fmv.d fa0, fs0
  0x1030: fmv.d fa1, fs1
  0x1034: ld ra, 0(sp)
  0x1038: fld fs0, 8(sp)
  0x103c: fld fs1, 0x10(sp)
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
  0x1078: fsd fa2, 0x10(t6)
  0x107c: ret
  0x1080: sin

#)

#( expected results
  0 1 -> 0 1 0
  1 0 -> 1 0 0
  1 1 -> 1 1 0.8414709848078965
#)

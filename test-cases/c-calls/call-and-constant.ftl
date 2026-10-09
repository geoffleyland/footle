argument a

return sin(a * 1.57079632679)

#( expected statements

  argument a
  return sin((a * 1.57079632679))

#)

#( expected vir

  argument I0
  local I1 = 1.57079632679
  local I2 = I0 * I1
  local I3 = sin(I2)
  return I3

#)

#( expected schedule aarch64

  I0: argument
  I1: ldr K0
  I3: ldr sin
  I2: fmul I0 I1
  I4: blr I3 I2
  I5: ret I4
  K0: 1.57079632679

#)

#( expected schedule riscv64

  I0: argument
  I1: auipc K0
  I4: auipc sin
  I2: fld I1 K0
  I5: ld I4 sin
  I3: fmul.d I0 I2
  I6: jalr I5 I3
  I7: ret I6
  K0: 1.57079632679

#)

#( expected assembly aarch64 f64

  0x1000: str x30, [sp, #-0x10]!
  0x1004: ldr d16, #0x1038
  0x1008: ldr x9, #0x1040
  0x100c: fmul d0, d0, d16
  0x1010: blr x9
  0x1014: ldr x30, [sp], #0x10
  0x1018: ret
  0x101c: mov x16, x0
  0x1020: stp x1, x30, [sp, #-0x10]!
  0x1024: ldr d0, [x16]
  0x1028: bl #0x1000
  0x102c: ldp x16, x30, [sp], #0x10
  0x1030: str d0, [x16]
  0x1034: ret
  0x1038: 1.57079632679
  0x1040: sin

#)

#( expected assembly riscv64 f64

  0x1000: addi sp, sp, -0x10
  0x1004: sd ra, 0(sp)
  0x1008: auipc t0, 0
  0x100c: auipc t1, 0
  0x1010: fld ft0, 0x50(t0)
  0x1014: ld t0, 0x54(t1)
  0x1018: fmul.d fa0, fa0, ft0
  0x101c: jalr t0
  0x1020: ld ra, 0(sp)
  0x1024: addi sp, sp, 0x10
  0x1028: ret
  0x102c: mv t6, a0
  0x1030: addi sp, sp, -0x10
  0x1034: sd a1, 0(sp)
  0x1038: sd ra, 8(sp)
  0x103c: fld fa0, 0(t6)
  0x1040: jal -0x40
  0x1044: ld t6, 0(sp)
  0x1048: ld ra, 8(sp)
  0x104c: addi sp, sp, 0x10
  0x1050: fsd fa0, 0(t6)
  0x1054: ret
  0x1058: 1.57079632679
  0x1060: sin

#)

#( expected results
 1 -> 1
 2 -> 0.000000000009793177720293495
#)

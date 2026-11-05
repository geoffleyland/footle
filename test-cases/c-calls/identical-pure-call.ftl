argument a
return sin(a), sin(a)

#( expected statements

  argument a
  return sin(a), sin(a)

#)

#( expected vir

  argument I0
  local I1 = sin(I0)
  return I1, I1

#)

#( expected schedule aarch64

  I0: argument
  I1: ldr sin
  I2: blr I1 I0
  I3: ret I2 I2

#)

#( expected schedule riscv64

  I0: argument
  I1: auipc sin
  I2: ld I1 sin
  I3: jalr I2 I0
  I4: ret I3 I3

#)

#( expected assembly aarch64 f64

  0x1000: str x30, [sp, #-0x10]!
  0x1004: ldr x9, #0x1038
  0x1008: blr x9
  0x100c: fmov d1, d0
  0x1010: ldr x30, [sp], #0x10
  0x1014: ret
  0x1018: mov x16, x0
  0x101c: stp x1, x30, [sp, #-0x10]!
  0x1020: ldr d0, [x16]
  0x1024: bl #0x1000
  0x1028: ldp x16, x30, [sp], #0x10
  0x102c: str d0, [x16]
  0x1030: str d1, [x16, #8]
  0x1034: ret
  0x1038: sin

#)

#( expected assembly riscv64 f64

  0x1000: addi sp, sp, -0x10
  0x1004: sd ra, 0(sp)
  0x1008: auipc t0, 0
  0x100c: ld t0, 0x50(t0)
  0x1010: jalr t0
  0x1014: fmv.d fa1, fa0
  0x1018: ld ra, 0(sp)
  0x101c: addi sp, sp, 0x10
  0x1020: ret
  0x1024: mv t6, a0
  0x1028: addi sp, sp, -0x10
  0x102c: sd a1, 0(sp)
  0x1030: sd ra, 8(sp)
  0x1034: fld fa0, 0(t6)
  0x1038: jal -0x38
  0x103c: ld t6, 0(sp)
  0x1040: ld ra, 8(sp)
  0x1044: addi sp, sp, 0x10
  0x1048: fsd fa0, 0(t6)
  0x104c: fsd fa1, 8(t6)
  0x1050: ret
  0x1058: sin

#)

#( expected results
  0 -> 0 0
  1.57079632679 -> 1 1
#)

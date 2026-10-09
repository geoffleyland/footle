argument a, b
return b % a

#( expected statements

  argument a, b
  return (b % a)

#)

#( expected vir

  argument I0
  argument I1
  local I2 = I1 % I0
  return I2

#)

#( expected schedule aarch64

  I0: argument
  I1: argument
  I2: ldr fmod
  I3: blr I2 I1 I0
  I4: ret I3

#)

#( expected schedule riscv64

  I0: argument
  I1: argument
  I2: auipc fmod
  I3: ld I2 fmod
  I4: jalr I3 I1 I0
  I5: ret I4

#)

#( expected assembly aarch64 f64 f64

  0x1000: str x30, [sp, #-0x10]!
  0x1004: ldr x9, #0x1040
  0x1008: fmov d16, d1
  0x100c: fmov d1, d0
  0x1010: fmov d0, d16
  0x1014: blr x9
  0x1018: ldr x30, [sp], #0x10
  0x101c: ret
  0x1020: mov x16, x0
  0x1024: stp x1, x30, [sp, #-0x10]!
  0x1028: ldr d0, [x16]
  0x102c: ldr d1, [x16, #8]
  0x1030: bl #0x1000
  0x1034: ldp x16, x30, [sp], #0x10
  0x1038: str d0, [x16]
  0x103c: ret
  0x1040: fmod

#)

#( expected assembly riscv64 f64 f64

  0x1000: addi sp, sp, -0x10
  0x1004: sd ra, 0(sp)
  0x1008: auipc t0, 0
  0x100c: ld t0, 0x58(t0)
  0x1010: fmv.d ft0, fa1
  0x1014: fmv.d fa1, fa0
  0x1018: fmv.d fa0, ft0
  0x101c: jalr t0
  0x1020: ld ra, 0(sp)
  0x1024: addi sp, sp, 0x10
  0x1028: ret
  0x102c: mv t6, a0
  0x1030: addi sp, sp, -0x10
  0x1034: sd a1, 0(sp)
  0x1038: sd ra, 8(sp)
  0x103c: fld fa0, 0(t6)
  0x1040: fld fa1, 8(t6)
  0x1044: jal -0x44
  0x1048: ld t6, 0(sp)
  0x104c: ld ra, 8(sp)
  0x1050: addi sp, sp, 0x10
  0x1054: fsd fa0, 0(t6)
  0x1058: ret
  0x1060: fmod

#)

#( expected results
    3 7 -> 1
#)

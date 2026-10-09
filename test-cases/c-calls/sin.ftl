argument a
return sin(a)

#( expected statements

  argument a
  return sin(a)

#)

#( expected vir

  argument I0
  local I1 = sin(I0)
  return I1

#)

#( expected schedule aarch64

  I0: argument
  I1: ldr sin
  I2: blr I1 I0
  I3: ret I2

#)

#( expected schedule riscv64

  I0: argument
  I1: auipc sin
  I2: ld I1 sin
  I3: jalr I2 I0
  I4: ret I3

#)

#( expected assembly aarch64 f64

  0x1000: str x30, [sp, #-0x10]!
  0x1004: ldr x9, #0x1030
  0x1008: blr x9
  0x100c: ldr x30, [sp], #0x10
  0x1010: ret
  0x1014: mov x16, x0
  0x1018: stp x1, x30, [sp, #-0x10]!
  0x101c: ldr d0, [x16]
  0x1020: bl #0x1000
  0x1024: ldp x16, x30, [sp], #0x10
  0x1028: str d0, [x16]
  0x102c: ret
  0x1030: sin

#)

#( expected assembly riscv64 f64

  0x1000: addi sp, sp, -0x10
  0x1004: sd ra, 0(sp)
  0x1008: auipc t0, 0
  0x100c: ld t0, 0x48(t0)
  0x1010: jalr t0
  0x1014: ld ra, 0(sp)
  0x1018: addi sp, sp, 0x10
  0x101c: ret
  0x1020: mv t6, a0
  0x1024: addi sp, sp, -0x10
  0x1028: sd a1, 0(sp)
  0x102c: sd ra, 8(sp)
  0x1030: fld fa0, 0(t6)
  0x1034: jal -0x34
  0x1038: ld t6, 0(sp)
  0x103c: ld ra, 8(sp)
  0x1040: addi sp, sp, 0x10
  0x1044: fsd fa0, 0(t6)
  0x1048: ret
  0x1050: sin

#)

#( expected results
  0 -> 0
  1.57079632679 -> 1
#)

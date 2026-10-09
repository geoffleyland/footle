argument a
return a + 1

#( expected statements

  argument a
  return (a + 1)

#)

#( expected vir

  argument I0
  local I1 = 1
  local I2 = I0 + I1
  return I2

#)

#( expected schedule aarch64

  I0: argument
  I1: ldr K0
  I2: fadd I0 I1
  I3: ret I2
  K0: 1.0

#)

#( expected schedule riscv64

  I0: argument
  I1: auipc K0
  I2: fld I1 K0
  I3: fadd.d I0 I2
  I4: ret I3
  K0: 1.0

#)

#( expected assembly aarch64 f64

  0x1000: ldr d16, #0x1028
  0x1004: fadd d0, d0, d16
  0x1008: ret
  0x100c: mov x16, x0
  0x1010: stp x1, x30, [sp, #-0x10]!
  0x1014: ldr d0, [x16]
  0x1018: bl #0x1000
  0x101c: ldp x16, x30, [sp], #0x10
  0x1020: str d0, [x16]
  0x1024: ret
  0x1028: 1.0

#)

#( expected assembly riscv64 f64

  0x1000: auipc t0, 0
  0x1004: fld ft0, 0x40(t0)
  0x1008: fadd.d fa0, fa0, ft0
  0x100c: ret
  0x1010: mv t6, a0
  0x1014: addi sp, sp, -0x10
  0x1018: sd a1, 0(sp)
  0x101c: sd ra, 8(sp)
  0x1020: fld fa0, 0(t6)
  0x1024: jal -0x24
  0x1028: ld t6, 0(sp)
  0x102c: ld ra, 8(sp)
  0x1030: addi sp, sp, 0x10
  0x1034: fsd fa0, 0(t6)
  0x1038: ret
  0x1040: 1.0

#)

#( expected results

  1 -> 2
  2 -> 3
  3 -> 4

#)

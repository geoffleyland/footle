local a = 1
return a

#( expected statements

    local a = 1
    return a

#)

#( expected vir

    local I0 = 1
    return I0

#)

#( expected schedule aarch64

  I0: ldr K0
  I1: ret I0
  K0: 1.0

#)

#( expected schedule riscv64

  I0: auipc K0
  I1: fld I0 K0
  I2: ret I1
  K0: 1.0

#)

#( expected assembly aarch64

  0x1000: ldr d0, #0x1020
  0x1004: ret
  0x1008: mov x16, x0
  0x100c: stp x1, x30, [sp, #-0x10]!
  0x1010: bl #0x1000
  0x1014: ldp x16, x30, [sp], #0x10
  0x1018: str d0, [x16]
  0x101c: ret
  0x1020: 1.0

#)

#( expected assembly riscv64

  0x1000: auipc t0, 0
  0x1004: fld fa0, 0x38(t0)
  0x1008: ret
  0x100c: mv t6, a0
  0x1010: addi sp, sp, -0x10
  0x1014: sd a1, 0(sp)
  0x1018: sd ra, 8(sp)
  0x101c: jal -0x1c
  0x1020: ld t6, 0(sp)
  0x1024: ld ra, 8(sp)
  0x1028: addi sp, sp, 0x10
  0x102c: fsd fa0, 0(t6)
  0x1030: ret
  0x1038: 1.0

#)

#( expected results

    -> 1

#)

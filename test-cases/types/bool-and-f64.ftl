return false, 1.3

#( expected schedule aarch64

  I1: ldr K0
  I0: mov #0
  I2: ret I0 I1
  K0: 1.3

#)

#( expected schedule riscv64

  I1: auipc K0
  I0: li #0
  I2: fld I1 K0
  I3: ret I0 I2
  K0: 1.3

#)

#( expected assembly aarch64

  0x1000: ldr d0, #0x1028
  0x1004: mov x0, #0
  0x1008: ret
  0x100c: mov x16, x0
  0x1010: stp x1, x30, [sp, #-0x10]!
  0x1014: bl #0x1000
  0x1018: ldp x16, x30, [sp], #0x10
  0x101c: str x0, [x16]
  0x1020: str d0, [x16, #8]
  0x1024: ret
  0x1028: 1.3

#)

#( expected assembly riscv64

  0x1000: auipc t0, 0
  0x1004: mv a0, zero
  0x1008: fld fa0, 0x40(t0)
  0x100c: ret
  0x1010: mv t6, a0
  0x1014: addi sp, sp, -0x10
  0x1018: sd a1, 0(sp)
  0x101c: sd ra, 8(sp)
  0x1020: jal -0x20
  0x1024: ld t6, 0(sp)
  0x1028: ld ra, 8(sp)
  0x102c: addi sp, sp, 0x10
  0x1030: sd a0, 0(t6)
  0x1034: fsd fa0, 8(t6)
  0x1038: ret
  0x1040: 1.3

#)

#( expected results
    -> false 1.3
#)

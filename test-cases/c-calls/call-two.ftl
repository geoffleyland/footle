argument a
return sin(a) ^ a

#( expected schedule aarch64

  I0: argument
  I1: ldr sin
  I3: ldr pow
  I2: blr I1 I0
  I4: blr I3 I2 I0
  I5: ret I4

#)

#( expected schedule riscv64

  I0: argument
  I1: auipc sin
  I4: auipc pow
  I2: ld I1 sin
  I5: ld I4 pow
  I3: jalr I2 I0
  I6: jalr I5 I3 I0
  I7: ret I6

#)

#( expected assembly aarch64 f64

  0x1000: stp x19, x30, [sp, #-0x10]!
  0x1004: str d8, [sp, #-0x10]!
  0x1008: ldr x9, #0x1048
  0x100c: ldr x19, #0x1050
  0x1010: fmov d8, d0
  0x1014: blr x9
  0x1018: fmov d1, d8
  0x101c: blr x19
  0x1020: ldr d8, [sp], #0x10
  0x1024: ldp x19, x30, [sp], #0x10
  0x1028: ret
  0x102c: mov x16, x0
  0x1030: stp x1, x30, [sp, #-0x10]!
  0x1034: ldr d0, [x16]
  0x1038: bl #0x1000
  0x103c: ldp x16, x30, [sp], #0x10
  0x1040: str d0, [x16]
  0x1044: ret
  0x1048: sin
  0x1050: pow

#)

#( expected assembly riscv64 f64

  0x1000: addi sp, sp, -0x20
  0x1004: sd ra, 0(sp)
  0x1008: sd s1, 8(sp)
  0x100c: fsd fs0, 0x10(sp)
  0x1010: auipc t0, 0
  0x1014: auipc t1, 0
  0x1018: ld t0, 0x60(t0)
  0x101c: ld s1, 0x64(t1)
  0x1020: fmv.d fs0, fa0
  0x1024: jalr t0
  0x1028: fmv.d fa1, fs0
  0x102c: jalr s1
  0x1030: ld ra, 0(sp)
  0x1034: ld s1, 8(sp)
  0x1038: fld fs0, 0x10(sp)
  0x103c: addi sp, sp, 0x20
  0x1040: ret
  0x1044: mv t6, a0
  0x1048: addi sp, sp, -0x10
  0x104c: sd a1, 0(sp)
  0x1050: sd ra, 8(sp)
  0x1054: fld fa0, 0(t6)
  0x1058: jal -0x58
  0x105c: ld t6, 0(sp)
  0x1060: ld ra, 8(sp)
  0x1064: addi sp, sp, 0x10
  0x1068: fsd fa0, 0(t6)
  0x106c: ret
  0x1070: sin
  0x1078: pow

#)

#( expected results

    0 -> 1
    0.5 -> 0.6924056171090779

#)

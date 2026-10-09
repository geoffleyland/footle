argument b
return 1, 2, 3, 4, 5, 6, 7, 8, b

#( expected schedule aarch64

  I0: argument
  I8: ldr K7
  I7: ldr K6
  I6: ldr K5
  I5: ldr K4
  I4: ldr K3
  I3: ldr K2
  I2: ldr K1
  I1: ldr K0
  I9: ret I1 I2 I3 I4 I5 I6 I7 I8 I0
  K0: 1.0
  K1: 2.0
  K2: 3.0
  K3: 4.0
  K4: 5.0
  K5: 6.0
  K6: 7.0
  K7: 8.0

#)

#( expected schedule riscv64

  I0: argument
  I15: auipc K7
  I13: auipc K6
  I11: auipc K5
  I12: fld I11 K5
  I14: fld I13 K6
  I16: fld I15 K7
  I9: auipc K4
  I7: auipc K3
  I5: auipc K2
  I6: fld I5 K2
  I8: fld I7 K3
  I10: fld I9 K4
  I3: auipc K1
  I1: auipc K0
  I2: fld I1 K0
  I4: fld I3 K1
  I17: ret I2 I4 I6 I8 I10 I12 I14 I16 I0
  K0: 1.0
  K1: 2.0
  K2: 3.0
  K3: 4.0
  K4: 5.0
  K5: 6.0
  K6: 7.0
  K7: 8.0

#)

#( expected assembly aarch64 bool

  0x1000: ldr d7, #0x1098
  0x1004: ldr d6, #0x1090
  0x1008: ldr d5, #0x1088
  0x100c: ldr d4, #0x1080
  0x1010: ldr d3, #0x1078
  0x1014: ldr d2, #0x1070
  0x1018: ldr d1, #0x1068
  0x101c: ldr d0, #0x1060
  0x1020: ret
  0x1024: mov x16, x0
  0x1028: stp x1, x30, [sp, #-0x10]!
  0x102c: ldr x0, [x16]
  0x1030: bl #0x1000
  0x1034: ldp x16, x30, [sp], #0x10
  0x1038: str d0, [x16]
  0x103c: str d1, [x16, #8]
  0x1040: str d2, [x16, #0x10]
  0x1044: str d3, [x16, #0x18]
  0x1048: str d4, [x16, #0x20]
  0x104c: str d5, [x16, #0x28]
  0x1050: str d6, [x16, #0x30]
  0x1054: str d7, [x16, #0x38]
  0x1058: str x0, [x16, #0x40]
  0x105c: ret
  0x1060: 1.0
  0x1068: 2.0
  0x1070: 3.0
  0x1078: 4.0
  0x1080: 5.0
  0x1088: 6.0
  0x1090: 7.0
  0x1098: 8.0

#)

#( expected assembly riscv64 bool

  0x1000: auipc t0, 0
  0x1004: auipc t1, 0
  0x1008: auipc t2, 0
  0x100c: fld fa5, 0xb0(t2)
  0x1010: fld fa6, 0xbc(t1)
  0x1014: fld fa7, 0xc8(t0)
  0x1018: auipc t0, 0
  0x101c: auipc t1, 0
  0x1020: auipc t2, 0
  0x1024: fld fa2, 0x80(t2)
  0x1028: fld fa3, 0x8c(t1)
  0x102c: fld fa4, 0x98(t0)
  0x1030: auipc t0, 0
  0x1034: auipc t1, 0
  0x1038: fld fa0, 0x5c(t1)
  0x103c: fld fa1, 0x68(t0)
  0x1040: ret
  0x1044: mv t6, a0
  0x1048: addi sp, sp, -0x10
  0x104c: sd a1, 0(sp)
  0x1050: sd ra, 8(sp)
  0x1054: ld a0, 0(t6)
  0x1058: jal -0x58
  0x105c: ld t6, 0(sp)
  0x1060: ld ra, 8(sp)
  0x1064: addi sp, sp, 0x10
  0x1068: fsd fa0, 0(t6)
  0x106c: fsd fa1, 8(t6)
  0x1070: fsd fa2, 0x10(t6)
  0x1074: fsd fa3, 0x18(t6)
  0x1078: fsd fa4, 0x20(t6)
  0x107c: fsd fa5, 0x28(t6)
  0x1080: fsd fa6, 0x30(t6)
  0x1084: fsd fa7, 0x38(t6)
  0x1088: sd a0, 0x40(t6)
  0x108c: ret
  0x1090: 1.0
  0x1098: 2.0
  0x10a0: 3.0
  0x10a8: 4.0
  0x10b0: 5.0
  0x10b8: 6.0
  0x10c0: 7.0
  0x10c8: 8.0

#)

#( expected results
    true -> 1 2 3 4 5 6 7 8 true
#)

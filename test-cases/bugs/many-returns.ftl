argument b
return 1, 2, 3, 4, 5, 6, 7, 8, b

#( expected schedule

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

#( expected assembly bool

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

#( expected results
    true -> 1 2 3 4 5 6 7 8 true
#)

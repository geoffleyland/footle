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

#( expected results

    0 -> 1
    0.5 -> 0.6924056171090779

#)

argument a, b
return a, b

#( expected statements

    argument a, b
    return a, b

#)

#( expected vir

    argument I0
    argument I1
    return I0, I1

#)

#( expected schedule

  I0: argument
  I1: argument
  I2: ret I0 I1

#)

#( expected assembly aarch64 f64 f64

  0x1000: ret
  0x1004: mov x16, x0
  0x1008: stp x1, x30, [sp, #-0x10]!
  0x100c: ldr d0, [x16]
  0x1010: ldr d1, [x16, #8]
  0x1014: bl #0x1000
  0x1018: ldp x16, x30, [sp], #0x10
  0x101c: str d0, [x16]
  0x1020: str d1, [x16, #8]
  0x1024: ret

#)

#( expected assembly aarch64 bool f64

  0x1000: ret
  0x1004: mov x16, x0
  0x1008: stp x1, x30, [sp, #-0x10]!
  0x100c: ldr x0, [x16]
  0x1010: ldr d0, [x16, #8]
  0x1014: bl #0x1000
  0x1018: ldp x16, x30, [sp], #0x10
  0x101c: str x0, [x16]
  0x1020: str d0, [x16, #8]
  0x1024: ret

#)

#( expected assembly aarch64 f64 bool

  0x1000: ret
  0x1004: mov x16, x0
  0x1008: stp x1, x30, [sp, #-0x10]!
  0x100c: ldr d0, [x16]
  0x1010: ldr x0, [x16, #8]
  0x1014: bl #0x1000
  0x1018: ldp x16, x30, [sp], #0x10
  0x101c: str d0, [x16]
  0x1020: str x0, [x16, #8]
  0x1024: ret

#)

#( expected assembly aarch64 bool bool

  0x1000: ret
  0x1004: mov x16, x0
  0x1008: stp x1, x30, [sp, #-0x10]!
  0x100c: ldr x0, [x16]
  0x1010: ldr x1, [x16, #8]
  0x1014: bl #0x1000
  0x1018: ldp x16, x30, [sp], #0x10
  0x101c: str x0, [x16]
  0x1020: str x1, [x16, #8]
  0x1024: ret

#)

#( expected assembly riscv64 f64 f64

  0x1000: ret
  0x1004: mv t6, a0
  0x1008: addi sp, sp, -0x10
  0x100c: sd a1, 0(sp)
  0x1010: sd ra, 8(sp)
  0x1014: fld fa0, 0(t6)
  0x1018: fld fa1, 8(t6)
  0x101c: jal -0x1c
  0x1020: ld t6, 0(sp)
  0x1024: ld ra, 8(sp)
  0x1028: addi sp, sp, 0x10
  0x102c: fsd fa0, 0(t6)
  0x1030: fsd fa1, 8(t6)
  0x1034: ret

#)

#( expected assembly riscv64 bool f64

  0x1000: ret
  0x1004: mv t6, a0
  0x1008: addi sp, sp, -0x10
  0x100c: sd a1, 0(sp)
  0x1010: sd ra, 8(sp)
  0x1014: ld a0, 0(t6)
  0x1018: fld fa0, 8(t6)
  0x101c: jal -0x1c
  0x1020: ld t6, 0(sp)
  0x1024: ld ra, 8(sp)
  0x1028: addi sp, sp, 0x10
  0x102c: sd a0, 0(t6)
  0x1030: fsd fa0, 8(t6)
  0x1034: ret

#)

#( expected assembly riscv64 f64 bool

  0x1000: ret
  0x1004: mv t6, a0
  0x1008: addi sp, sp, -0x10
  0x100c: sd a1, 0(sp)
  0x1010: sd ra, 8(sp)
  0x1014: fld fa0, 0(t6)
  0x1018: ld a0, 8(t6)
  0x101c: jal -0x1c
  0x1020: ld t6, 0(sp)
  0x1024: ld ra, 8(sp)
  0x1028: addi sp, sp, 0x10
  0x102c: fsd fa0, 0(t6)
  0x1030: sd a0, 8(t6)
  0x1034: ret

#)

#( expected assembly riscv64 bool bool

  0x1000: ret
  0x1004: mv t6, a0
  0x1008: addi sp, sp, -0x10
  0x100c: sd a1, 0(sp)
  0x1010: sd ra, 8(sp)
  0x1014: ld a0, 0(t6)
  0x1018: ld a1, 8(t6)
  0x101c: jal -0x1c
  0x1020: ld t6, 0(sp)
  0x1024: ld ra, 8(sp)
  0x1028: addi sp, sp, 0x10
  0x102c: sd a0, 0(t6)
  0x1030: sd a1, 8(t6)
  0x1034: ret

#)

#( expected results

  1 2 -> 1 2
  2 3 -> 2 3
  2 1 -> 2 1
  true 1 -> true 1
  1 true -> 1 true
  true false -> true false

#)

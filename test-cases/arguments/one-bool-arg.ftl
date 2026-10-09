argument a
return a

#( expected schedule

  I0: argument
  I1: ret I0

#)

#( expected assembly aarch64 bool

  0x1000: ret
  0x1004: mov x16, x0
  0x1008: stp x1, x30, [sp, #-0x10]!
  0x100c: ldr x0, [x16]
  0x1010: bl #0x1000
  0x1014: ldp x16, x30, [sp], #0x10
  0x1018: str x0, [x16]
  0x101c: ret

#)

#( expected assembly riscv64 bool

  0x1000: ret
  0x1004: mv t6, a0
  0x1008: addi sp, sp, -0x10
  0x100c: sd a1, 0(sp)
  0x1010: sd ra, 8(sp)
  0x1014: ld a0, 0(t6)
  0x1018: jal -0x18
  0x101c: ld t6, 0(sp)
  0x1020: ld ra, 8(sp)
  0x1024: addi sp, sp, 0x10
  0x1028: sd a0, 0(t6)
  0x102c: ret

#)

#( expected results

    true -> true
    false -> false

#)

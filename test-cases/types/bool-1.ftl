return true

#( expected statements

  return true

#)

#( expected vir

  local I0 = true
  return I0

#)

#( expected schedule aarch64

  I0: mov #1
  I1: ret I0

#)

#( expected schedule riscv64

  I0: li #1
  I1: ret I0

#)

#( expected assembly aarch64

  0x1000: mov x0, #1
  0x1004: ret
  0x1008: mov x16, x0
  0x100c: stp x1, x30, [sp, #-0x10]!
  0x1010: bl #0x1000
  0x1014: ldp x16, x30, [sp], #0x10
  0x1018: str x0, [x16]
  0x101c: ret

#)

#( expected assembly riscv64

  0x1000: addi a0, zero, 1
  0x1004: ret
  0x1008: mv t6, a0
  0x100c: addi sp, sp, -0x10
  0x1010: sd a1, 0(sp)
  0x1014: sd ra, 8(sp)
  0x1018: jal -0x18
  0x101c: ld t6, 0(sp)
  0x1020: ld ra, 8(sp)
  0x1024: addi sp, sp, 0x10
  0x1028: sd a0, 0(t6)
  0x102c: ret

#)

#( expected results
    -> true
#)

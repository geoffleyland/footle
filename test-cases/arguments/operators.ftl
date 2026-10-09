argument a, b
local c = a + b
local d = a - b
local e = a * b
local f = a / b
return c, d, e, f

#( expected statements

  argument a, b
  local c = (a + b)
  local d = (a - b)
  local e = (a * b)
  local f = (a / b)
  return c, d, e, f

#)

#( expected vir

  argument I0
  argument I1
  local I2 = I0 + I1
  local I3 = I0 - I1
  local I4 = I0 * I1
  local I5 = I0 / I1
  return I2, I3, I4, I5

#)

#( expected schedule aarch64

  I0: argument
  I1: argument
  I5: fdiv I0 I1
  I4: fmul I0 I1
  I3: fsub I0 I1
  I2: fadd I0 I1
  I6: ret I2 I3 I4 I5

#)

#( expected schedule riscv64

  I0: argument
  I1: argument
  I5: fdiv.d I0 I1
  I4: fmul.d I0 I1
  I3: fsub.d I0 I1
  I2: fadd.d I0 I1
  I6: ret I2 I3 I4 I5

#)

#( expected assembly aarch64 f64 f64

  0x1000: fdiv d3, d0, d1
  0x1004: fmul d2, d0, d1
  0x1008: fsub d16, d0, d1
  0x100c: fadd d0, d0, d1
  0x1010: fmov d1, d16
  0x1014: ret
  0x1018: mov x16, x0
  0x101c: stp x1, x30, [sp, #-0x10]!
  0x1020: ldr d0, [x16]
  0x1024: ldr d1, [x16, #8]
  0x1028: bl #0x1000
  0x102c: ldp x16, x30, [sp], #0x10
  0x1030: str d0, [x16]
  0x1034: str d1, [x16, #8]
  0x1038: str d2, [x16, #0x10]
  0x103c: str d3, [x16, #0x18]
  0x1040: ret

#)

#( expected assembly riscv64 f64 f64

  0x1000: fdiv.d fa3, fa0, fa1
  0x1004: fmul.d fa2, fa0, fa1
  0x1008: fsub.d ft0, fa0, fa1
  0x100c: fadd.d fa0, fa0, fa1
  0x1010: fmv.d fa1, ft0
  0x1014: ret
  0x1018: mv t6, a0
  0x101c: addi sp, sp, -0x10
  0x1020: sd a1, 0(sp)
  0x1024: sd ra, 8(sp)
  0x1028: fld fa0, 0(t6)
  0x102c: fld fa1, 8(t6)
  0x1030: jal -0x30
  0x1034: ld t6, 0(sp)
  0x1038: ld ra, 8(sp)
  0x103c: addi sp, sp, 0x10
  0x1040: fsd fa0, 0(t6)
  0x1044: fsd fa1, 8(t6)
  0x1048: fsd fa2, 0x10(t6)
  0x104c: fsd fa3, 0x18(t6)
  0x1050: ret

#)

#( expected results

  1 2 -> 3 -1 2 0.5
  2 1 -> 3 1 2 2

#)

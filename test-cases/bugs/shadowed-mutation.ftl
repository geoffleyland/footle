mutable local x = 1
local y = begin
  local x = begin
    x = 5
    2
  end
  x
end
mutable local z = 1
z = begin
  z = 5
  z + 1
end
return x, y, z

#( expected statements

  mutable local x = 1
  local y = begin
    local x = begin
      x = 5
      2
    end
    x
  end
  mutable local z = 1
  z = begin
    z = 5
    (z + 1)
  end
  return x, y, z

#)

#( expected vir

  local I0 = 5
  local I1 = 2
  local I2 = 6
  return I0, I1, I2

#)

#( expected schedule aarch64

  I2: ldr K2
  I1: ldr K1
  I0: ldr K0
  I3: ret I0 I1 I2
  K0: 5.0
  K1: 2.0
  K2: 6.0

#)

#( expected schedule riscv64

  I4: auipc K2
  I2: auipc K1
  I0: auipc K0
  I1: fld I0 K0
  I3: fld I2 K1
  I5: fld I4 K2
  I6: ret I1 I3 I5
  K0: 5.0
  K1: 2.0
  K2: 6.0

#)

#( expected assembly aarch64

  0x1000: ldr d2, #0x1040
  0x1004: ldr d1, #0x1038
  0x1008: ldr d0, #0x1030
  0x100c: ret
  0x1010: mov x16, x0
  0x1014: stp x1, x30, [sp, #-0x10]!
  0x1018: bl #0x1000
  0x101c: ldp x16, x30, [sp], #0x10
  0x1020: str d0, [x16]
  0x1024: str d1, [x16, #8]
  0x1028: str d2, [x16, #0x10]
  0x102c: ret
  0x1030: 5.0
  0x1038: 2.0
  0x1040: 6.0

#)

#( expected assembly riscv64

  0x1000: auipc t0, 0
  0x1004: auipc t1, 0
  0x1008: auipc t2, 0
  0x100c: fld fa0, 0x48(t2)
  0x1010: fld fa1, 0x54(t1)
  0x1014: fld fa2, 0x60(t0)
  0x1018: ret
  0x101c: mv t6, a0
  0x1020: addi sp, sp, -0x10
  0x1024: sd a1, 0(sp)
  0x1028: sd ra, 8(sp)
  0x102c: jal -0x2c
  0x1030: ld t6, 0(sp)
  0x1034: ld ra, 8(sp)
  0x1038: addi sp, sp, 0x10
  0x103c: fsd fa0, 0(t6)
  0x1040: fsd fa1, 8(t6)
  0x1044: fsd fa2, 0x10(t6)
  0x1048: ret
  0x1050: 5.0
  0x1058: 2.0
  0x1060: 6.0

#)

#( expected results
    -> 5 2 6
#)

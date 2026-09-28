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

#( expected schedule

  I2: ldr K2
  I1: ldr K1
  I0: ldr K0
  I3: ret I0 I1 I2
  K0: 5.0
  K1: 2.0
  K2: 6.0

#)

#( expected assembly

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

#( expected results
    -> 5 2 6
#)

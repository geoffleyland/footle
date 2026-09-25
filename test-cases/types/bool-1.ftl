return true

#( expected statements

  return true

#)

#( expected vir

  local I0 = true
  return I0

#)

#( expected schedule

  I0: mov #1
  I1: ret I0

#)

#( expected assembly

  0x1000: mov x0, #1
  0x1004: ret
  0x1008: mov x16, x0
  0x100c: stp x1, x30, [sp, #-0x10]!
  0x1010: bl #0x1000
  0x1014: ldp x16, x30, [sp], #0x10
  0x1018: str x0, [x16]
  0x101c: ret

#)

#( expected results
    -> true
#)

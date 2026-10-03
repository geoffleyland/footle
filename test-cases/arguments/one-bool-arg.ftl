argument a
return a

#( expected schedule aarch64

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

#( expected results

    true -> true
    false -> false

#)

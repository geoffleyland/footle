argument a
local b = not_sin(a)
local c = sin(a, b)
return b, c

#( expected vir-errors

  cannot find function 'not_sin' in this scope (21, 31)
  function 'sin' called with 2 arguments, expected 1 (42, 51)
  cannot find value 'b' in this scope (59, 60)

#)

mutable local a = 1
local b = 1 == 2
a = b
return a

#( expected vir-errors

  Reassignment of `a` from `float` to `bool` (37, 38) `a` was `float` here: (18, 19) The rhs is `bool` here: (30, 36)

#)

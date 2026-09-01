argument x, y
mutable local a = 1
local b = x == y
a = b
return a

#( expected vir-errors

  Reassignment of `a` from `float` to `bool` (51, 52) `a` was `float` here: (32, 33) The rhs is `bool` here: (44, 50)

#)

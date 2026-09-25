argument x, y
mutable local a = 1
local b = x == y
a = b
return a

#( expected errors

  Reassignment of `a` from `f64` to `bool` (51, 52) `a` was `f64` here: (32, 33) The rhs is `bool` here: (44, 50)

#)

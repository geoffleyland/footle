mutable local a = 1
local b = 1 == 2
a = b
return a

#( expected errors

  Reassignment of `a` from `f64` to `bool` (37, 38) `a` was `f64` here: (18, 19) The rhs is `bool` here: (30, 36)

#)

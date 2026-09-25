local a = 1
local b = true
return a == b

#( expected errors

  Expected `f64`, got `bool` (34, 40) `f64` was set here: (10, 11) `bool` was set here: (22, 26)

#)

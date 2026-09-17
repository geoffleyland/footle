local a = 1
local b = true
return a == b

#( expected vir-errors

  Expected `float`, got `bool` (34, 40) `float` was set here: (10, 11) `bool` was set here: (22, 26)

#)

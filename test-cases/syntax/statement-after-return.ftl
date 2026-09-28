begin
    return 1
    local a = 2
end
local b = 3
return b
local c = 4

#( expected errors

  `return` must be the last statement in a block (23, 28) the `return` is here: (10, 18)
  `return` must be the last statement in a block (60, 65) the `return` is here: (51, 59)

#)

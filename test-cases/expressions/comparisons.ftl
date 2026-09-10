local a, b = 1, 2
local c = a < b
local d = a > b
local e = a <= b
local f = a >= b
local g = a == b
local h = a != b
return c, d, e, f, g, h

#( expected statements

    local a, b = 1, 2
    local c = (a < b)
    local d = (a > b)
    local e = (a <= b)
    local f = (a >= b)
    local g = (a == b)
    local h = (a != b)
    return c, d, e, f, g, h

#)

#( expected vir

  local I0 = 1
  local I1 = 0
  return I0, I1, I0, I1, I1, I0

#)

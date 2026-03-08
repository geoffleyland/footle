local a, b, c = 1, 2, 3
local d = a + b * c
local e = a * b + c
local f = (a + b) * c
return d, e, f

#( expected statements

    local a, b, c = 1, 2, 3
    local d = (a + (b * c))
    local e = ((a * b) + c)
    local f = ((a + b) * c)
    return d, e, f

#)

#( expected vir

  local I0 = 7
  local I1 = 5
  local I2 = 9
  return I0, I1, I2

#)

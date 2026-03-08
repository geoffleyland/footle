argument a
argument b
local c = a + b
local d = a + b
return c, d

#( expected statements

  argument a
  argument b
  local c = (a + b)
  local d = (a + b)
  return c, d

#)

#( expected vir

  argument I0
  argument I1
  local I2 = I0 + I1
  return I2, I2

#)

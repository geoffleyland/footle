argument a
argument b
return a > b

#( expected statements

  argument a
  argument b
  return (a > b)

#)

#( expected vir

  argument I0
  argument I1
  local I2 = I1 < I0
  return I2

#)

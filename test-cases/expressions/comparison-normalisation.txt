argument a
argument b
return a > b

#( expected statements

  argument a
  argument b
  return (a > b)

#)

# Note that the comparison operator gets switch to canonicalise it.
#( expected vir

  argument I0
  argument I1
  local I2 = I1 < I0
  return I2

#)

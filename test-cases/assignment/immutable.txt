local a = 1
a = 2
return a

#( expected statements

  local a = 1
  a = 2
  return a

#)

#( expected vir-errors

  cannot assign twice to the immutable variable 'a' (12, 13) the declaration of 'a' is here: (6, 7)

#)

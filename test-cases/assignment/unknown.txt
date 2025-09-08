a = 1
return a

#( expected statements

  a = 1
  return a

#)

#( expected vir-errors

  cannot find value 'a' in this scope (0, 1)
  cannot find value 'a' in this scope (13, 14)

#)

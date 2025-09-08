begin
    local b = 1
end
return b

#( expected statements

  begin
    local b = 1
  end
  return b

#)

#( expected vir-errors

  cannot find value 'b' in this scope (33, 34)

#)

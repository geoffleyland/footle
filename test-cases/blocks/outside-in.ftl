local a = 1
local c = begin
    local b = a
    b
end
return c

#( expected statements

  local a = 1
  local c = begin
    local b = a
    b
  end
  return c

#)

#( expected vir

  local I0 = 1
  return I0

#)

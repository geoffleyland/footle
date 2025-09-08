local a = begin
    local b = 1
    b
end

return a


#( expected statements

    local a = begin
      local b = 1
      b
    end
    return a

#)

#( expected vir

    local I0 = 1
    return I0

#)

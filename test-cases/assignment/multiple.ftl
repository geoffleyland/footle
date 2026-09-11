local a = 1
local b = 2
return a + b

#( expected statements

    local a = 1
    local b = 2
    return (a + b)

#)

#( expected vir

    local I0 = 3
    return I0

#)

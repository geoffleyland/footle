local a, b = 1, 2
return a + b

#( expected statements

    local a, b = 1, 2
    return (a + b)

#)

#( expected vir

    local I0 = 3
    return I0

#)

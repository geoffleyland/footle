mutable local a = 1
a = 2
return a

#( expected statements

    mutable local a = 1
    a = 2
    return a

#)

#( expected vir

    local I0 = 2
    return I0

#)

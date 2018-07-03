Module Kinds
    
    Implicit None
    Public
    
#   if EXT_PREC
        Integer, Parameter :: sp = Selected_Real_Kind(p=33, r=4931)  !quadruple precision
        Integer, Parameter :: dp = Selected_Real_Kind(p=33, r=4931)  !quadruple precision
#   else
        Integer, Parameter :: sp = Selected_Real_Kind(p=6, r=37)   !single precision
        Integer, Parameter :: dp = Selected_Real_Kind(p=15, r=307)  !double precision
#   endif
    Integer, Parameter :: qp = Selected_Real_Kind(p=33, r=4931)  !quadruple precision

    Integer, Parameter :: is = Selected_Int_Kind(3)  !short word, +/- 10**4
    Integer, Parameter :: il = Selected_Int_Kind(8)  !long word, +/- 10**8
    Integer, Parameter :: id = Selected_Int_Kind(14)  !double word, +/- 10**18
    
End Module Kinds

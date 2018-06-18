Module Kinds
    
    Implicit None
    Public
    
    Integer, Parameter :: sp = Selected_Real_Kind(p=6, r=37)   !single precision
    Integer, Parameter :: dp = Selected_Real_Kind(p=15, r=307)  !double precision
    Integer, Parameter :: qp = Selected_Real_Kind(p=33, r=4931)  !quadruple precision

    Integer, Parameter :: i2 = Selected_Int_Kind(4)  !+/- 10**4
    Integer, Parameter :: i4 = Selected_Int_Kind(8)  !+/- 10**8
    Integer, Parameter :: i8 = Selected_Int_Kind(18)  !+/- 10**18
    
End Module Kinds

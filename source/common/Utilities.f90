Module Utilities

    Implicit None
    Public
!In general, the contents of this module are public, but the
!followng procedures are support routines or are accessible via 
!generic interfaces and need not be explicitly public...
    Private :: Qroot_small  !support routine for SMALLER_QUADRATIC_ROOT
    Private :: Qroot_large  !support routine for LARGER_QUADRATIC_ROOT
    Private :: Converged_defaultTolerances  !Public via CONVERGED
    Private :: Converged_userTolerances     !Public via CONVERGED
    Private :: Prec_dp  !Public via PREC
    Private :: Prec_sp  !Public via PREC
    
    Interface Converged
        Module Procedure Converged_defaultTolerances
        Module Procedure Converged_userTolerances
    End Interface Converged
    
    Interface Prec
        Module Procedure Prec_dp
        Module Procedure Prec_sp
    End Interface Prec
    
Contains

Pure Function Prec_dp(x,y) Result(p)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: p  !precision of y, relative to true value x
    Real(dp), Intent(In) :: x  !true value
    Real(dp), Intent(in) :: y  !approximation
    
    If (x .NE. y) Then
        p = Max(0._dp,-Log10(2._dp * Abs((x-y)/x)))
    Else
        p = -Log10(2._dp*Spacing(1._dp))
    End If
End Function Prec_dp

Pure Function Prec_sp(x,y) Result(p)
    Use Kinds, Only: sp
    Implicit None
    Real(sp) :: p  !precision of y, relative to true value x
    Real(sp), Intent(In) :: x  !true value
    Real(sp), Intent(in) :: y  !approximation
    
    If (x .NE. y) Then
        p = Max(0._sp,-Log10(2._sp * Abs((x-y)/x)))
    Else
        p = -Log10(2._sp*Spacing(1._sp))
    End If
End Function Prec_sp

Pure Function Octogon_Area(P) Result(A)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: A
    Real(dp), Intent(In):: P(1:3,1:8)
    Real(dp) :: Q(1:3,1:4)
    Integer :: i
    
    A = 0._dp
    Q(:,1) = P(:,1)
    Do i = 2,6,2
        Q(:,2:4) = P(:,i:i+2)
        A = A + Quadrilateral_Area(Q)
    End Do
End Function Octogon_Area

Pure Function Quadrilateral_Area(P) Result(A)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: A
    Real(dp), Intent(In):: P(1:3,1:4)   
    
    A = 0.5_dp * Vector_Length(Normal_Vector_4(P))
End Function Quadrilateral_Area

Pure Function Triangle_Area(P) Result(A)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: A
    Real(dp), Intent(In):: P(1:3,1:3)   
    
    A = 0.5_dp * Vector_Length(Normal_Vector_3(P))
End Function Triangle_Area

Pure Function Normal_Vector_4(P,hat) Result(N)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: N(1:3)
    Real(dp), Intent(In):: P(1:3,1:4)
    Logical, Intent(In), Optional :: hat
    Real(dp):: b(1:3),c(1:3)
    
    b = p(:,1) - p(:,3)
    c = p(:,2) - p(:,4)
    N = Cross_Product(b,c)
    If (Present(hat)) Then
        If (hat) N = Unit_Vector(N)
    End If
End Function Normal_Vector_4

Pure Function Normal_Vector_3(P,hat) Result(N)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: N(1:3)
    Real(dp), Intent(In):: P(1:3,1:3)
    Logical, Intent(In), Optional :: hat
    Real(dp):: b(1:3),c(1:3)
    
    b = p(:,1) - p(:,3)
    c = p(:,2) - p(:,3)
    N = Cross_Product(b,c)
    If (Present(hat)) Then
        If (hat) N = Unit_Vector(N)
    End If
End Function Normal_Vector_3

Function Smaller_Quadratic_root(b,c) Result(u)
    !Returns the smaller (in absolute magnitude) root of the quadratic u^2 + 2*b*u = c
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: u
    Real(dp), Intent(In) :: b,c
    
    If (c .GT. 0._dp) Then
        u = Qroot_small(b,c)
    Else If (c .LT. 0._dp) Then
        If (Abs(c) .LE. b**2) Then
            u = Qroot_small(b,c)
        Else
            Print *,'ERROR:  Utilities: Smaller_Quadratic_root:  No real roots. b**2:',b**2,' c:',c
            ERROR STOP
        End If
    Else !c = 0
        u = 0._dp
    End If
End Function Smaller_Quadratic_root

Function Larger_Quadratic_root(b,c) Result(u)
    !Returns the larger (in absolute magnitude) root of the quadratic u^2 + 2*b*u = c
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: u
    Real(dp), Intent(In) :: b,c
    
    If (c .GT. 0._dp) Then
        u = Qroot_large(b,c)
    Else If (c .LT. 0._dp) Then
        If (Abs(c) .LE. b**2) Then
            u = Qroot_large(b,c)
        Else
            Print *,'ERROR:  Utilities: Larger_Quadratic_root:  No real roots. b**2:',b**2,' c:',c
            ERROR STOP
        End If
    Else !c = 0
        u = -2._dp * b * Sign(1._dp,b)
    End If
End Function Larger_Quadratic_root

Subroutine Quadratic_Roots(b,c,u_large,u_small)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: b,c
    Real(dp), Intent(Out) :: u_large,u_small
    
    u_large = Larger_Quadratic_Root(b,c)
    u_small = -c / u_large
End Subroutine Quadratic_Roots

Pure Function Qroot_large(b,c) Result(u)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: u
    Real(dp), Intent(In) :: b,c

    u = -b - Sign(1._dp,b) * Sqrt(b**2 + c)
End Function Qroot_large

Pure Function Qroot_small(b,c) Result(u)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: u
    Real(dp), Intent(In) :: b,c

    u = -c / Qroot_large(b,c)
End Function Qroot_small

Elemental Function Log2(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Log2
    Real(dp), Intent(In) :: x
    Real(dp), Parameter :: L2 = 1._dp / Log(2._dp)
    
    Log2 = Log(x) * L2
End Function Log2

Pure Function Factorial(n) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Integer, Intent(In) :: n
    Integer :: i
    
    If (n .EQ. 0) Then
        f = 0._dp
        Return
    End If
    f = 1._dp
    Do i = 2,n
        f = f * Real(i,dp)
    End Do
End Function Factorial

Elemental Function Cube_Root(x) Result(c)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: c
    Real(dp), Intent(In) :: x
    Real(dp), Parameter :: one_third = 1._dp / 3._dp
    
    c = Sign(Abs(x)**one_third,x)
End Function Cube_Root

Pure Function Bisection_Search(x, xList, n) Result (j)
    ! Finds index j, such that x is between xList(j) and xList(j-1)
    ! The list must be sorted in acsending order
    ! j = 1 or j = n+1 indicates that x is not in the range xList(1) to xList(n)
    Use Kinds, Only: dp
    Implicit None
    Integer :: j
    Real(dp), Intent(In) :: x
    Integer, Intent(In) :: n
    Real(dp), Intent(In) :: xList(1:n)
    Integer :: jLow,jHigh,jMid
    
    !First, check for endpoints
    If (x .EQ. xList(1)) Then
        j = 2
        Return
    Else If (x .EQ. xList(n)) Then
        j = n
        Return
    End If
    !Otherwise, search the list
    jLow = 0
    jHigh = n + 1
    Do
        If (jHigh - jLow .LE. 1) Exit
        jMid = (jHigh + jLow) / 2  !integer arithmetic
        If (x .GT. xList(jMid)) Then
            jLow = jMid
        Else
            jHigh = jMid
        End If
    End Do
    j = jHigh
End Function Bisection_Search

Pure Function Converged_defaultTolerances(x1,x2) Result(bingo) 
    Use Kinds, Only: dp
    Implicit None
    Logical :: bingo
    Real(dp), Intent(In) :: x1,x2  !values for comparison
    Real(dp), Parameter :: rTol = 1.E-15_dp  !relative tolerance
    Real(dp), Parameter :: aTol = 0._dp  !absolute tolerance
    
    bingo = (Abs(x1-x2) .LT. rTol*(Min(Abs(x1),Abs(x2))) .OR. Abs(x1-x2) .LT. aTol)
End Function Converged_defaultTolerances

Pure Function Converged_userTolerances(x1,x2,rTol,aTol) Result(bingo) 
    Use Kinds, Only: dp
    Implicit None
    Logical :: bingo
    Real(dp), Intent(In) :: x1,x2  !values for comparison
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances
    
    bingo = (Abs(x1-x2) .LT. rTol*(Min(Abs(x1),Abs(x2))) .OR. Abs(x1-x2) .LT. aTol)
End Function Converged_userTolerances

Pure Function Vector_Length(v) Result(d)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: d
    Real(dp), Intent(In) :: v(1:3)
    
    d = Sqrt(Sum(v*v))
End Function Vector_Length

Pure Function Unit_Vector(v) Result(hat)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: hat(1:3)
    Real(dp), Intent(In) :: v(1:3)
    
    hat = v / Vector_Length(v)
End Function Unit_Vector

Pure Function Cross_Product(v,w) Result(u)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: u(1:3)
    Real(dp), Intent(In) :: v(1:3),w(1:3)
    
    u = (/ v(2)*w(3) - v(3)*w(2) , &
         & v(3)*w(1) - v(1)*w(3) , &
         & v(1)*w(2) - v(2)*w(1) /)
End Function Cross_Product

Pure Function pNorm(v,p) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x
    Real(dp), Intent(In) :: v(:)
    Real(dp), Intent(In) :: p

    x = Sum(Abs(v)**p)**(1._dp/p)
End Function pNorm

End Module Utilities

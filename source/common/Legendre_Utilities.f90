Module Legendre_Utilities

    Implicit None
    Private
    Public :: Legendre_P     ! Call Subroutine Legendre_P(x, n, P)
        ! If P is a scalar, it is set to Legendre_P[n,x] (Mathematica notation)
        ! If P is a vector, P(0:n), it is set to P(j) = LegendreP[j,x] for j = 0,..,n
    Public :: Legendre_pdf   ! (x, a, n)
        ! Returns the pdf f(x) = Sum[a[j] * Legendre_P[j],{j,0,n}]
    Public :: Legendre_CDF   ! (x, a, n)
        ! Returns the value of the CDF of that pdf
    Public :: Legendre_CDF_Coefficients ! (a, n)
        ! Returns the vector b(0:n+1) of coefficients of the Legendre series for the CDF
    Public :: Legendre_Derivative_Coefficients ! (a, n)
        ! Returns the vector c(0:n-1) of coefficients of the Legendre series of the derivative
        !   f'(x)  = Sum(c(0:n-1)*P(0:n-1)) = df(x)/dx 
        !   where f(x) is the Legendre series f(x) = Sum(a(0:n)*P(0:n))
    
    ! The variables in the arguments are
        ! x is the independent (distributed) variable
        ! a(0:n) is the vector of Legendre expansion coefficients of the pdf
        ! n is the order of the expansion
        ! b(0:n+1) is the vector of Legendre expansion coefficients of the CDF,
        !   obtained from a(0:n)
        !   Note that, for efficiency, a(j) includes the factor (2j+1)/2, 
        !   so that the pdf is simply  Dot_Product(a(0:n), LegendreP(0:n,x))
    
    Interface Legendre_P
        Module Procedure LegendreP_Scalar
        Module Procedure LegendreP_Vector
        Module Procedure LegendreP_Matrix
    End Interface Legendre_P
    
Contains
    
Function Legendre_pdf(x,n,a) Result(f)
    ! Evaluates f(x) = Dot_Product(a(0:n), LegendreP(0:n,x))
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: f
    Real(dp), Intent(In):: x        ! independent variable of pdf, usually the cosine of an angle
    Integer,  Intent(In):: n        ! Order of the (truncated) series
    Real(dp), Intent(In):: a(0:n)   ! Coefficients of Legendre series for pdf
    Real(dp):: P(0:n)
    
    Call Legendre_P(x,n,P)
    f = Dot_Product(a, P)
End Function Legendre_pdf

Function Legendre_CDF(x,a,n) Result(F)
    ! Evaluates f(x) = Dot_Product(a(0:n), LegendreP(0:n,x))
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: F
    Real(dp), Intent(In):: x        ! independent variable of pdf,  usually the cosine of an angle
    Integer,  Intent(In):: n        ! Order of the (truncated) series
    Real(dp), Intent(In):: a(0:n)   ! Coefficients of Legendre series for pdf, Note that these include the factor (2j+1)/2
    Real(dp):: P(0:n+1)             ! Legendre polynomials evaluated at x
    Real(dp):: b(0:n+1)             ! Coefficients of Legendre series for CDF
    
    Call Legendre_P(x, n+1, P)
    b = Legendre_CDF_Coefficients(a, n)
    F = Dot_Product(b, P)
End Function Legendre_CDF

Function Legendre_CDF_Coefficients(a, n) Result (b)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: b(0:n+1)             ! 
    Integer, Intent(In):: n         ! Order of pdf. Order of CDF is n+1 due to integration
    Real(dp), Intent(In):: a(0:n)   ! Coefficients of Legendre series of order n of a pdf
    Real(dp):: c(0:n)               ! working vector, to avoid changing the argument a
    Integer:: j                     ! Do loop counter
    
    ! Set up coefficients of CDF
    Do j = 0,n
        c(j) = a(j) / Real(2*j+1,dp)
    End do
    b(0) = c(0) - c(1)
    b(1:n-1) = c(0:n-2) - c(2:n)
    b(n:n+1) = c(n-1:n)
End Function Legendre_CDF_Coefficients

Function Legendre_Derivative_Coefficients(a, n) Result(c)
    ! This function produces the coefficients c(0:n-1) of the derivative f'(x)
    !   given the coefficients a(0:n) of f(x)
    !   where f(x) = Dot_Product(a(0:n), LegendreP(0:n,x))
    !   and f'(x) = Dot_Product(c(0:n-1), LegendreP(0:n-1,x))
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: c(0:n-1)
    Integer,  Intent(In):: n
    Real(dp), Intent(In):: a(0:n)
    Integer:: j
    
    Do j = 0,n-1
        c(j) = Real(2*j+1,dp) * Sum(a(j+1:n:2))
    End do
    ! Note: this approach entails redundant additions
    !   but this should be justified by its elegance
End Function Legendre_Derivative_Coefficients

Subroutine LegendreP_Scalar(x, n, P)
    ! This routine computes LegendreP(n,x) recursively
    !   discarding all the Legendre polynomials of lower order than L
    ! It is very inefficient to call it repeatedly to get P(0,x) through P(n,X)
    !   Use the vector routine which does not waste work.
    ! Better, use the matrix routine,
    !   which vectorizes the arithmetic for multiple arguments x(1:N)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: x            ! Argument
    Integer, Intent(In) :: n             ! Order of Legendre Polynomial
    Real(dp), Intent(Out) :: P           ! P = LegendreP(L, x)
    Integer :: j
    Real(dp) :: P_prior
    Real(dp) :: P_current
    
    Select Case (n)
        Case(2:)
            P = 1.0_dp
            P_current = P
            P = x
            Do j = 1,n-1
                P_prior = P_current
                P_current = P
                P = (Real(2*j+1,dp)/Real(j+1,dp)) * x * P_current - (Real(j,dp)/Real(j+1,dp)) * P_prior
            End do
        Case (1)
            P = x
        Case(0)
            P = 1.0_dp
        End Select
End Subroutine LegendreP_Scalar

Subroutine LegendreP_Vector(x, n, P)
    ! More efficient to use the matrix version rather than calling this repeatedly,
    !   because it vectorizes and this is sequential.
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: x            ! Argument
    Integer, Intent(In) :: n             ! Max order of Legendre Polynomials
    Real(dp), Intent(Out) :: P(0:n)      ! P(j) = LegendreP(j, x)
    Integer :: j
    
    Select Case (n)
        Case (2:)
            P(0) = 1._dp
            P(1) = x
            Do j = 1,n-1
                P(j+1) = (Real(2*j+1,dp)/Real(j+1,dp)) * x * P(j) - (Real(j,dp)/Real(j+1,dp)) * P(j-1)
            End do
        Case(1)
            P(0) = 1._dp
            P(1) = x
        Case(0)
            P = 1._dp
        End Select
End Subroutine LegendreP_Vector

Subroutine LegendreP_Matrix(nx, x, n, P)
    Use Kinds, Only: dp
    Implicit None
    Integer, Intent(In) :: nx            ! Number of arguments
    Real(dp), Intent(In) :: x(1:nx)       ! Arguments
    Integer, Intent(In) :: n     ! Max order of Legendre Polynomials
    Real(dp), Intent(Out) :: P(1:nx,0:n)  ! P(k,j) = LegendreP(j, x(k))
           ! The inconvenient subscript order is required so that the vector operations are on P(:,j), not P(j,:),
           !  for sequential storage of vector arithmetic arguments.
    Integer :: j
    
    Select Case (n)
        Case (2:)
            P(:,0) = 1._dp
            P(:,1) = x
            Do j = 1,n-1
                P(:,j+1) = (Real(2*j+1,dp)/Real(j+1,dp)) * x(:) * P(:,j) - (Real(j,dp)/Real(j+1,dp)) * P(:,j-1)
            End do
        Case(1)
            P(:,0) = 1._dp
            P(:,1) = x
        Case(0)
            P = 1._dp
        End Select           
End Subroutine LegendreP_Matrix

End Module Legendre_Utilities

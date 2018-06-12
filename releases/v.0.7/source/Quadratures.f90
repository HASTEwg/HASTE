Module Quadratures
    
    Use Kinds, Only: dp
    Implicit None
    Private
    Public :: Romberg_Quad
    Public :: Romberg_Simpson_Quad
    Public :: Composite_Quad
    Public :: Composite_Trapezoid
    Public :: Composite_Simpson
    Public :: Adaptive_Quad
    Public :: Adaptive_Simpson
    Public :: MidPoint
    Public :: Trapezoid
    Public :: Simpson
    Public :: GaussLegendre5
    Public :: GaussLegendre16
    Public :: GaussLegendre96
    
    Real(dp), Parameter :: one_third = 1._dp / 3._dp
    Real(dp), Parameter :: one_sixth = 1._dp / 6._dp
    Real(dp), Parameter :: one_fifteenth = 1._dp / 15._dp
    !Precomuted parameters for Romberg Quadrature routines
    Real(dp), Parameter :: Romb1(1:10) = (/ 4._dp, &
                                          & 4._dp**2, &
                                          & 4._dp**3, &
                                          & 4._dp**4, &
                                          & 4._dp**5, &
                                          & 4._dp**6, &
                                          & 4._dp**7, &
                                          & 4._dp**8, &
                                          & 4._dp**9, &
                                          & 4._dp**10 /)
    Real(dp), Parameter :: Romb2(1:10) = 1._dp / (Romb1 - 1._dp)
    
Contains

Function Romberg_Quad(f,a,b,aTol,rTol) Result(q)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Real(dp):: q    !the result of the integration
    Interface
        Function f(x)    !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances for convergence
    Real(dp) :: R(0:10,0:10)  !Romberg table
    Integer :: n,i,j
    Real(dp) :: h,s

    n = 1
    h = b - a
    s = 0.5_dp * (f(a) + f(b))
    R(0,0) = h * s
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        Do j = 1,i
            R(j,i) = Romb2(j) * (Romb1(j) * R(j-1,i) - R(j-1,i-1))
        End Do
        !check for convergence
        If (Converged(R(i-1,i-1),R(i,i),rTol,aTol)) Then
            q = R(i,i)  !R(i,i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
    End Do
    !If we get this far, we did not converge
    Call Continue_Romberg(f,a,b,rTol,aTol,s,10,R(:,10),2,q)
End Function Romberg_Quad

Recursive Subroutine Continue_Romberg(f,a,b,rTol,aTol,s,d,R0,level,q)  !adds 10 more rows to the previous Romberg_Quad table
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Interface
        Function f(x)    !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances for convergence
    Real(dp), Intent(InOut) :: s  !previous sum of ordinates
    Integer, Intent(In) :: d  !length of final row in OLD Romberg Table
    Real(dp), Intent(In) :: R0(0:d)  !final row of OLD romberg table
    Integer, Intent(In) :: level
    Real(dp), Intent(Out) :: q    !the result of the integration, if convergence attained
    Real(dp) :: R(0:d+10,0:10)  !Romberg table extension
    Integer :: n,i,j
    Real(dp) :: h
    Integer :: fours
    
    R(0:d,0) = R0
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = 2**(d+i)
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        fours = 1
        Do j = 1,d+i
            fours = fours * 4
            R(j,i) = (Real(fours,dp) * R(j-1,i) - R(j-1,i-1)) / Real(fours - 1,dp)
            !R(j,i) = (((4._dp)**j) * R(j-1,i) - R(j-1,i-1)) / (((4._dp)**j) - 1._dp)
        End Do
        !check for convergence
        If (Converged(R(d+i-1,i-1),R(d+i,i),rTol,aTol)) Then
            q = R(d+i,i)
            Return  !Normal exit
        End If
    End Do
    If (level .GT. 10) Then !max allowed recursion depth, interval has been split 100 times...
        Print *,"ERROR:  Quadratures: Continue_Romberg:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !If we get this far, we did not converge, recurse to add 10 more rows
    Call Continue_Romberg(f,a,b,rTol,aTol,s,d+10,R(:,10),level+1,q)
End Subroutine Continue_Romberg

Function Romberg_Simpson_Quad(f,a,b,aTol,rTol) Result(q)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Real(dp):: q    !the result of the integration
    Interface
        Function f(x)    !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances for convergence
    Real(dp) :: R(0:10,0:10)  !Romberg table
    Integer :: n,i,j
    Real(dp) :: h,s1,s2,s3
    
    n = 1
    h = 0.5_dp * (b - a)
    s1 = f(a) + f(b)
    s2 = 0._dp
    s3 = f(0.5_dp*(a+b))
    R(0,0) = h * (s1 + 4._dp*s3) * one_third
    Do i = 1,10
        !compute simpson estimate for next row of table
        n = n * 2
        s2 = s2 + s3
        h = (b - a) / Real(2*n,dp)
        s3 = 0._dp
        Do j = 1,2*n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s3 = s3 + f(a + Real(j,dp)*h)
        End Do
        R(0,i) = h * (s1 + 2._dp*s2 + 4._dp*s3) * one_third
        !fill out Romberg table row
        Do j = 1,i
            !R(j,i) = (((4._dp)**j) * R(j-1,i) - R(j-1,i-1)) / (((4._dp)**j) - 1._dp)
            R(j,i) = Romb2(j) * (Romb1(j) * R(j-1,i) - R(j-1,i-1))
        End Do
        !check for convergence
        If (Converged(R(i-1,i-1),R(i,i),rTol,aTol)) Then
            q = R(i,i)  !R(i,i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
    End Do
    !If we get this far, we did not converge
    Call Continue_Romberg_Simpson(f,a,b,rTol,aTol,s1,s2,s3,10,R(:,10),2,q)
End Function Romberg_Simpson_Quad

Recursive Subroutine Continue_Romberg_Simpson(f,a,b,rTol,aTol,s1,s2,s3,d,R0,level,q)  !adds 10 more rows to the previous Romberg_Quad table
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Interface
        Function f(x)    !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp):: f
            Real(dp), Intent(In):: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances for convergence
    Real(dp), Intent(InOut) :: s1,s2,s3  !previous sum of ordinates
    Integer, Intent(In) :: d  !length of final row in OLD Romberg Table
    Real(dp), Intent(In) :: R0(0:d)  !final row of OLD romberg table
    Integer, Intent(In) :: level
    Real(dp), Intent(Out) :: q    !the result of the integration, if convergence attained
    Real(dp) :: R(0:d+10,0:10)  !Romberg table extension
    Integer :: n,i,j
    Real(dp) :: h
    Integer :: fours
    
    R(0:d,0) = R0
    Do i = 1,10
        !compute simpson estimate for next row of table
        n = 2**(d+i)
        s2 = s2 + s3
        h = (b - a) / Real(2*n,dp)
        s3 = 0._dp
        Do j = 1,2*n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s3 = s3 + f(a + Real(j,dp)*h)
        End Do
        R(0,i) = h * (s1 + 2._dp*s2 + 4._dp*s3) * one_third
        !fill out Romberg table row
        fours = 1
        Do j = 1,d+i
            fours = fours * 4
            R(j,i) = (Real(fours,dp) * R(j-1,i) - R(j-1,i-1)) / Real(fours - 1,dp)
            !R(j,i) = (((4._dp)**j) * R(j-1,i) - R(j-1,i-1)) / (((4._dp)**j) - 1._dp)
        End Do
        !check for convergence
        If (Converged(R(d+i-1,i-1),R(d+i,i),rTol,aTol)) Then
            q = R(d+i,i)
            Return  !Normal exit
        End If
    End Do
    If (level .GT. 10) Then !max allowed recursion depth, interval has been split 100 times...
        Print *,"ERROR:  Quadratures: Continue_Romberg_Simpson:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !If we get this far, we did not converge, recurse to add 10 more rows
    Call Continue_Romberg_Simpson(f,a,b,rTol,aTol,s1,s2,s3,d+10,R(:,10),level+1,q)
End Subroutine Continue_Romberg_Simpson

Function Adaptive_Quad(f,a,b,Quad,rTol,aTol) Result(q)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Real(dp) :: q    !the result of the integration
    Interface
        Function f(x)  !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
        Function Quad(f,a,b)  !the quadrature rule to use
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: Quad  !the result of the integration
            Interface
                Function f(x)    !the function to be integrated
                    Use Kinds,Only: dp
                    Implicit None
                    Real(dp) :: f
                    Real(dp), Intent(In) :: x
                End Function f
            End Interface
            Real(dp),Intent(In) :: a,b  !limits of integration
        End Function Quad
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerance to be used on each interval as criteria for convergence
    Real(dp) :: q0,q1,q2
    
    q0 = Quad(f,a,b)
    q1 = Quad(f,a,0.5_dp*(a+b))
    q2 = Quad(f,0.5_dp*(a+b),b)
    q = q1 + q2
    If (Converged(q,q0,rTol,aTol)) Return  !check convergence with specified tolerances
    !otherwise, recurse to refine grid
    q = Continue_Adaptive_Quad(f,a,0.5_dp*(a+b),Quad,q1,rTol,0.5_dp*aTol,level=2) + &
      & Continue_Adaptive_Quad(f,0.5_dp*(a+b),b,Quad,q2,rTol,0.5_dp*aTol,level=2)
End Function Adaptive_Quad

Recursive Function Continue_Adaptive_Quad(f,a,b,Quad,q0,rTol,aTol,level) Result(q)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Real(dp) :: q    !the result of the integration
    Interface
        Function f(x)  !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
        Function Quad(f,a,b)  !the quadrature rule to use
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: Quad  !the result of the integration
            Interface
                Function f(x)    !the function to be integrated
                    Use Kinds,Only: dp
                    Implicit None
                    Real(dp) :: f
                    Real(dp), Intent(In) :: x
                End Function f
            End Interface
            Real(dp),Intent(In) :: a,b  !limits of integration
        End Function Quad
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: q0
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerance to be used on each interval as criteria for convergence
    Integer, Intent(In) :: level
    Real(dp) :: q1,q2
    
    q1 = Quad(f,a,0.5_dp*(a+b))
    q2 = Quad(f,0.5_dp*(a+b),b)
    q = q1 + q2
    If (Converged(q,q0,rTol,aTol)) Return  !check convergence with specified tolerances
    If (level .GT. 100) Then !max allowed recursion depth, interval has been split 101 times...
        Print *,"ERROR:  Quadratures: Continue_Adaptive_Quad:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !otherwise, recurse to refine grid
    q = Continue_Adaptive_Quad(f,a,0.5_dp*(a+b),Quad,q1,rTol,0.5_dp*aTol,level+1) + &
      & Continue_Adaptive_Quad(f,0.5_dp*(a+b),b,Quad,q2,rTol,0.5_dp*aTol,level+1)
End Function Continue_Adaptive_Quad

Function Adaptive_Simpson(f,a,b,rTol,aTol) Result(q)
    Use Kinds,Only: dp
    Implicit None
    Real(dp) :: q    !the result of the integration
    Interface
        Function f(x)  !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerance to be used on each interval as criteria for convergence
    Real(dp) :: x2,x3,x4
    Real(dp) :: y1,y2,y3,y4,y5
    Real(dp) :: q0,q1,q2
    Real(dp) :: err_est
    
    x3 = 0.5_dp * (a + b)
    x2 = 0.5_dp * (a + x3)
    x4 = 0.5_dp * (x3 + b)
    y1 = f(a)
    y2 = f(x2)
    y3 = f(x3)
    y4 = f(x4)
    y5 = f(b)
    q0 = (b-a) * (y1 + 4._dp*y3 + y5) * one_sixth
    q1 = (x3-a) * (y1 + 4._dp*y2 + y3) * one_sixth
    q2 = (b-x3) * (y3 + 4._dp*y4 + y5) * one_sixth
    err_est = (q1 + q2 - q0) * one_fifteenth
    If (Abs(err_est).LE.aTol .OR. Abs(err_est).LE.rTol*Min(Abs(q1+q2),Abs(q0))) Then  !check convergence with specified tolerances
        q = q1 + q2 + err_est
        Return
    End If
    !otherwise, recurse to refine grid
    q = Continue_Adaptive_Simpson(f,a,x2,x3,y1,y2,y3,q1,rTol,0.5_dp*aTol,level=2) + &
      & Continue_Adaptive_Simpson(f,x3,x4,b,y3,y4,y5,q2,rTol,0.5_dp*aTol,level=2)
End Function Adaptive_Simpson

Recursive Function Continue_Adaptive_Simpson(f,x1,x3,x5,y1,y3,y5,q0,rTol,aTol,level) Result(q)
    Use Kinds, Only: dp
    Implicit none
    Real(dp) :: q
    Interface
        Function f(x)  !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: x1,x3,x5
    Real(dp), Intent(In) :: y1,y3,y5
    Real(dp), Intent(In) :: q0
    Real(dp), Intent(In) :: rTol,aTol
    Integer, Intent(In) :: level
    Real(dp) :: x2,x4
    Real(dp) :: y2,y4
    Real(dp) :: q1,q2
    Real(dp) :: err_est
    
    x2 = 0.5_dp * (x1 + x3)
    x4 = 0.5_dp * (x3 + x5)
    y2 = f(x2)
    y4 = f(x4)
    q1 = (x3-x1) * (y1 + 4._dp*y2 + y3) * one_sixth
    q2 = (x5-x3) * (y3 + 4._dp*y4 + y5) * one_sixth
    err_est = (q1 + q2 - q0) * one_fifteenth
    If (Abs(err_est).LE.aTol .OR. Abs(err_est).LE.rTol*Min(Abs(q1+q2),Abs(q0))) Then  !check convergence with specified tolerances
        q = q1 + q2 + err_est
        Return
    End If
    If (level .GT. 100) Then !max allowed recursion depth, interval has been split 101 times...
        Print *,"ERROR:  Quadratures: Continue_Adaptive_Simpson:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !Otherwise, recurse to refine grid
    q = Continue_Adaptive_Simpson(f,x1,x2,x3,y1,y2,y3,q1,rTol,0.5_dp*aTol,level+1) + &
      & Continue_Adaptive_Simpson(f,x3,x4,x5,y3,y4,y5,q2,rTol,0.5_dp*aTol,level+1)
End Function Continue_Adaptive_Simpson

Function Composite_Quad(f,a,b,Quad,aTol,rTol) Result(q)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Real(dp) :: q    !the result of the integration
    Interface
        Function f(x)  !the function to be integrated
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
        Function Quad(f,a,b)  !the quadrature rule to use
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: Quad  !the result of the integration
            Interface
                Function f(x)    !the function to be integrated
                    Use Kinds ,Only: dp
                    Implicit None
                    Real(dp) :: f
                    Real(dp), Intent(In) :: x
                End Function f
            End Interface
            Real(dp), Intent(In) :: a,b  !limits of integration
        End Function Quad
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: aTol,rTol
    Integer :: i,j,n
    Real(dp) :: h,qOld
    
    n = 1
    qOld = Quad(f,a,b)
    Do i = 1,100  !max intervals is bounded by splitting interval 100 times
        q = 0._dp
        n = n*2
        h = (b-a) / Real(n,dp)
        Do j = 1,n
            q = q + Quad(f,a+(Real((j-1),dp)*h),a+(Real(j,dp)*h))
        End Do
        If (Converged(q,qOld,rTol,aTol)) Return  !check convergence with specified tolerances
        qOld = q
    End Do
    !if we get this far, we failed to converge
    Print *,"ERROR:  Quadratures: Composite_Quad:  Failed to converge on ",n," intervals."
    ERROR STOP
End Function Composite_Quad

Function MidPoint(f,a,b) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: q    !the result of the integration
    Interface
        Function f(x)    !the function to be integrated
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration

    q = (b-a) * f(0.5_dp*(a+b))
End Function MidPoint

Function Trapezoid(f,a,b) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: q    !the result of the integration
    Interface
        Function f(x)    !the function to be integrated
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration

    q = 0.5_dp * (b-a) * (f(a) + f(b))
End Function Trapezoid

Function Simpson(f,a,b) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Interface
        Function f(x)    !the function to be integrated
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    
    q = (b-a) * (f(a) + 4._dp*f(0.5_dp*(a+b)) + f(b)) * one_sixth
End Function Simpson

Function Composite_Trapezoid(f,a,b,aTol,rTol) Result(q)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Real(dp) :: q    !the result of the integration
    Interface
        Function f(x)  !the function to be integrated
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: aTol,rTol
    Integer :: i,j,n
    Real(dp) :: qOld,s,h
    
    n = 1
    s = 0.5_dp * (f(a) + f(b))
    qOld = (b-a) * s
    Do i = 1,100
        n = n*2
        h = (b-a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h)
        End Do
        q = h * s
        If (Converged(q,qOld,rTol,aTol)) Return  !check convergence with specified tolerances
        qOld = q
    End Do
    !if we get this far, we failed to converge
    Print *,"ERROR:  Quadratures: Composite_Trapezoid:  Failed to converge on ",n," intervals."
    ERROR STOP
End Function Composite_Trapezoid

Function Composite_Simpson(f,a,b,aTol,rTol) Result(q)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Real(dp) :: q    !the result of the integration
    Interface
        Function f(x)  !the function to be integrated
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(In) :: aTol,rTol
    Integer :: i,j,n
    Real(dp) :: qOld,s1,s2,s3,h
    
    n = 1
    s1 = f(a) + f(b)
    s2 = 0._dp
    s3 = f(0.5_dp*(a+b))
    qOld = (b-a) * (s1 + 4._dp*s3) * one_sixth
    Do i = 1,100
        n = n*2
        h = (b-a) / Real(2*n,dp)
        s2 = s2 + s3
        s3 = 0._dp
        Do j = 1,2*n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s3 = s3 + f(a + Real(j,dp)*h)
        End Do
        q = h * (s1 + 2._dp*s2 + 4._dp*s3) * one_third
        If (Converged(q,qOld,rTol,aTol)) Return  !check convergence with specified tolerances
        qOld = q
    End Do
    !if we get this far, we failed to converge
    Print *,"ERROR:  Quadratures: Composite_Simpson:  Failed to converge on ",n," intervals."
    ERROR STOP
End Function Composite_Simpson

Function GaussLegendre(f,a,b,n,wi,xi) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: q
    Interface
        Function f(x)  !the function to integrate
            Use Kinds, Only: dp    
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b  !limits of integration
    Integer, Intent(In) :: n
    Real(dp), Intent(In) :: wi(1:n),xi(1:n)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i
    Real(dp) :: fi(1:n)
    
    c1 = 0.5_dp * (b-a)
    c2 = 0.5_dp * (b+a)
    Do i = 1,n
        fi(i) = f(c1 * xi(i) + c2)
    End Do
    q = c1 * Dot_Product(wi,fi)
End Function GaussLegendre

Function GaussLegendre5(f,a,b) Result(q)
    Use Kinds, Only: dp
    Use GaussLegendre_w_a, Only: n => n005
    Use GaussLegendre_w_a, Only: wi => w005
    Use GaussLegendre_w_a, Only: xi => a005
    Implicit None
    Real(dp) :: q  !result of the integration
    Interface
        Function f(x)  !the function to integrate
            Use Kinds, Only: dp    
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b  !limits of integration
    
    q = GaussLegendre(f,a,b,n,wi,xi)
End Function GaussLegendre5

Function GaussLegendre16(f,a,b) Result(q)
    Use Kinds, Only: dp
    Use GaussLegendre_w_a, Only: n => n016
    Use GaussLegendre_w_a, Only: wi => w016
    Use GaussLegendre_w_a, Only: xi => a016
    Implicit None
    Real(dp) :: q  !result of the integration
    Interface
        Function f(x)  !the function to integrate
            Use Kinds, Only: dp    
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b  !limits of integration
    
    q = GaussLegendre(f,a,b,n,wi,xi)
End Function GaussLegendre16

Function GaussLegendre96(f,a,b) Result(q)
    Use Kinds, Only: dp
    Use GaussLegendre_w_a, Only: n => n096
    Use GaussLegendre_w_a, Only: wi => w096
    Use GaussLegendre_w_a, Only: xi => a096
    Implicit None
    Real(dp) :: q  !result of the integration
    Interface
        Function f(x)  !the function to integrate
            Use Kinds, Only: dp    
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b  !limits of integration
    
    q = GaussLegendre(f,a,b,n,wi,xi)
End Function GaussLegendre96

End Module Quadratures
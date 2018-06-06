Module Root_Solvers
    
    Implicit None
    
    Private
    Public :: RootLaguerre
    Public :: RootNewton
    Public :: RootSteffensen
    Public :: RootSecant
    Public :: RootBisect
    Public :: RootFalsePos
    Public :: RootIllinois
    
Contains

Function RootLaguerre(f,df,d2f,x0,rTol,aTol,n0) Result(r)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Interface
        Function f(x)  !function being root-solved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
        Function df(x)  !derivative w/ respect to x of function being root-solved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: df
            Real(dp), Intent(In) :: x
        End Function df
        Function d2f(x)  !second derivative w/ respect to x of function being root-solved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: d2f
            Real(dp), Intent(In) :: x
        End Function d2f
    End Interface
    Real(dp) :: r
    Real(dp), Intent(In) :: x0  !initial guess for root
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances
    Integer, Intent(In), Optional :: n0
    Real(dp) :: n1,n2,n3
    Real(dp) :: fr,dfr,d2fr
    Real(dp) :: old_r  !root from previous iteration
    Integer :: i  !counter, counts iterations of the solver
    
    If (Present(n0)) Then
        n1 = Real(n0,dp)
        n2 = Real((n0-1)**2,dp)
        n3 = Real(n0*(n0-1),dp)
    Else
        n1 = 5._dp
        n2 = 16._dp
        n3 = 20._dp
    End If
    r = x0
    Do i = 1,100
        old_r = r
        fr = f(r)
        dfr = df(r)
        d2fr = d2f(r)
        r = r - n1*fr / (dfr + Sign(1._dp,dfr)*Sqrt(n2*dfr**2-n3*fr*d2fr))
        If (Converged(r,old_r,rTol,aTol)) Return  !Normal Exit
    End Do
    !If we get this far, we failed to converge in 100 Laguerre iterations...
    Print *,'ERROR:  Root_Solvers: RootLaguerre:  Failed to converge on a root.'
    ERROR STOP
End Function RootLaguerre

Function RootNewton(f,df,x0,rTol,aTol) Result(r)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Interface
        Function f(x)  !function being root-solved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
        Function df(x)  !derivative w/ respect to x of function being root-solved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: df
            Real(dp), Intent(In) :: x
        End Function df
    End Interface
    Real(dp) :: r
    Real(dp), Intent(In) :: x0  !initial guess for root
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances
    Real(dp) :: old_r  !root from previous iteration
    Integer :: i  !counter, counts iterations of the solver
    
    r = x0
    Do i = 1,100
        old_r = r
        r = r - f(r) / df(r)
        If (Converged(r,old_r,rTol,aTol)) Return  !Normal Exit
    End Do
    !If we get this far, we failed to converge in 100 Newton iterations...
    Print *,'ERROR:  Root_Solvers: RootNewton:  Failed to converge on a root.'
    ERROR STOP
End Function RootNewton

Function RootSteffensen(f,x0,rTol,aTol) Result(r)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Interface
        Function f(x)  !function being rootsolved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp) :: r
    Real(dp), Intent(In) :: x0  !initial guess for root
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances
    Real(dp) :: old_r  !root from previous iteration
    Integer :: i  !counter, counts iterations of the solver
    Real(dp) :: fr,gr
    
    r = x0
    Do i = 1,100
        old_r = r
        fr = f(r)
        gr = f(r + fr) / fr
        r = r - fr / gr
        If (Converged(r,old_r,rTol,aTol)) Return  !Normal Exit
    End Do
    !If we get this far, we failed to converge in 100 Steffensen iterations...
    Print *,'ERROR:  Root_Solvers: RootSteffensen:  Failed to converge on a root.'
    ERROR STOP
End Function RootSteffensen

Function RootSecant(f,x0,x1,rTol,aTol) Result(r)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Interface
        Function f(x)  !function being rootsolved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp) :: r
    Real(dp), Intent(In) :: x0,x1  !initial guesses for root
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances
    Real(dp) :: old_r  !root from previous iteration
    Integer :: i  !counter, counts iterations of the solver
    Real(dp) :: f0,f1
    
    old_r = x0
    f0 = f(x0)
    r = x1
    Do i = 1,1000
        f1 = f(r)
        r = r - f1 * (r-old_r) / (f1-f0)
        If (Converged(r,old_r,rTol,aTol)) Return  !Normal Exit
        old_r = r
        f0 = f1
    End Do
    !If we get this far, we failed to converge in 1000 Secant iterations...
    Print *,'ERROR:  Root_Solvers: RootSecant:  Failed to converge on a root.'
    ERROR STOP
End Function RootSecant

Function RootBisect(f,a,b,rTol,aTol) Result(r)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Interface
        Function f(x)  !function being rootsolved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp) :: r
    Real(dp), Intent(InOut) :: a,b  !left and right boundaries bracketing root
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances
    Real(dp) :: old_r  !root from previous iteration
    Integer :: i  !counter, counts iterations of the solver

    r = a
    fa = f(a)
    fb = f(b)
    Do i = 1,1000
        old_r = r
        r = 0.5_dp * (a + b)
        If (Converged(r,old_r,rTol,aTol)) Return  !Normal Exit
        fr = f(r)
        If (fr.EQ.0._dp .OR. a.EQ.b) Return  !Interation landed on root, or interval closed to zero width
        Call Update_Brackets(a,fa,b,fb,r,fr)
    End Do
    !If we get this far, we failed to converge in 1,000 Bisection iterations...
    Print *,'ERROR:  Root_Solvers: RootBisect:  Failed to converge on a root.'
    ERROR STOP
End Function RootBisect

Subroutine Update_Brackets(a,fa,b,fb,r,fr)
    Use Kinds, Only: dp
    Implcit None
    Real(dp), Intent(InOut) :: a,fa
    Real(dp), Intent(InOut) :: b,fb
    Real(dp), Intent(In) :: r,fr
    
    If (fr*fa .GE. 0._dp) Then !fr and fa have same sign
        a = r
        fa = fr
    Else !fr and fb have same sign
        b = r
        fb = fr
    End If    
End Subroutine Update_Brackets

Function RootFalsePos(f,a,b,rTol,aTol,Illinois) Result(r)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Interface
        Function f(x)  !function being rootsolved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp) :: r
    Real(dp), Intent(InOut) :: a,b  !left and right boundaries bracketing root
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances
    Logical, Intent(In), Optional :: Illinois
    Logical = ill_fix
    Real(dp) :: old_r  !root from previous iteration
    Real(dp) :: old_fr  !function at root from previous iteration
    Integer :: i,j  !counters
    Real(dp) :: fr  !function value at current root guess
    Real(dp) :: fa,fb  !function values at left/right brackets

    If (Present(Illinois)) Then
        ill_fix = Illinois
    Else
        ill_fix = .FALSE.
    End If
    !false position starter (the starter is necessary to obtain an old_fr for Illinois method)
    fa = f(a)
    fb = f(b)
    r = FalsePos(a,b,fa,fb)
    fr = f(r)
    Call Update_Brackets(a,fa,b,fb,r,fr)
    Do i = 1,1000
        old_r = r
        old_fr = fr
        !False-Position for next r
        r = FalsePos(a,b,fa,fb)
        If (Converged(r,old_r,rTol,aTol)) Return  !Normal Exit
        fr = f(r)
        If (fr.EQ.0._dp .OR. a.EQ.b) Return  !Interation landed on root, or interval closed to zero width
        If (ill_fix) Then !apply Illinois fix to false position
            Call Do_Illinois(f,a,fa,b,fb,r,fr,old_fr)
        Else !regular false-position
            Call Update_Brackets(a,fa,b,fb,r,fr)
        End If
    End Do
    !If we get this far, we failed to converge in 1,000 False-Position/Illinois iterations...
    If (ill_fix) Then
        Print *,'ERROR:  Root_Solvers: RootFalsePos w/ Illinois fix:  Failed to converge on a root.'
    Else
        Print *,'ERROR:  Root_Solvers: RootFalsePos:  Failed to converge on a root.'
    End If
    ERROR STOP
End Function RootFalsePos

Function FalsePos(x1,x2,y1,y2) Result(x3)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x3
    Real(dp), Intent(In) :: x1,x2
    Real(dp), Intent(In) :: y1,y2
        
    If (y1 .NE. y2) Then
        x3 = (x1*y2 - x2*y1) / (y2 - y1)
    Else
        x3 = 0.5_dp * (x1 + x2)
    End If
End Function FalsePos

Subroutine Do_Illinois(f,a,fa,b,fb,r,fr,old_fr)
    Use Kinds, Only: dp
    Implicit None
    Interface
        Function f(x)  !function being rootsolved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp), Intent(InOut), Target :: a,fa
    Real(dp), Intent(InOut), Target :: b,fb
    Real(dp), Intent(InOut) :: r,fr,old_fr
    Real(dp), Target :: ma,mb  !function value modifiers for Illinois
    Real(dp), Pointer :: rep,frep,mrep  !pointers to 'repeating' bracket (the one on the side of the root that is repeating)
    Real(dp), Pointer :: far,ffar,mfar  !pointers to 'far' bracket (the one Illinois forces to move)
    
    !check for poor behavior of False-Position
    If (fr*old_fr .GE. 0._dp) Then !fr and old_fr have same sign, False-Position is behaving poorly, apply Illinois
        If (fr*fa .GE. 0._dp) Then !bracket 'b' is on the 'far' side, function value fb will be modified
            rep  => a
            frep => fa
            mrep => ma
            far  => b
            ffar => fb
            mfar => mb
        Else !bracket 'a' is on the 'far' side, function value fa will be modified
            rep  => b
            frep => fb
            mrep => mb
            far  => a
            ffar => fa
            mfar => ma
        End If
        mrep = 1._dp
        mfar = 0.5_dp
        Do j = 1,10 !index limits number of illinois refinements so a simple sign-change indicates success
        !If a normal exit is not encountered in 10 iterations, the repeating bracket is still refined with each iterations, but the routine terminates without moving the other bracket.  This (excessively) poor performance of False-Position w/ Illinois should be rare.
            !move repeating bracket
            rep = r
            frep = fr
            !False-Position w/ Illinois for next r
            r = FalsePos(a,b,fa*ma,fb*mb)
            fr = f(r)
            !check for sign change in fr
            If (fr*frep .LT. 0._dp) Then  !Illinois successfully moved BOTH brackets w/in 10 attempts
                !move close bracket
                far = r
                ffar = fr
                Exit  !normal exit
            End If
            !Otherwise, one bracket has still been refined, set a more agressive m and cycle
            mfar = mfar * 0.5_dp
        End Do
    Else !False-Position is behaving well, replace appropriate bracket
        Call Update_Brackets(a,fa,b,fb,r,fr)
    End If
End Subroutine Do_Illinois

Function RootIllinois(f,a,b,rTol,aTol) Result(r)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Interface
        Function f(x)  !function being rootsolved
            Use Kinds, Only: dp
            Implicit None
            Real(dp) :: f
            Real(dp), Intent(In) :: x
        End Function f
    End Interface
    Real(dp) :: r
    Real(dp), Intent(InOut) :: a,b  !left and right boundaries bracketing root
    Real(dp), Intent(In) :: rTol,aTol  !relative and absolute tolerances
    
    r = RootFalsePos(f,a,b,rTol,aTol,Illinois = .TRUE.)
Ens Function RootIllinois

End Module Root_Solvers

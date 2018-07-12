Program testQuadratures

Use Kinds, Only: dp
Use Global, Only: pi
Use FileIO_Utilities, Only: delta_Time
Use Quadratures, Only: Romberg_Quad

Implicit None

Integer :: c
Real(dp) :: dt
Integer, Parameter :: n = 1000
Real(dp) :: f(0:n),g(0:n)
Integer, SAVE :: calls1,calls2
Real(dp) :: r(1:n)
Integer :: i

Call RANDOM_SEED()
Call RANDOM_NUMBER(r)

f = 0._dp
g = 0._dp
calls1 = 0
calls2 = 0
Call SYSTEM_CLOCK(c)
f(0) = Romberg_Quad(fSin,0._dp,pi,rtol = 1.E-12_dp,atol = 0._dp)
g(0) = Romberg_Quad(fExp,0._dp,Log(2._dp),rtol = 1.E-12_dp,atol = 0._dp)
Do i = 1,n
    f(i) = Romberg_Quad(fSin,0._dp,r(i)*pi,rtol = 1.E-12_dp,atol = 0._dp)
    g(i) = Romberg_Quad(fExp,0._dp,r(i)*Log(2._dp),rtol = 1.E-12_dp,atol = 0._dp)
End Do
dt = delta_Time(clock_then = c)
Write(*,*)
Write(*,'(A)') 'Romberg'
Write(*,'(A)') '---------'
Write(*,'(A,F20.16)') 'Integral Sin(x**3) from x = 0 to Pi:    ',f(0)
Write(*,'(A,F20.16)') 'Integral Exp(x**3) from x = 0 to Ln(2): ',g(0)
Write(*,'(A,F20.6)') 'Compute time:  ',dt
Write(*,'(A,I0,A,I0)') 'Function calls:  ',calls1,', ',calls2

Contains

Function fSin(x) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: x
    
    f = Sin(x**3)
    calls1 = calls1 + 1
End Function fSin

Function fExp(x) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: x
    
    f = Exp(x**3)
    calls2 = calls2 + 1
End Function fExp

End Program testQuadratures

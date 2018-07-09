Program testQuadratures

Use Kinds, Only: dp
Use Global, Only: pi
Use Utilities, Only: delta_Time
Use Quadratures, Only: Extrapolation_Quad
Use Quadratures, Only: Romberg_Quad
Use Quadratures, Only: Progressive_GaussLegendre

Implicit None

Integer :: c
Real(dp) :: dt
Integer, Parameter :: n = 100
Real(dp) :: f(0:n)
Real(dp) :: g(0:n)
Real(dp) :: r(1:n)
Integer :: i

Call RANDOM_SEED(7777777)
Call RANDOM_NUMBER(r)

f = 0._dp
g = 0._dp
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
Write(*,'(A,F20.16)') 'Integral Sin(x) from x = 0 to Pi:    ',f(0)
Write(*,'(A,F20.16)') 'Integral Exp(x) from x = 0 to Ln(2): ',g(0)
Write(*,'(A,F20.6)') 'Compute time:  ',dt

f = 0._dp
g = 0._dp
Call SYSTEM_CLOCK(c)
f(0) = Extrapolation_Quad(fSin,0._dp,pi,rtol = 1.E-12_dp,atol = 0._dp)
g(0) = Extrapolation_Quad(fExp,0._dp,Log(2._dp),rtol = 1.E-12_dp,atol = 0._dp)
Do i = 1,n
    f(i) = Extrapolation_Quad(fSin,0._dp,r(i)*pi,rtol = 1.E-12_dp,atol = 0._dp)
    g(i) = Extrapolation_Quad(fExp,0._dp,r(i)*Log(2._dp),rtol = 1.E-12_dp,atol = 0._dp)
End Do
dt = delta_Time(clock_then = c)
Write(*,*)
Write(*,'(A)') 'Extrapolation'
Write(*,'(A)') '---------------'
Write(*,'(A,F20.16)') 'Integral Sin(x) from x = 0 to Pi:    ',f(0)
Write(*,'(A,F20.16)') 'Integral Exp(x) from x = 0 to Ln(2): ',g(0)
Write(*,'(A,F20.6)') 'Compute time:  ',dt

f = 0._dp
g = 0._dp
Call SYSTEM_CLOCK(c)
f(0) = Progressive_GaussLegendre(fSin,0._dp,pi,rtol = 1.E-12_dp,atol = 0._dp)
g(0) = Progressive_GaussLegendre(fExp,0._dp,Log(2._dp),rtol = 1.E-12_dp,atol = 0._dp)
Do i = 1,n
    f(i) = Progressive_GaussLegendre(fSin,0._dp,r(i)*pi,rtol = 1.E-12_dp,atol = 0._dp)
    g(i) = Progressive_GaussLegendre(fExp,0._dp,r(i)*Log(2._dp),rtol = 1.E-12_dp,atol = 0._dp)
End Do
dt = delta_Time(clock_then = c)
Write(*,*)
Write(*,'(A)') 'Progressive_GaussLegendre'
Write(*,'(A)') '---------------------------'
Write(*,'(A,F20.16)') 'Integral Sin(x) from x = 0 to Pi:    ',f(0)
Write(*,'(A,F20.16)') 'Integral Exp(x) from x = 0 to Ln(2): ',g(0)
Write(*,'(A,F20.6)') 'Compute time:  ',dt

Contains

Function fSin(x) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: x
    
    f = Sin(x)
End Function fSin

Function fExp(x) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: x
    
    f = Exp(x)
End Function fExp

End Program testQuadratures

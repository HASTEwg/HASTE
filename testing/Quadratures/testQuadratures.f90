Program testQuadratures

Use Kinds, Only: dp
Use Global, Only: pi
Use FileIO_Utilities, Only: delta_Time
Use Quadratures, Only: Romberg_Simpson_Quad
Use Quadratures, Only: Romberg_Quad

Implicit None

Integer :: c
Real(dp) :: dt
Integer, Parameter :: n = 100
Real(dp) :: f,g
Integer, SAVE :: calls1,calls2

f = 0._dp
g = 0._dp
calls1 = 0
calls2 = 0
Call SYSTEM_CLOCK(c)
f = Romberg_Quad(fSin,0._dp,pi,rtol = 1.E-12_dp,atol = 0._dp)
g = Romberg_Quad(fExp,0._dp,Log(2._dp),rtol = 1.E-12_dp,atol = 0._dp)
dt = delta_Time(clock_then = c)
Write(*,*)
Write(*,'(A)') 'Romberg'
Write(*,'(A)') '---------'
Write(*,'(A,F20.16)') 'Integral Sin(x) from x = 0 to Pi:    ',f
Write(*,'(A,F20.16)') 'Integral Exp(x) from x = 0 to Ln(2): ',g
Write(*,'(A,F20.6)') 'Compute time:  ',dt
Write(*,'(A,I0,A,I0)') 'Function calls:  ',calls1,', ',calls2

f = 0._dp
g = 0._dp
calls1 = 0
calls2 = 0
Call SYSTEM_CLOCK(c)
f = Romberg_Simpson_Quad(fSin,0._dp,pi,rtol = 1.E-12_dp,atol = 0._dp)
g = Romberg_Simpson_Quad(fExp,0._dp,Log(2._dp),rtol = 1.E-12_dp,atol = 0._dp)
dt = delta_Time(clock_then = c)
Write(*,*)
Write(*,'(A)') 'Romberg Simpson'
Write(*,'(A)') '-----------------'
Write(*,'(A,F20.16)') 'Integral Sin(x) from x = 0 to Pi:    ',f
Write(*,'(A,F20.16)') 'Integral Exp(x) from x = 0 to Ln(2): ',g
Write(*,'(A,F20.6)') 'Compute time:  ',dt
Write(*,'(A,I0,A,I0)') 'Function calls:  ',calls1,', ',calls2

Contains

Function fSin(x) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: x
    
    f = Sin(x)
    calls1 = calls1 + 1
End Function fSin

Function fExp(x) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: x
    
    f = Exp(x)
    calls2 = calls2 + 1
End Function fExp

End Program testQuadratures

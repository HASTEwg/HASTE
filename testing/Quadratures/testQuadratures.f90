Program testQuadratures

Use Kinds, Only: id
Use Kinds, Only: dp
Use Global, Only: pi
Use FileIO_Utilities, Only: delta_Time
Use Quadratures, Only: Romberg_Quad
Use Utilities, Only: Prec

Implicit None

Integer(id) :: c
Real(dp) :: dt1,dt2
Real(dp) :: f,g
Integer, SAVE :: calls1,calls2
Integer :: n,i

f = 0._dp
g = 0._dp
calls1 = 0
calls2 = 0
n = 100000

Call SYSTEM_CLOCK(c)
Do i = 1,n
    f = Romberg_Quad(fSin,0._dp,pi,rtol = 1.E-12_dp,atol = 0._dp)
End Do
dt1 = delta_Time(clock_then = c)

Call SYSTEM_CLOCK(c)
Do i = 1,n
    g = Romberg_Quad(fExp,0._dp,Log(2._dp),rtol = 1.E-12_dp,atol = 0._dp)
End Do
dt2 = delta_Time(clock_then = c)

Write(*,*)
Write(*,'(A)') 'Romberg'
Write(*,'(A)') '---------'
Write(*,'(A,F19.16,A,F6.3,A)') 'Integral Sin(x**3) from x = 0 to Pi:    ',f,' w/ ',Prec(f,0.41583381465627398043_dp),' digits'
Write(*,'(A,F19.16,A,F6.3,A)') 'Integral Exp(x**3) from x = 0 to Ln(2): ',g,' w/ ',Prec(g,0.75680249730643946285_dp),' digits'
Write(*,'(2(A,ES10.3E3))') 'Compute times:  ',dt1/Real(n,dp),', ',dt2/Real(n,dp)
Write(*,'(2(A,I0))') 'Function calls:  ',calls1/n,', ',calls2/n

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

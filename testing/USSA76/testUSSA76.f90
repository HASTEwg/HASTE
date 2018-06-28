Program testUSSA76

Use Kinds, Only: dp
Use US_Std_Atm_1976, Only: Zb
Use US_Std_Atm_1976, Only: T
Use US_Std_Atm_1976, Only: P
Use US_Std_Atm_1976, Only: rho
Implicit None

Integer :: i,j,unit
Real(dp) :: z

Real(dp), Parameter :: dZmax = 1.E-3 !1 meter

Open(NEWUNIT=unit,FILE='TPRho.tst',ACTION='WRITE',STATUS='REPLACE')
z = Zb(0)
i = 1
j = 1
Do
    Write(unit,'(4ES24.16)') z,T(z),P(z),rho(z)
    z = Real(j,dp) * dZmax
    If (z .GE. Zb(i)) Then
        If (i .GT. Size(Zb)+1) Exit
        z = Zb(i)
        i = i + 1
    Else
        j = j + 1
    End If
End Do
Close(unit)

z = 0._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 86._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 90._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 91._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 92._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 97._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 100._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 115._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 250._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 500._dp
Write(*,'(F9.3,F32.15)') z,rho(z)

End Program
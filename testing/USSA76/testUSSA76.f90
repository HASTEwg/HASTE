Program testUSSA76

Use Kinds, Only: dp
Use US_Std_Atm_1976, Only: Zb
Use US_Std_Atm_1976, Only: T
Use US_Std_Atm_1976, Only: P
Use US_Std_Atm_1976, Only: rho
Implicit None

Integer :: i,j,unit
Real(dp) :: z
Real(dp) :: temp,pres,dens

Real(dp), Parameter :: dZmax = 1.E-3_dp !1 meter

Open(NEWUNIT=unit,FILE='TPRho.tst',ACTION='WRITE',STATUS='REPLACE')
i = 1
j = 1
z = 0._dp
Write(*,*)
Write(*,'(A9,3A14)') ' Z [km] ','   T [K]   ','   P [pa]   ',' rho [g/m^3]'
Write(*,'(A9,3A14)') '--------','-----------','------------','------------'
temp = T(z)
pres = P(z)
dens = rho(z)
Write(*,'(A,F9.3,3ES14.6)') ACHAR(13),z,temp,pres,dens
Write(unit,'(F9.3,3ES24.16)') z,temp,pres,dens
Do
    temp = T(z)
    pres = P(z)
    dens = rho(z)
    Write(*,'(A,F9.3,3ES14.6)',ADVANCE='NO') ACHAR(13),z,temp,pres,dens
    Write(unit,'(F9.3,3ES24.16)') z,temp,pres,dens
    z = Real(j,dp) * dZmax
    If (z .GE. Zb(i)) Then
        If (i .GT. Size(Zb)+1) Exit
        temp = T(Zb(i))
        pres = P(Zb(i))
        dens = rho(Zb(i))
        Write(*,'(A,F9.3,3ES14.6)') ACHAR(13),Zb(i),temp,pres,dens
        Write(unit,'(F9.3,3ES24.16)') Zb(i),temp,pres,dens
        i = i + 1
    Else
        j = j + 1
    End If
End Do
Close(unit)
Write(*,*)

End Program
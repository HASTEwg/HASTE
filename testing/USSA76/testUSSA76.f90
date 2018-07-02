Program testUSSA76

Use Kinds, Only: dp
Use US_Std_Atm_1976, Only: Zb
Use US_Std_Atm_1976, Only: T
Use US_Std_Atm_1976, Only: P
Use US_Std_Atm_1976, Only: rho
Use US_Std_Atm_1976, Only: nN2_power_stops
Use US_Std_Atm_1976, Only: nO1_O2_power_stops
Use US_Std_Atm_1976, Only: nAr_He_power_stops
Implicit None

Integer :: i,j,unit
Real(dp) :: ZxN2(1:5,1:2)
Real(dp) :: ZxO1O2(1:8,1:3)
Real(dp) :: ZxArHe(1:7,1:3)
Real(dp) :: z
Real(dp) :: temp,pres,dens

Real(dp), Parameter :: dZmax = 1.E-3_dp !1 meter resolution

Write(*,*)
Write(*,'(A)') 'N-density Integrand stops: N2'
Write(*,'(A9,A23)') ' Z [km] ','        x          '
Write(*,'(A9,A23)') '--------','-------------------'
ZxN2 = nN2_power_stops()
Do i = 2,5
    Write(*,'(F9.3,F23.16)') Zx(i,1),Zx(i,2)
End Do
STOP  !TEMPORARY STOP

Write(*,*)
Write(*,'(A9,3A14)') ' Z [km] ','   T [K]   ','   P [pa]   ',' rho [g/m^3]'
Write(*,'(A9,3A14)') '--------','-----------','------------','------------'
temp = T(0._dp)
pres = P(0._dp)
dens = rho(0._dp)
Write(*,'(F9.3,3ES14.6)') z,temp,pres,dens
Open(NEWUNIT=unit,FILE='TPRho.tst',ACTION='WRITE',STATUS='REPLACE')
Write(unit,'(F9.3,3ES24.16)') z,temp,pres,dens
i = 1
j = 1
Do
    z = Real(j,dp) * dZmax
    If (z .GT. MaxVal(Zb)) Exit
    temp = T(z)
    pres = P(z)
    dens = rho(z)
    Write(*,'(A,F9.3,3ES14.6)',ADVANCE='NO') ACHAR(13),z,temp,pres,dens
    Write(unit,'(F9.3,3ES24.16)') z,temp,pres,dens
    If (Real(j+1,dp)*dZmax .GE. Zb(i)) Then !the NEXT z will pass the next base layer
        If (Real(j+1,dp)*dZmax .NE. Zb(i)) Then !the NEXT z will get the base value
            temp = T(Zb(i))
            pres = P(Zb(i))
            dens = rho(Zb(i))
            Write(*,'(A,F9.3,3ES14.6)') ACHAR(13),Zb(i),temp,pres,dens
            Write(unit,'(F9.3,3ES24.16)') Zb(i),temp,pres,dens
        End If
        i = i + 1
    Else
        j = j + 1
    End If
End Do
Close(unit)
Write(*,*)

End Program

Program testUSSA76

Use Kinds, Only: dp
Use US_Std_Atm_1976, Only: Find_base_layer
Use US_Std_Atm_1976, Only: Zb
Use US_Std_Atm_1976, Only: T
Use US_Std_Atm_1976, Only: P
Use US_Std_Atm_1976, Only: rho
Use US_Std_Atm_1976, Only: rho_N
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
Real(dp) :: NumDens(1:6)

Real(dp), Parameter :: dZmax = 1.E-3_dp !1 meter resolution

Open(NEWUNIT=unit,FILE='IntegrandStops.tst',ACTION='WRITE',STATUS='REPLACE')
Write(*   ,*)
Write(unit,*)
Write(*   ,'(A)') 'N-density Integrand stops: N2'
Write(unit,'(A)') 'N-density Integrand stops: N2'
Write(*   ,'(A9,A23)') ' Z [km] ','        x          '
Write(unit,'(A9,A23)') ' Z [km] ','        x          '
Write(*   ,'(A9,A23)') '--------','-------------------'
Write(unit,'(A9,A23)') '--------','-------------------'
ZxN2 = nN2_power_stops()
Do i = 2,5
    Write(*   ,'(F9.3,F23.16)') ZxN2(i,1),ZxN2(i,2)
    Write(unit,'(F9.3,F23.16)') ZxN2(i,1),ZxN2(i,2)
End Do
Close(unit)
STOP  !TEMP STOP
Open(NEWUNIT=unit,FILE='IntegrandStops.tst',ACTION='WRITE',STATUS='OLD',POSITION='APPEND')
Write(*   ,*)
Write(unit,*)
Write(*   ,'(A)') 'N-density Integrand stops: O1 & O2'
Write(unit,'(A)') 'N-density Integrand stops: O1 & O2'
Write(*   ,'(A9,2A23)') ' Z [km] ','      x - O1       ','      x - O2       '
Write(unit,'(A9,2A23)') ' Z [km] ','      x - O1       ','      x - O2       '
Write(*   ,'(A9,2A23)') '--------','-------------------','-------------------'
Write(unit,'(A9,2A23)') '--------','-------------------','-------------------'
ZxO1O2 = nO1_O2_power_stops()
Do i = 2,8
    Write(*   ,'(F9.3,2F23.16)') ZxO1O2(i,1),ZxO1O2(i,2),ZxO1O2(i,3)
    Write(unit,'(F9.3,2F23.16)') ZxO1O2(i,1),ZxO1O2(i,2),ZxO1O2(i,3)
End Do
Close(unit)
STOP  !TEMP STOP
Open(NEWUNIT=unit,FILE='IntegrandStops.tst',ACTION='WRITE',STATUS='OLD',POSITION='APPEND')
Write(*   ,*)
Write(unit,*)
Write(*   ,'(A)') 'N-density Integrand stops: Ar & He'
Write(unit,'(A)') 'N-density Integrand stops: Ar & He'
Write(*   ,'(A9,2A23)') ' Z [km] ','      x - Ar       ','      x - He       '
Write(unit,'(A9,2A23)') ' Z [km] ','      x - Ar       ','      x - He       '
Write(*   ,'(A9,2A23)') '--------','-------------------','-------------------'
Write(unit,'(A9,2A23)') '--------','-------------------','-------------------'
ZxArHe = nAr_He_power_stops()
Do i = 2,7
    Write(*   ,'(F9.3,2F23.16)') ZxArHe(i,1),ZxArHe(i,2),ZxArHe(i,3)
    Write(unit,'(F9.3,2F23.16)') ZxArHe(i,1),ZxArHe(i,2),ZxArHe(i,3)
End Do
Close(unit)
STOP  !TEMP STOP

Write(*,*)
Write(*,'(A)') 'Temperature, Pressure, & Density as a function of altitude'
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
    If ( Any( Int(z/dZmax).EQ.(/250000,500000/) ) ) Then  !make 250km and 500km lines persistent
        Write(*,'(A,F9.3,3ES14.6)') ACHAR(13),z,temp,pres,dens
    Else
        Write(*,'(A,F9.3,3ES14.6)',ADVANCE='NO') ACHAR(13),z,temp,pres,dens
    End If
    Write(unit,'(F9.3,3ES24.16)') z,temp,pres,dens
    If (Real(j+1,dp)*dZmax .GE. Zb(i)) Then !the NEXT z will pass the next base layer
        If (Real(j+1,dp)*dZmax .NE. Zb(i)) Then !the NEXT z won't hit the next value
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

Write(*,*)
Write(*,'(A)') 'Number Density as a function of altitude'
Write(*,'(A9,6A10)') ' Z [km] ','   N2    ','   O1    ','   O2    ','   Ar    ','   He    ','   H1    '
Write(*,'(A9,6A10)') '--------','---------','---------','---------','---------','---------','---------'
z = 86000._dp
NumDens = 0._dp
Call rhoN(Z,T(z),Find_Base_Layer(z),NumDens(1:5))
Write(*,'(F9.3,6ES10.3)') z,NumDens
Open(NEWUNIT=unit,FILE='Ndens.tst',ACTION='WRITE',STATUS='REPLACE')
Write(unit,'(F9.3,6ES24.16)') z,NumDens
i = 8
j = 86001
Do
    z = Real(j,dp) * dZmax
    If (z .GT. MaxVal(Zb)) Exit
    Call rhoN(Z,T(z),Find_Base_Layer(z),NumDens(1:5))
    If ( Any( Int(z/dZmax).EQ.(/250000,500000/) ) ) Then  !make 250km and 500km lines persistent
        Write(*,'(A,F9.3,6ES10.3)') ACHAR(13),z,NumDens
    Else
        Write(*,'(A,F9.3,6ES10.3)',ADVANCE='NO') ACHAR(13),z,NumDens
    End If
    Write(unit,'(F9.3,6ES24.16)') z,NumDens
    If (Real(j+1,dp)*dZmax .GE. Zb(i)) Then !the NEXT z will pass the next base layer
        If (Real(j+1,dp)*dZmax .NE. Zb(i)) Then !the NEXT z won't hit the next value
            Call rhoN(Zb(i),T(Zb(i)),Find_Base_Layer(Zb(i)),NumDens(1:5))
            Write(*,'(A,F9.3,6ES10.3)') ACHAR(13),Zb(i),NumDens
            Write(unit,'(F9.3,6ES24.16)') Zb(i),NumDens
        End If
        i = i + 1
    Else
        j = j + 1
    End If
End Do
Close(unit)
Write(*,*)

End Program

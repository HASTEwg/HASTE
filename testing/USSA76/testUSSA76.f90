Program testUSSA76

Use Kinds, Only: dp
Use Kinds, Only: qp,sp,is,il,id
Use US_Std_Atm_1976, Only: Find_base_layer
Use US_Std_Atm_1976, Only: Zb
Use US_Std_Atm_1976, Only: T
Use US_Std_Atm_1976, Only: P
Use US_Std_Atm_1976, Only: rho
Use US_Std_Atm_1976, Only: rho_N
# if TEST_CODE
    Use US_Std_Atm_1976, Only: nN2_power_stops
    Use US_Std_Atm_1976, Only: nO1_O2_power_stops
    Use US_Std_Atm_1976, Only: nAr_He_power_stops
# endif

Implicit None

Integer :: i,j,unit
# if TEST_CODE
    Real(dp) :: ZxN2(1:5,1:2)
    Real(dp) :: ZxO1O2(1:8,1:3)
    Real(dp) :: ZxArHe(1:8,1:3)
# endif
Real(dp) :: z
Real(dp) :: temp,pres,dens
Real(dp) :: NumDens(1:6)
Integer, Parameter :: other_alts(1:12) = (/   5000, &
                                          &  15000, &
                                          &  25000, &
                                          &  40000, &
                                          &  49000, &
                                          &  60000, &
                                          &  80000, &
                                          &  88000, &
                                          & 100000, &
                                          & 117000, &
                                          & 250000, &
                                          & 500000  /)

Real(dp), Parameter :: dZmax = 1.E-3_dp !1 meter resolution

# if TEST_CODE
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
    Do i = 2,8
        Write(*   ,'(F9.3,2F23.16)') ZxArHe(i,1),ZxArHe(i,2),ZxArHe(i,3)
        Write(unit,'(F9.3,2F23.16)') ZxArHe(i,1),ZxArHe(i,2),ZxArHe(i,3)
    End Do
    Close(unit)
    STOP
# endif

z = 400._dp
Call rho_N(z,T(z),Find_Base_Layer(z),NumDens)
Write(*,'(F9.3,6ES10.3)') z,NumDens
z = 500._dp
Call rho_N(z,T(z),Find_Base_Layer(z),NumDens)
Write(*,'(F9.3,6ES10.3)') z,NumDens
z = 600._dp
Call rho_N(z,T(z),Find_Base_Layer(z),NumDens)
Write(*,'(F9.3,6ES10.3)') z,NumDens
STOP

Write(*,*)
Write(*,'(A)') 'Temperature, Pressure, & Density as a function of altitude'
Write(*,'(A9,3A14)') ' Z [km] ','   T [K]   ','   P [pa]   ',' rho [g/m^3]'
Write(*,'(A9,3A14)') '--------','-----------','------------','------------'
z = 0._dp
temp = T(z)
pres = P(z)
dens = rho(z)
Write(*,'(F9.3,3ES14.6,A)') z,temp,pres,dens,' <--Zb(0)'
Open(NEWUNIT=unit,FILE='TPRho.tst',ACTION='WRITE',STATUS='REPLACE')
Write(unit,'(F9.3,3ES24.16)') z,temp,pres,dens
i = 1
j = 1
Do j = 1,1000*1000
    z = Real(j,dp) * dZmax
    temp = T(z)
    pres = P(z)
    dens = rho(z)
    If ( Any(j .EQ. other_alts) ) Then  !make these lines persistent
        Write(*,'(A,F9.3,3ES14.6)') ACHAR(13),z,temp,pres,dens
    Else  !otherwise overprint
        Write(*,'(A,F9.3,3ES14.6)',ADVANCE='NO') ACHAR(13),z,temp,pres,dens
    End If
    If (z .GE. Zb(i)) Then !this z passed a base layer
        If (z .EQ. Zb(i)) Then !this z lands exaclty on the layer boundary, mark and prevent overprint
            Write(*,'(A,I0,A)') ' <--Zb(',i,')'
            i = i + 1
        Else !Compute the base layer values
            temp = T(Zb(i))
            pres = P(Zb(i))
            dens = rho(Zb(i))
            Write(*,'(A,F9.3,3ES14.6,A,I0,A)') ACHAR(13),Zb(i),temp,pres,dens,' <--Zb(',i,')'
            Write(unit,'(F9.3,3ES24.16)') Zb(i),temp,pres,dens
            i = i + 1
        End If
    End If
    Write(unit,'(F9.3,3ES24.16)') z,temp,pres,dens
End Do
Close(unit)
Write(*,*)

Write(*,*)
Write(*,'(A)') 'Number Density as a function of altitude'
Write(*,'(A9,6A10)') ' Z [km] ','   N2    ','   O1    ','   O2    ','   Ar    ','   He    ','   H1    '
Write(*,'(A9,6A10)') '--------','---------','---------','---------','---------','---------','---------'
z = 86._dp
NumDens = 0._dp
Call rho_N(Z,T(z),Find_Base_Layer(z),NumDens)
Write(*,'(F9.3,6ES10.3,A)') z,NumDens,' <--Zb(7)'
Open(NEWUNIT=unit,FILE='Ndens.tst',ACTION='WRITE',STATUS='REPLACE')
Write(unit,'(F9.3,6ES24.16)') z,NumDens
i = 8
Do j = 86001,1000*1000
    z = Real(j,dp) * dZmax
    Call rho_N(Z,T(z),Find_Base_Layer(z),NumDens)
    If ( Any(j .EQ. other_alts) ) Then  !make these lines persistent
        Write(*,'(A,F9.3,6ES10.3)') ACHAR(13),z,NumDens
    Else  !otherwise overprint
        Write(*,'(A,F9.3,6ES10.3)',ADVANCE='NO') ACHAR(13),z,NumDens
    End If
    If (z .GE. Zb(i)) Then !this z passed a base layer
        If (z .EQ. Zb(i)) Then !this z lands exaclty on the layer boundary, mark and prevent overprint
            Write(*,'(A,I0,A)') ' <--Zb(',i,')'
            i = i + 1
        Else !Compute the base layer values
            Call rho_N(Zb(i),T(Zb(i)),Find_Base_Layer(Zb(i)),NumDens)
            Write(*,'(A,F9.3,6ES10.3,A,I0,A)') ACHAR(13),Zb(i),NumDens,' <--Zb(',i,')'
            Write(unit,'(F9.3,6ES24.16)') Zb(i),NumDens
            i = i + 1
        End If
    End If
    Write(unit,'(F9.3,6ES24.16)') z,NumDens
End Do
Close(unit)
Write(*,*)

End Program

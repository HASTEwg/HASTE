Program LPadjoint

Use Kinds, Only: dp
Use Satellite_Motion, Only: Satellite_Position_Type
Use Satellite_Motion, Only: Initialize_Satellite_Motion
Use Random_Numbers, Only: RNG_Type
Use Detectors, Only: Grid_info_type
Use Detectors, Only: Define_Grid_Info
Use Neutron_Utilities, Only: Neutron_Speed
Use Neutron_Utilities, Only: Neutron_Energy
Use Find_Trajectory, Only: Prev_Event_Trajectory
Use Diverge_Approx, Only: Div_Fact_by_shooting
Use Neutron_Scatter, Only: Scattered_Angles
Use Random_Directions, Only: Isotropic_Omega_Hat
Use Global, Only: Pi,TwoPi,halfPi
Use Global, Only: r2deg,deg2r
Use Global, Only: R_center
Use Global, Only: Z_hat,X_hat,Y_hat
Use Utilities, Only: Unit_Vector
Use Utilities, Only: Cross_Product
Use Utilities, Only: Vector_Length
Use Statistics, Only: Std_Err
Use Statistics, Only: std_devs_for_95CI
Use FileIO_Utilities, Only: cr => creturn

Implicit None

Type(Satellite_Position_Type) :: sat
Type(RNG_Type) :: RNG
Integer, Parameter :: n_trials = 100000000
Integer, Parameter :: n_lat_bins = 36 , n_lon_bins = 72
Real(dp) :: E_tallies(1:n_lat_bins,1:n_lon_bins,1:2)
Real(dp) :: tof_tallies(1:n_lat_bins,1:n_lon_bins,1:2)
Real(dp) :: D_tallies(1:n_lat_bins,1:n_lon_bins,1:2)
Real(dp) :: fD_tallies(1:n_lat_bins,1:n_lon_bins,1:2)
Real(dp) :: fDt_tallies(1:n_lat_bins,1:n_lon_bins,1:2)
Real(dp) :: zeta_tallies(1:n_lat_bins,1:n_lon_bins,1:2)
Integer :: tally_ct(1:n_lat_bins,1:n_lon_bins)

Logical :: Gravity  !flag to set gravity on or off
Real(dp) :: t2  !time relative to epoch of intercept event
Real(dp) :: r_sat(1:3),v_sat(1:3)  !position and velocity of the satellite at t2
Real(dp) :: D_hat(1:3),N_hat(1:3),F_hat(1:3),U_hat(1:3),E_hat(1:3)  !basis vectors for satellite or emission frame of reference
Real(dp) :: u,w
Real(dp) :: Omega_hat2(1:3) !direction of neutron arrival in satellite frame
Integer, Parameter :: n_En = 15
Real(dp) :: En(1:n_En) ![keV] neutron energy at arrival in satellite frame
Logical :: Found  !flag for whether a trajectory was found
Real(dp) :: r1(1:3),v1(1:3),tof !position,velocity, time of flight defining flight from the surface of the central body
Real(dp) :: v2(1:3)
Real(dp) :: DFact !divergence factor for the intercepting flight from emission to intercept
Real(dp) :: DEC,HA
Integer :: dec_bin,ha_bin
Integer :: map_unit
Real(dp) :: t_min,t_max,t_res
Real(dp) :: E_min,E_max,E_res
Integer :: t_bins_per_decade,E_bins_per_decade
Real(dp) :: u_min,u_max,u_res
Real(dp) :: w_min,w_max,w_res
Integer :: u_bins_per_decade,w_bins_per_decade
Integer :: i,i_miss
Integer :: e,j
Character(2) :: e_char
Real(dp) :: f,Ee,fD,fDt,zeta
Real(dp) :: DFact_err,tof_err,Ee_err,fD_err,fDt_err,zeta_err
Real(dp) :: lat,lon
Character(2) :: n_En_char
# if CAF
Integer :: next_e[*]
Character(80) :: stat_lines(1:n_En)[*]
Character(80) :: new_stat_line
# endif

Gravity = .TRUE.
Call Initialize_Satellite_Motion('','Conic_tab ',sat)
t2 = 212640._dp  !This many seconds into mission start
!Satellite DNF basis vectors
Call sat%R_and_V(t2,r_sat,v_sat)
D_hat = -Unit_Vector(r_sat)
N_hat = Unit_Vector(Cross_Product(D_hat,v_sat))
F_hat = Cross_Product(N_hat,D_hat)
! Random Number Generator
# if CAF
Call RNG%Initialize(seed = 7777777 , thread = this_image())
# else
Call RNG%Initialize(seed = 7777777)
# endif
! Grid of energies at which to create maps
En = 1000._dp * (/ 1.e-8_dp,   & 
                 & 2.5e-8_dp,  & 
                 & 1.e-7_dp,   & 
                 & 2.e-7_dp,   & 
                 & 3.e-7_dp,   & 
                 & 4.e-7_dp,   & 
                 & 5.e-7_dp,   & 
                 & 6.e-7_dp,   & 
                 & 8.e-7_dp,   & 
                 & 1.e-6_dp,   & 
                 & 3.16e-6_dp, & 
                 & 1.e-5_dp,   & 
                 & 1.e-4_dp,   & 
                 & 1.e-3_dp,   & 
                 & 1.e-2_dp    /)
Write(n_En_char,'(I2)') n_En
# if CAF
Do e = 1,n_En
    Write(e_char,'(I2.2)') e
    stat_lines(e) = 'En '//e_char//'/'//n_En_char//'   *.**% (  *.**% hits) Total F: *.********E+***'
End Do
next_e = 1
Do
    CRITICAL
        e = next_e[1]
        next_e[1] = next_e[1] + 1
    END CRITICAL
    If (e .GT. n_En) Exit
# else
Do e = 1,n_En
# endif
    !initialize tally arrays for this energy
    tally_ct = 0
    fD_tallies = 0._dp
    fDt_tallies = 0._dp
    tof_tallies = 0._dp
    D_tallies = 0._dp
    E_tallies = 0._dp
    zeta_tallies = 0._dp
    !initialize output files for this energy
    Write(e_char,'(I2.2)') e
    Open(NEWUNIT = map_unit , FILE = 'LPemissionMap_e'//e_char//'.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Write(map_unit,'(17ES25.16E3)',ADVANCE='NO') En(e),t2,r_sat,v_sat,D_hat,N_hat,F_hat
    i = 0
    i_miss = 0
    Do
        !choose a random direction of arrival at the detector
        Omega_hat2 = Isotropic_Omega_Hat(RNG)
        !check if an emission is possible at the surface at this energy to result in this rendezvous
        Call Prev_Event_Trajectory(sat, Gravity, t2, -Omega_hat2*Neutron_Speed(En(e)), Found, r1, v1, tof)
        If (found) Then
            i = i + 1
            v2 = -Omega_hat2*Neutron_Speed(En(e)) + v_sat
            DFact = Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)
            !compute the declination and right-ascension indexes for this emission point
            U_hat = Unit_Vector(r1)
            DEC = ACOS( Dot_Product(Z_hat,U_hat) )
            HA = Atan2( Dot_Product(U_hat,X_hat) , Dot_Product(U_hat,Y_hat) )
            If (HA .LT. 0._dp) HA = HA + TwoPi
            dec_bin = 1 + Floor(Real(n_lat_bins,dp) * DEC / Pi)
            ha_bin = 1 + Floor(Real(n_lon_bins,dp) * HA / TwoPi)
            f = DFact
            Ee = Neutron_Energy(v1)
            zeta = Dot_Product(Unit_Vector(r1),Unit_Vector(v1))
            fD_tallies(dec_bin,ha_bin,1) = fD_tallies(dec_bin,ha_bin,1) + f
            fD_tallies(dec_bin,ha_bin,2) = fD_tallies(dec_bin,ha_bin,2) + f**2
            fDt_tallies(dec_bin,ha_bin,1) = fDt_tallies(dec_bin,ha_bin,1) + f*Exp(-tof/895._dp)
            fDt_tallies(dec_bin,ha_bin,2) = fDt_tallies(dec_bin,ha_bin,2) + (f*Exp(-tof/895._dp))**2
            tof_tallies(dec_bin,ha_bin,1) = tof_tallies(dec_bin,ha_bin,1) + f*tof
            tof_tallies(dec_bin,ha_bin,2) = tof_tallies(dec_bin,ha_bin,2) + (f*tof)**2
            E_tallies(dec_bin,ha_bin,1) = E_tallies(dec_bin,ha_bin,1) + f*Ee
            E_tallies(dec_bin,ha_bin,2) = E_tallies(dec_bin,ha_bin,2) + (f*Ee)**2
            zeta_tallies(dec_bin,ha_bin,1) = zeta_tallies(dec_bin,ha_bin,1) + f*zeta
            zeta_tallies(dec_bin,ha_bin,2) = zeta_tallies(dec_bin,ha_bin,2) + (f*zeta)**2
            D_tallies(dec_bin,ha_bin,1) = D_tallies(dec_bin,ha_bin,1) + DFact
            D_tallies(dec_bin,ha_bin,2) = D_tallies(dec_bin,ha_bin,2) + DFact**2
            tally_ct(dec_bin,ha_bin) = tally_ct(dec_bin,ha_bin) + 1
        Else
            i_miss = i_miss + 1
        End If
#       if CAF
        If (MOD(i,1000).EQ.0) Then
            Write(new_stat_line,'(A,I2,A,F6.2,A,F6.2,A,ES15.8E3)') & 
                                    & 'En ',e,'/'//n_En_char//' ',100._dp*Real(i,dp)/Real(n_trials,dp),'% (', &
                                    & 100._dp*Real(i,dp)/Real(i+i_miss,dp),'% hits) Total F: ',Sum(fD_tallies(:,:,1))
            If (this_image() .EQ. 1) Then
                stat_lines(e) = new_stat_line
                Do j = 1,n_En
                    Write(*,'(A)') stat_lines(j)
                End Do
                Write(*,'(A)',ADVANCE='NO') ACHAR(27)//'['//n_En_char//'F'
            Else
                stat_lines(e)[1] = new_stat_line
            End If
        End If
#       else
        If (MOD(i,1000).EQ.0) Write(*,'(A,I2,A,F6.2,A,F6.2,A,ES15.8E3,A)',ADVANCE='NO') & 
                                     & 'En ',e,'/'//n_En_char//' ',100._dp*Real(i,dp)/Real(n_trials,dp),'% (', &
                                     & 100._dp*Real(i,dp)/Real(i+i_miss,dp),'% hits) Total F: ',Sum(fD_tallies(:,:,1)),cr
#       endif
        If (i .GE. n_trials) Exit
    End Do
    Write(map_unit,'(ES25.16E3,2I12)') Sum(fD_tallies(:,:,1)),i,i_miss
    Do i = 1,n_lat_bins
        Do j = 1,n_lon_bins
            If (tally_ct(i,j) .GT. 0) Then
                DEC = Real(2*i-1,dp) * halfPi / Real(n_lat_bins,dp)
                HA = Real(2*j-1,dp) * Pi / Real(n_lon_bins,dp)
                r1 = Cos(DEC) * Z_hat + Sqrt(1._dp - Cos(DEC)**2) * (Cos(HA) * Y_hat + Sin(HA) * X_hat)
                fD = fD_tallies(i,j,1) / Sum(fD_tallies(:,:,1))
                fD_err = Std_err(Sum(fD_tallies(:,:,1)),fD_tallies(i,j,1),fD_tallies(i,j,2))
                fDt = fDt_tallies(i,j,1) / Sum(fD_tallies(:,:,1))
                fDt_err = Std_err(Sum(fD_tallies(:,:,1)),fDt_tallies(i,j,1),fDt_tallies(i,j,2))
                tof = tof_tallies(i,j,1) / fD_tallies(i,j,1)
                tof_err = Std_err(fD_tallies(i,j,1),tof_tallies(i,j,1),tof_tallies(i,j,2))
                Ee = E_tallies(i,j,1) / fD_tallies(i,j,1)
                Ee_err = Std_err(fD_tallies(i,j,1),E_tallies(i,j,1),E_tallies(i,j,2))
                zeta = zeta_tallies(i,j,1) / fD_tallies(i,j,1)
                zeta_err = Std_err(fD_tallies(i,j,1),zeta_tallies(i,j,1),zeta_tallies(i,j,2))
                DFact = D_tallies(i,j,1) / Real(tally_ct(i,j),dp)
                DFact_err = Std_err(tally_ct(i,j),D_tallies(i,j,1),D_tallies(i,j,2))
                lat = halfPi - DEC
                lon = -(HA - Pi) - halfPi
                If (Abs(lon) .GT. Pi) lon = lon + SIGN(TwoPi,-lon)
                Write(map_unit,'(2F8.2,15ES25.16E3,I12)') lat*r2deg,lon*r2deg, & 
                                                        & r1, & 
                                                        & fD,fD_err, & 
                                                        & fDt,fDt_err, & 
                                                        & tof,tof_err, & 
                                                        & Ee,Ee_err, & 
                                                        & zeta,zeta_err, & 
                                                        & DFact,DFact_err,tally_ct(i,j)
            End If
        End Do
    End Do
    Close(map_unit)
#   if CAF
#   else
    Write(*,*)
#   endif
End Do
# if CAF
SYNC ALL
If (this_image() .EQ. 1) Then
    Do i = 1,n_En+1
        Write(*,*)
    End Do
End If
# endif
End Program

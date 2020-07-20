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
Use Global, Only: r2deg
Use Global, Only: Z_hat,X_hat,Y_hat
Use Global, Only: n_life
Use Utilities, Only: Unit_Vector
Use Utilities, Only: Cross_Product
Use Utilities, Only: Vector_Length
Use Statistics, Only: Std_Err
Use FileIO_Utilities, Only: cr => creturn

Implicit None

Type(Satellite_Position_Type) :: sat
Type(RNG_Type) :: RNG
Integer, Parameter :: n_trials = 10000000
Integer, Parameter :: n_lat_bins = 36 , n_lon_bins = 72
Real(dp) :: Espec(0:6,1:n_lat_bins,1:n_lon_bins)
Real(dp) :: D_tallies(1:4,1:n_lat_bins,1:n_lon_bins)
Real(dp) :: tof_tallies(1:2,1:n_lat_bins,1:n_lon_bins)
Real(dp) :: E_tallies(1:4,1:n_lat_bins,1:n_lon_bins)
Real(dp) :: zeta_tallies(1:4,1:n_lat_bins,1:n_lon_bins)
Real(dp) :: det_tallies(1:4,1:n_lat_bins,1:n_lon_bins)
Integer :: tally_ct(1:n_lat_bins,1:n_lon_bins)
Logical :: Gravity  !flag to set gravity on or off
Real(dp) :: t2  !time relative to epoch of intercept event
Real(dp) :: r_sat(1:3),v_sat(1:3)  !position and velocity of the satellite at t2
Real(dp) :: D_hat(1:3),N_hat(1:3),F_hat(1:3),U_hat(1:3),E_hat(1:3)  !basis vectors for satellite or emission frame of reference
Real(dp) :: Omega_hat2(1:3) !direction of neutron arrival in satellite frame
Integer, Parameter :: n_En = 15
Real(dp) :: En(1:n_En) ![keV] neutron energy at arrival in satellite frame
Integer, Parameter :: n_nLifes = 15
Real(dp) :: nLifes(1:n_nLifes) ![s] neutron lifetimes for experiment
Real(dp) :: tF_tallies(1:2,1:n_nLifes,1:n_lat_bins,1:n_lon_bins)
Logical :: Found  !flag for whether a trajectory was found
Real(dp) :: r1(1:3),v1(1:3),tof !position,velocity, time of flight defining flight from the surface of the central body
Real(dp) :: v2(1:3)
Real(dp) :: DFact !divergence factor for the intercepting flight from emission to intercept
Real(dp) :: DEC,HA
Integer :: dec_bin,ha_bin
Integer :: map_unit
Integer :: i,i_miss
Integer :: e,j,k
Character(2) :: e_char
Real(dp) :: lnDfact,lnEfact,lnTfact,lnZfact
Real(dp) :: Di,Df,Ee,Ef,zeta,aF,tF(1:n_nLifes)
Real(dp) :: Di_err,Df_err,Ee_err,Ef_err,tof_err,tF_err(1:n_nLifes),zeta_err,aF_err
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
! List of energies at which to create maps
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
! List of neutron lifetimes at which to evaluate decay function
nLifes = n_Life + (/ -100._dp, & 
                   &  -50._dp, & 
                   &  -20._dp, & 
                   &  -10._dp, & 
                   &   -5._dp, & 
                   &   -2._dp, & 
                   &   -1._dp, & 
                   &    0._dp, & 
                   &    1._dp, & 
                   &    2._dp, & 
                   &    5._dp, & 
                   &   10._dp, & 
                   &   20._dp, & 
                   &   50._dp, & 
                   &  100._dp  /)
!Read in parameters of surface emission functions
!TEMPORARY:  Hard code to example function
Do i = 1,n_lat_bins
    Do j = 1,n_lon_bins
        Espec(0:6,i,j) = (/ 4.4598E6_dp ,                  &  !p0
                          & 3.6840E-9_dp ,                 &  !p1
                          & 4.4183E5_dp ,                  &  !p2
                          & 0.9181_dp ,                    &  !p3
                          & 1.4567E-7_dp ,                 &  !p4
                          & 5.0338_dp ,                    &  !p5
                          & 1._dp / 1.33446100936458714_dp /) !PDF normalization
    End Do
End Do

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
    D_tallies = 0._dp
    tof_tallies = 0._dp
    tF_tallies = 0._dp
    E_tallies = 0._dp
    zeta_tallies = 0._dp
    det_tallies = 0._dp
    !initialize output files for this energy
    Write(e_char,'(I2.2)') e
    Open(NEWUNIT = map_unit , FILE = 'LPemissionMap_e'//e_char//'.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Write( map_unit , '(17ES25.16E3)' , ADVANCE = 'NO' ) En(e),t2,r_sat,v_sat,D_hat,N_hat,F_hat
    i = 0
    i_miss = 0
    Do
        !choose a random direction of arrival at the detector
        Omega_hat2 = Isotropic_Omega_Hat(RNG)
        !check if an emission is possible at the surface at this energy to result in this rendezvous
        Call Prev_Event_Trajectory(sat, Gravity, t2, -Omega_hat2*Neutron_Speed(En(e)), Found, r1, v1, tof)
        If (found) Then
            i = i + 1
            !compute the declination and right-ascension indexes for this emission point
            U_hat = Unit_Vector(r1)
            DEC = ACOS( Dot_Product(Z_hat,U_hat) )
            HA = Atan2( Dot_Product(U_hat,X_hat) , Dot_Product(U_hat,Y_hat) )
            If (HA .LT. 0._dp) HA = HA + TwoPi
            dec_bin = 1 + Floor(Real(n_lat_bins,dp) * DEC / Pi)
            ha_bin = 1 + Floor(Real(n_lon_bins,dp) * HA / TwoPi)
            !Increment the tally counter for this declination-hourangle bin
            tally_ct(dec_bin,ha_bin) = tally_ct(dec_bin,ha_bin) + 1
            !Compute and tally divergence properties of the trajectory
            v2 = -Omega_hat2*Neutron_Speed(En(e)) + v_sat
            DFact = Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)
            lnDFact = Log(DFact)
            D_tallies(:,dec_bin,ha_bin) = D_tallies(:,dec_bin,ha_bin) + (/ DFact , DFact**2 , lnDFact , lnDFact**2 /)
            !Compute and tally flight time and decay properties of the trajectory
            tof_tallies(:,dec_bin,ha_bin) = tof_tallies(:,dec_bin,ha_bin) + (/ tof , tof**2 /)
            Do k = 1,n_nLifes
                !lnTFact = Log( Exp( -tof / nLifes(k) ) )
                lnTFact = -tof / nLifes(k)
                tF_tallies(:,k,dec_bin,ha_bin) = tF_tallies(:,k,dec_bin,ha_bin) + (/ lnTFact , lnTFact**2 /)
            End Do
            !Compute and tally emission energy properties of the trajectory
            Ee = Neutron_Energy(v1)
            lnEFact = Log(PDF_LunarAlbedo(Espec(:,dec_bin,ha_bin),Ee))
            E_tallies(:,dec_bin,ha_bin) = E_tallies(:,dec_bin,ha_bin) + (/ Ee , Ee**2 , lnEFact , lnEFact**2 /)
            !compute and tally emission direction properties of the trajectory
            zeta = Dot_Product(Unit_Vector(r1),Unit_Vector(v1))
            lnZFact = Log(PDF_power_cosine125(zeta))
            zeta_tallies(:,dec_bin,ha_bin) = zeta_tallies(:,dec_bin,ha_bin) + (/ zeta , zeta**2 , lnZFact , lnZFact**2 /)
            !Compute and tally detection sensitivity properties of the trajectory
            !UNDONE Need approach for computing "detector sensitivity factor" or some-such
            !det_tallies(:,dec_bin,ha_bin) = det_tallies(:,dec_bin,ha_bin) + (/ ??? , ???**2 , Log(???) , Log(???)**2 /)
        Else
            i_miss = i_miss + 1
        End If
#       if CAF
         If (MOD(i,1000).EQ.0) Then
            Write( new_stat_line,'(A,I2,A,F6.2,A,F6.2,A,ES16.8E3)' ) & 
                 & 'En ',e,'/'//n_En_char//' ',100._dp*Real(i,dp)/Real(n_trials,dp),'% (', &
                 & 100._dp*Real(i,dp)/Real(i+i_miss,dp),'% hits) Total F: ',Sum(D_tallies(1,:,:))
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
         If (MOD(i,1000).EQ.0) Write( * , '(A,I2,A,F6.2,A,F6.2,A,ES16.8E3,A)' , ADVANCE = 'NO' ) & 
                                   & 'En ',e,'/'//n_En_char//' ',100._dp*Real(i,dp)/Real(n_trials,dp),'% (', &
                                   & 100._dp*Real(i,dp)/Real(i+i_miss,dp),'% hits) Total F: ',Sum(D_tallies(1,:,:)),cr
#       endif
    If (i .GE. n_trials) Exit
    End Do
    Write(map_unit,'(ES25.16E3,2I12)') Sum(D_tallies(1,:,:)),i,i_miss
    Do i = 1,n_lat_bins
        Do j = 1,n_lon_bins
            If (tally_ct(i,j) .GT. 0) Then
                DEC = Real(2*i-1,dp) * halfPi / Real(n_lat_bins,dp)
                HA = Real(2*j-1,dp) * Pi / Real(n_lon_bins,dp)
                r1 = Cos(DEC) * Z_hat + Sqrt(1._dp - Cos(DEC)**2) * (Cos(HA) * Y_hat + Sin(HA) * X_hat)
                !intensity
                Di = D_tallies(1,i,j) / Real(i+i_miss,dp)
                Di_err = Std_err( i+i_miss , D_tallies(1,i,j) , D_tallies(2,i,j) )
                Df = Exp( D_tallies(3,i,j) / Real(i+i_miss,dp) )
                Df_err = Std_err( i+i_miss , D_tallies(3,i,j) , D_tallies(4,i,j) )
                Df_err = Exp(Df_err)
                !tof and decayed intensity
                tof = tof_tallies(1,i,j) / Real(tally_ct(i,j),dp)
                tof_err = Std_err( tally_ct(i,j) , tof_tallies(1,i,j) , tof_tallies(2,i,j) )
                Do k = 1,n_nLifes
                    tF(k) = Exp( tF_tallies(1,k,i,j) / Real(tally_ct(i,j),dp) )
                    tF_err(k) = Std_err( tally_ct(i,j) , tF_tallies(1,k,i,j) , tF_tallies(2,k,i,j) )
                End Do
                !emission energy intensity
                Ee = E_tallies(1,i,j) / Real(tally_ct(i,j),dp)
                Ee_err = Std_err( tally_ct(i,j) , E_tallies(1,i,j) , E_tallies(2,i,j) )
                Ef = Exp( E_tallies(3,i,j) / Real(tally_ct(i,j),dp) )
                Ef_err = Std_err( tally_ct(i,j) , E_tallies(3,i,j) , E_tallies(4,i,j) )
                !emission direction intensity
                zeta = zeta_tallies(1,i,j) / Real(tally_ct(i,j),dp)
                zeta_err = Std_err( tally_ct(i,j) , zeta_tallies(1,i,j) , zeta_tallies(2,i,j) )
                aF = Exp( zeta_tallies(3,i,j) / Real(tally_ct(i,j),dp) )
                aF_err = Std_err( tally_ct(i,j) , zeta_tallies(3,i,j) , zeta_tallies(4,i,j) )
                !detector sensitivity

                !Exponentiate error estimates for geometric means, catching overflow when number of counts is only 1
                If (tally_ct(i,j) .GT. 1) Then
                    tF_err = Exp(tF_err)
                    Ef_err = Exp(Ef_err)
                    aF_err = Exp(aF_err)
                Else
                    tF_err = -1._dp
                    Ef_err = -1._dp
                    aF_err = -1._dp
                End If
                !compute lat-lon and write each lat-lon bin to a line of output
                lat = halfPi - DEC
                lon = -(HA - Pi) - halfPi
                If (Abs(lon) .GT. Pi) lon = lon + SIGN(TwoPi,-lon)
                Write( map_unit,'(2F8.2,3ES25.16E3,I12,6ES25.16E3)' , ADVANCE = 'NO' ) &
                     & lat*r2deg , lon*r2deg , & 
                     & r1 , tally_ct(i,j) , & 
                     & Di , Di_err , Df , Df_err , & 
                     & tof , tof_err
                Do k = 1,n_nLifes
                    Write( map_unit,'(2ES25.16E3)' , ADVANCE = 'NO' ) &
                         & tF(k) , tF_err(k)
                End Do
                Write( map_unit,'(8ES25.16E3)' ) &
                     & Ee , Ee_err , Ef , Ef_err , & 
                     & zeta , zeta_err , aF , aF_err
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

Contains

Function PDF_LunarAlbedo(p,E) Result(d)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: d
    Real(dp), Intent(In) :: p(0:6)
    Real(dp), Intent(In) :: E
    
    d = p(6) * (p(0) * E * Exp(-E/p(1)) / p(1) + p(2) * ((E / p(4))**p(5)) * ((p(4) / E)**p(3)) / (1._dp + (E / p(4))**p(5)))
End Function PDF_LunarAlbedo

Function PDF_power_cosine125(mu) Result(p)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: mu
    Real(dp), Parameter :: invPDFnorm = 1._dp / 0.9308740569746155_dp

    p = (mu**1.25_dp) * invPDFnorm
End Function PDF_power_cosine125

End Program

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
Use Global, Only: n_life
Use Utilities, Only: Unit_Vector
Use Utilities, Only: Cross_Product
Use Utilities, Only: Vector_Length
Use Statistics, Only: Std_Err
Use FileIO_Utilities, Only: cr => creturn

Implicit None

Type(Satellite_Position_Type) :: sat
Type(RNG_Type) :: RNG
Integer, Parameter :: n_trials = 100000
Integer, Parameter :: n_lat_bins = 36 , n_lon_bins = 72

Logical :: Gravity  !flag to set gravity on or off
Real(dp) :: t2  !time relative to epoch of intercept event
Real(dp) :: r_sat(1:3),v_sat(1:3)  !position and velocity of the satellite at t2
Real(dp) :: Omega_hat2(1:3) !direction of neutron arrival in satellite frame
Integer, Parameter :: n_En = 15
Real(dp) :: En(1:n_En) ![keV] neutron energy at arrival in satellite frame
Logical :: Found  !flag for whether a trajectory was found
Real(dp) :: r1(1:3),v1(1:3),tof !position,velocity, time of flight defining flight from the surface of the central body
Real(dp) :: v2(1:3)
Real(dp) :: U_hat(1:3)
Real(dp) :: DFact !divergence factor for the intercepting flight from emission to intercept
Real(dp) :: DEC,HA
Integer :: dec_bin,ha_bin
Integer :: map_unit_f,map_unit_u
Integer :: i,i_miss
Integer :: e
Real(dp) :: Ee,zeta
Character(2) :: n_En_char
Character(8) :: t_char

Gravity = .TRUE.
Call Initialize_Satellite_Motion('','Conic_tab ',sat)
t2 = 212640._dp  !This many seconds into mission start
Write(t_char,'(I8.8)') FLOOR(t2)
!Satellite DNF basis vectors
Call sat%R_and_V(t2,r_sat,v_sat)
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
Open(NEWUNIT = map_unit_f , FILE = 'LPmap_t'//t_char//'.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
Open(NEWUNIT = map_unit_u , FILE = 'LPmap_t'//t_char//'.stream' , STATUS = 'REPLACE' , ACTION = 'WRITE' &
                                                               &, ACCESS = 'STREAM' , FORM = 'UNFORMATTED')
Write(map_unit_f,'(7ES25.16E3,I6)') t2,r_sat,v_sat,n_En
Write(map_unit_u) t2,r_sat,v_sat,n_En
Do e = 1,n_En
    Write(map_unit_f,'(ES25.16E3,I12)') En(e) / 1000._dp,n_trials
    Write(map_unit_u) En(e) / 1000._dp,n_trials
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
            Ee = Neutron_Energy(v1) / 1000._dp
            zeta = Dot_Product(Unit_Vector(r1),Unit_Vector(v1))
            !tally and write
            Write(map_unit_f,'(2I3,4ES25.16E3)') dec_bin,HA_bin,Ee,zeta,Dfact,tof
            Write(map_unit_u) dec_bin,HA_bin,Ee,zeta,Dfact,tof
        Else
            i_miss = i_miss + 1
        End If
        If (MOD(i,1000).EQ.0) Write(*,'(A,I2,A,F6.2,A,F6.2,A)',ADVANCE='NO') & 
                                     & 'En ',e,'/'//n_En_char//' ',100._dp*Real(i,dp)/Real(n_trials,dp),'% (', &
                                     & 100._dp*Real(i,dp)/Real(i+i_miss,dp),'% hits)'//cr
        If (i .GE. n_trials) Exit
    End Do
    Write(map_unit_f,'(2I12)') i,i_miss
    Write(map_unit_u) i,i_miss
    Write(*,*)
End Do
Close(map_unit_f)
Close(map_unit_u)

End Program

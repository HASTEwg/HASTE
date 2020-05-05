Program LPadjoint

Use Kinds, Only: dp
Use Satellite_Motion, Only: Satellite_Position_Type
Use Satellite_Motion, Only: Initialize_Satellite_Motion
Use Random_Numbers, Only: RNG_Type
Use Tallies, Only: Contrib_array
Use Tallies, Only: Setup_Tallies,Clear_Tallies
Use Tallies, Only: Contrib_triplet
Use Detectors, Only: Grid_info_type
Use Detectors, Only: Define_Grid_Info
Use Neutron_Utilities, Only: Neutron_Speed
Use Neutron_Utilities, Only: Neutron_Energy
Use Find_Trajectory, Only: Prev_Event_Trajectory
Use Diverge_Approx, Only: Div_Fact_by_shooting
Use Neutron_Scatter, Only: Scattered_Angles
Use Random_Directions, Only: Isotropic_Omega_Hat

Use Global, Only: Pi,TwoPi
Use Global, Only: R_center
Use Global, Only: Z_hat
Use Utilities, Only: Unit_Vector
Use Utilities, Only: Cross_Product
Use Utilities, Only: Vector_Length

Implicit None

Type(Satellite_Position_Type) :: sat
Type(RNG_Type) :: RNG
Type(Grid_info_type) :: TE_grid(1:2)
Type(Grid_info_type) :: Dir_grid(1:2)
Type(Contrib_array) :: TE_tallies(1:72,1:72)
Type(Contrib_array) :: Dir_tallies(1:72,1:72)
Integer :: tally_ct(1:72,1:72)
Type(Contrib_triplet) :: contrib(1:2)

Logical :: Gravity  !flag to set gravity on or off
Real(dp) :: t2  !time relative to epoch of intercept event
Real(dp) :: r_sat(1:3),v_sat(1:3)  !position and velocity of the satellite at t2
Real(dp) :: D_hat(1:3),N_hat(1:3),F_hat(1:3),U_hat(1:3),E_hat(1:3)  !basis vectors for satellite or emission frame of reference
Real(dp) :: u,w
Real(dp) :: Omega_hat2(1:3) !direction of neutron arrival in satellite frame
Real(dp) :: En(1:15) ![keV] neutron energy at arrival in satellite frame
Logical :: Found  !flag for whether a trajectory was found
Real(dp) :: r1(1:3),v1(1:3),tof !position,velocity, time of flight defining flight from the surface of the central body
Real(dp) :: v2(1:3)
Real(dp) :: DFact !divergence factor for the intercepting flight from emission to intercept
Real(dp) :: DEC,RA
Integer :: dec_bin,ra_bin
Integer :: map_unit
Real(dp) :: t_min,t_max,t_res
Real(dp) :: E_min,E_max,E_res
Integer :: t_bins_per_decade,E_bins_per_decade
Real(dp) :: u_min,u_max,u_res
Real(dp) :: w_min,w_max,w_res
Integer :: u_bins_per_decade,w_bins_per_decade
Integer :: e,i,j
Integer :: n_trials
Real(dp) :: wt
Character(2) :: e_char

Write(*,*)

Gravity = .TRUE.
Call Initialize_Satellite_Motion('','Conic_tab ',sat)
t2 = 212640._dp  !This many seconds into mission start
!Satellite DNF basis vectors
Call sat%R_and_V(t2,r_sat,v_sat)
D_hat = -Unit_Vector(r_sat)
N_hat = Unit_Vector(Cross_Product(D_hat,v_sat))
F_hat = Cross_Product(N_hat,D_hat)
! Random Number Generator
Call RNG%Initialize(seed = 7777777)
! Define time, energy, and direction grids in which to tally emissions
t_min = 0.1_dp
t_max = 1.E6_dp
t_res = 1._dp
t_bins_per_decade = 10
TE_grid(1) = Define_Grid_Info(.TRUE.,t_min,t_max,t_res,t_bins_per_decade)
E_min = 1.E-5_dp
E_max = 1.E6_dp
E_res = 1._dp
E_bins_per_decade = 100
TE_grid(2) = Define_Grid_Info(.TRUE.,E_min,E_max,E_res,E_bins_per_decade)
u_min = -1._dp
u_max = 1._dp
u_res = 2._dp / 263._dp
u_bins_per_decade = 1
Dir_grid(1) = Define_Grid_Info(.FALSE.,u_min,u_max,u_res,u_bins_per_decade)
w_min = -Pi
w_max = Pi
w_res = Pi / 72._dp
w_bins_per_decade = 1
Dir_grid(2) = Define_Grid_Info(.FALSE.,w_min,w_max,w_res,w_bins_per_decade)
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
!define an arbitrary starting weight for particels (for better normalization)
wt = 1._dp / (Vector_Length(r_sat) - R_center)**2
!define an arbitrary nuber of trials
n_trials = 1000
Do e = 1,15
    !initialize tally arrays for this energy
    tally_ct = 0
    Do CONCURRENT (i=1:72 , j=1:72)
        Call Clear_Tallies(TE_tallies(i,j))
        TE_tallies(i,j) = Setup_Tallies(TE_grid(1)%n_bins,TE_grid(2)%n_bins)
        Call Clear_Tallies(Dir_tallies(i,j))
        Dir_tallies(i,j) = Setup_Tallies(Dir_grid(1)%n_bins,Dir_grid(2)%n_bins)
    End Do
    !initialize output files for this energy
    Write(e_char,'(I2.2)') e
    Open(NEWUNIT = map_unit , FILE = 'LPemissionMap_e'//e_char//'.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Write(map_unit,'(18ES25.16E3)') En(e),t2,r_sat,v_sat,D_hat,N_hat,F_hat,wt
    Do i = 1,n_trials
        !choose a random direction of arrival at the detector
        Omega_hat2 = Isotropic_Omega_Hat(RNG)
        !check if an emission is possible at the surface at this energy to result in this rendezvous
        Call Prev_Event_Trajectory(sat, Gravity, t2, -Omega_hat2*Neutron_Speed(En(e)), Found, r1, v1, tof)
        If (found) Then
            v2 = -Omega_hat2*Neutron_Speed(En(e)) + v_sat
            DFact = Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)
            !compute the declination and right-ascension indexes for this emission point
            U_hat = Unit_Vector(r1)
            DEC = ACOS(U_hat(3))
            dec_bin = 1 + Floor(72._dp * DEC / Pi)
            If (dec_bin.EQ.1 .OR. dec_bin.EQ.72) Then
                ra_bin = 1
            Else
                RA = ACOS(U_hat(1) / Sqrt(1._dp - U_hat(3)**2))
                ra_bin = 1 + Floor(72._dp * RA / TwoPi)
            End If
            !compute the TE and direction indexes for the contribution
            contrib(1)%i1 = TE_grid(1)%Bin_Number(tof)
            contrib(1)%i2 = TE_grid(2)%Bin_Number(Neutron_Energy(v1))
            E_hat = Cross_Product(Z_hat,U_hat)
            N_hat = Cross_Product(U_hat,E_hat)
            Call Scattered_Angles(E_hat,Unit_Vector(v1),u,w,N_hat,U_hat)
            contrib(2)%i1 = Dir_grid(1)%Bin_Number(u)
            contrib(2)%i2 = Dir_grid(2)%Bin_Number(w)
            contrib(:)%f = wt / DFact
            !tally the contribution to this DEC-RA bin
            Call TE_tallies(dec_bin,ra_bin)%Tally_1_History(contrib(1))
            Call Dir_tallies(dec_bin,ra_bin)%Tally_1_History(contrib(2))
            tally_ct(dec_bin,ra_bin) = tally_ct(dec_bin,ra_bin) + 1
        End If

    End Do
    Close(map_unit)
End Do

End Program

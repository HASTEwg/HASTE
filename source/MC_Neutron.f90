Module MC_Neutron

    Implicit None
    Private
    Public :: Do_Neutron
    
Contains

Subroutine Do_Neutron(s,d,atm,ScatMod,RNG,contributed)
    Use Kinds, Only: dp
    Use Sources, Only: Source_Type
    Use Detectors, Only: Detector_Type
    Use Atmospheres, Only: Atmosphere_Type
    Use Random_Numbers, Only: RNG_Type
    Use Neutron_Scatter, Only: Neutron_Type
    Use Neutron_Scatter, Only: Scatter_Model_Type
    Implicit None
    Type(Source_Type), Intent(InOut) :: s
    Type(Detector_Type), Intent(InOut) :: d
    Type(Atmosphere_Type), Intent(In) :: atm
    Type(Scatter_Model_Type), Intent(InOut) :: ScatMod
    Type(RNG_Type), Intent(InOut) :: RNG
    Logical, Intent(Out) :: contributed
    Type(Neutron_Type) :: n
    Logical :: leaked,absorbed  !indicates kill criteria met for leakage or absorption
    Integer :: scatter  !loop counter
        
    !Start a new neutron
    n = Start_Neutron(s,atm,RNG,ScatMod,d,atm)
    If (ScatMod%direct_contribution) Then
        Call First_Event_Neutron(n,ScatMod,s,d,atm)
        If (ScatMod%n_scatters .EQ. 0) Then
            If (d%TE_contrib_index.GT.0 .OR. d%Dir_contrib_index.GT.0) Then
                contributed = .TRUE.
            Else
                contributed = .FALSE.
            End If
            ScatMod%n_kills(6) = ScatMod%n_kills(6) + 1
            Return
        End If
    End If
    scatter = 1
    !Transport the neutron for the desired number of scatters
    Do  !Exiting this loop kills the neutron
        !Move the neutron to the site of next collision and check for leakage
        Call Move_Neutron(n,ScatMod,atm,RNG,leaked)
        If (leaked) Then  !kill for leakage
            ScatMod%n_kills(4) = ScatMod%n_kills(4) + 1
            Exit
        End If
        !Choose scatter parameters for next scatter
        Call ScatMod%Sample_Scatter(n,atm,RNG)
        If (scatter .EQ. ScatMod%n_scatters) Then  !neutron has reached final simulation position, scatter to detector and exit
            ScatMod%n_kills(6) = ScatMod%n_kills(6) + 1
            Call Next_Event_Neutron(n,ScatMod,d,atm,RNG,s%w)
            Exit
        Else If (ScatMod%estimate_each_scatter) Then  !Scatter to detector and continue
            Call Next_Event_Neutron(n,ScatMod,d,atm,RNG,s%w)
        End If
        !Scatter into new random direction and check for absorption
        Call Scatter_Neutron(n,ScatMod,RNG,absorbed)
        If (absorbed) Then  !kill for absorption
            ScatMod%n_kills(5) = ScatMod%n_kills(5) + 1
            Exit
        End If
        If (ScatMod%n_scatters .EQ. -1) Then  !check kill criteria
            If (Kill_Neutron(n%t,n%E,d%TE_Grid(1)%max,d%TE_grid(2)%min,ScatMod%n_kills(1:2))) Exit
            If (ScatMod%roulette) Then
                If (Roulette_Neutron(n%weight,ScatMod%roulette_weight,ScatMod%roulette_rate,ScatMod%roulette_mult,ScatMod%n_kills(3),RNG)) Exit
            End If
        Else  !increment the scatter count and continue
            scatter = scatter + 1
        End If
    End Do
    If (d%TE_contrib_index.GT.0 .OR. d%Dir_contrib_index.GT.0) Then
        contributed = .TRUE.
        !reduce the list of tallies for this history, sorting in time and energy and combining duplicates
        Call d%Combine_Duplicate_Tallies()
    Else
        contributed = .FALSE.
    End If
End Subroutine Do_Neutron

Function Start_Neutron(source,atm,RNG,ScatMod,detector) Result(n)
    Use Kinds, Only: dp
    Use Neutron_Scatter, Only: Neutron_Type
    Use Neutron_Scatter, Only: Scatter_Model_Type
    Use Neutron_Utilities, Only: Neutron_Speed
    Use Detectors, Only: Detector_Type
    Use Sources, Only: Source_Type
    Use Atmospheres, Only: Atmosphere_Type
    Use Random_Numbers, Only: RNG_Type
    Use Global, Only: R_Earth
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Unit_Vector
    Use Utilities, Only: Smaller_Quadratic_Root
    Use Pathlengths, Only: R_close_approach
    Implicit None
    Type(Neutron_Type) :: n
    Type(Source_Type), Intent(InOut) :: source
    Type(Atmosphere_Type), Intent(In) :: atm
    Type(RNG_Type), Intent(InOut) :: RNG
    Type(Scatter_Model_Type), Intent(InOut) :: ScatMod
    Type(Detector_Type), Intent(InOut) :: detector
    Real(dp) :: zeta
    Real(dp) :: dZ
    Real(dp) :: d_to_atm
    Real(dp) :: v(1:3),s
    Real(dp) :: xi
    Real(dp) :: tof,t_min,t_max

    n%t = 0._dp  !Initialize simulation time
    n%r = source%r  !Initial position is at neutron source
    Call New_Energy_Direction_in_SF_and_ECI()
    If (source%exoatmospheric) Then  !check to see if this neutron is headed for the atmosphere
        If (ScatMod%gravity) Then
            Do
                zeta = Dot_Product(Unit_Vector(n%r),n%Omega_hat)
                s = Neutron_Speed(n%E)
                v = s * n%Omega_hat
                xi = SME(n%r,v)
                If (zeta.LT.0._dp .OR. xi.LT.0._dp) Then !downward path or return trajectory, the path might intersect the atmosphere
                    rp = Radius_of_Perigee(n%r,v)
                    If (rp .LT. atm%R_top) Then  !this path intersects the atmosphere
                        !find flight time to atmosphere interface
                        dZ = atm%z_top - source%z
                        t_min = dZ / (s + Sqrt(2._dp*mu*(1._dp/source%big_r - 1._dp/(source%big_r+dZ))))
                        t_max = Time_since_periapsis(n%r,v)
                        If (xi .LT. 0._dp) t_max = TwoPi * Sqrt((-0.5_dp * mu / xi)**3 / mu) - t_max
                        tof = Time_to_R(n%r,v,atm%R_top,t_min,t_max)
                        !adjust starting time, position, and direction to state at atmospheric interface
                        Call Kepler_Gooding(n%r,v,tof,n%r,v)
                        n%E = Neutron_Energy(v)
                        n%Omega_hat = Unit_Vector(v)
                        n%t = n%t + tof
                        Exit
                    End If
                End If
                !upward/flat path or not low enough Z_closest_approach, this path will not intersect atmosphere, reject it
                ScatMod%n_uncounted = ScatMod%n_uncounted + 1
                !If direct contributions are being included, tally one before rejecting it
                If (ScatMod%direct_contribution) Call First_Event_Neutron(n,ScatMod,source,detector,atm)
                !draw a new random energy and direction
                Call New_Energy_Direction_in_SF_and_ECI()
            End Do
        Else
            Do
                zeta = Dot_Product(Unit_Vector(n%r),n%Omega_hat)
                If (zeta .LT. 0._dp) Then !downward path, the path might intersect the atmosphere
                    If (R_close_approach(source%big_r,zeta) .LT. atm%R_top) Then  !this path intersects the atmosphere
                        dZ = atm%z_top - source%z
                        d_to_atm = Smaller_Quadratic_root(zeta*source%big_r,dZ * (2._dp*source%big_r + dZ))
                        !adjust starting position to atmosphere interface
                        n%r = n%r + d_to_atm * n%Omega_hat
                        !adjust starting time for time of flight from source to atmospheric interface
                        n%t = n%t + d_to_atm / Neutron_Speed(n%E)
                        Exit
                    End If
                End If
                !upward/flat path or not low enough Z_closest_approach, this path will not intersect atmosphere, reject it
                ScatMod%n_uncounted = ScatMod%n_uncounted + 1
                !If direct contributions are being included, tally one before rejecting it
                If (ScatMod%direct_contribution) Call First_Event_Neutron(n,ScatMod,source,detector,atm)
                !draw a new random energy and direction
                Call New_Energy_Direction_in_SF_and_ECI()
            End Do
        End If
    End If
    !update z,zeta,big_r for initial position
    n%zeta = Dot_Product(Unit_Vector(n%r),n%Omega_hat)
    n%big_r = Vector_Length(n%r)
    n%Z = n%big_r - R_Earth
Contains
    Subroutine New_Energy_Direction_in_SF_and_ECI()
        Use Neutron_Utilities, Only: Neutron_Energy
        Implicit None
        Real(dp) :: v(1:3)
        
        Call source%Sample_EandD(RNG,n%E0ef,n%Omega_hat,n%weight)  !Initialize energy and weight
        n%s0ef = Neutron_Speed(n%E0ef)
        If (source%has_velocity) Then  !move energy & direction into ECI frame
            v = n%Omega_hat * n%s0ef + source%v
            n%E = Neutron_Energy(v)
            n%Omega_hat = Unit_Vector(v)
        Else  !otherwise, source frame is ECI frame
            n%E = n%E0ef
        End If
    End Subroutine New_Energy_Direction_in_SF_and_ECI
End Function Start_Neutron

Subroutine First_Event_Neutron(n,ScatMod,s,d,atm)
    Use Kinds, Only: dp
    Use Global, Only: inv_FourPi
    Use Global, Only: mfp_per_barn_per_km_at_seaLevel
    Use Global, Only: std_grav_parameter
    Use Global, Only: R_earth
    Use Global, Only: n_lambda
    Use Global, Only: mu => std_grav_parameter
    Use Detectors, Only: Detector_Type
    Use Sources, Only: Source_Type
    Use Atmospheres, Only: Atmosphere_Type
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Unit_Vector
    Use Pathlengths, Only: R_close_approach
    Use Pathlengths, Only: EPL_upward
    Use Pathlengths, Only: Next_Event_L_to_edge
    Use Neutron_Scatter, Only: Neutron_Type
    Use Neutron_Scatter, Only: Scatter_Model_Type
    Use Neutron_Scatter, Only: Scattered_Angles
    Use Neutron_Utilities, Only: Neutron_Energy
    Use Divergence, Only: Div_Fact_by_shooting
    Use Divergence, Only: Div_Fact_Straight
    Use Find_Trajectory, Only: Next_Event_Trajectory
    Use Astro_Utilities, Only: SME
    Implicit None
    Type(Neutron_Type), Intent(In) :: n
    Type(Scatter_Model_Type), Intent(InOut) :: ScatMod
    Type(Source_Type), Intent(InOut) :: s
    Type(Detector_Type), Intent(InOut) :: d
    Type(Atmosphere_Type), Intent(In) :: atm
    Real(dp) :: mu0ef  !cosine of polar scatter angle for scatter in EF frame
    Real(dp) :: omega0ef  !rotational scatter angle for scatter in EF frame
    Real(dp) :: Omega_hat1_ef(1:3)  ![1 km/s]  velocity unit vector in CM frameafter collision
    Real(dp) :: v1(1:3)  ![km/s] velocity vector of neutron after collision
    Real(dp) :: v1ef(1:3)  ![km/s] velocity vector of neutron after collision in EF frame
    Real(dp) :: sigma_T1  ![km^-1]  total microscopic (seal-level) cross section after scatter
    Real(dp) :: zeta1  !cosine of the zenith angle (angle from verticle to Omega_hat1)
    Real(dp) :: xEff_top_atm  ![km] effective seal-level path length along velocity vector to distance d_top_atm
    Real(dp) :: Bn  !used to compute dmu0cm_dmu0
    Real(dp) :: dmu0ef_dmu0  !conversion from angular pdf in CM fram to lab frame
    Real(dp) :: w2  !modified weight for forced scatter to detector
    Real(dp) :: tof  !time of flight to detector, simulation time of this scatter plus time of flight from current position to the detector
    Real(dp) :: dt  ![s]  flight time from scatter to satellite
    Logical :: path_found  !indicates whether path to satellite was found
    Real(dp) :: v2sat(1:3)  ![km/s]  velocity vector of neutron at arrival to satellite in satellite frame
    Real(dp) :: Omega_hat2_sat(1:3)  ![1 km/s]  unit velocity vector of neutron at arrival to satellite in satellite frame
    Real(dp) :: E2sat  ![keV]  Energy of neutron at arrival to satellite in satellite frame
    Real(dp) :: rS2(1:3)  ![km]  position of the satellite at arrival of neutron
    Real(dp) :: vS2(1:3)  ![km/s]  velocity of satellite at arrival of neutron
    Real(dp) :: divEF  !divergence factor
    Logical :: no_LOS  !flag indicates path to detector intersects Earth
    Real(dp) :: r_ca
    Real(dp) :: h,xi,p,e,s1
        
    ScatMod%next_events(1) = ScatMod%next_events(1) + 1
    If (ScatMod%Gravity) Then !check if energy is adequate to reach detector
        If (SME(s%big_r,n%s0ef+s%speed) .LT. 0._dp) Then !neutron's max velocity is less than escape velocity, check if satellite altitude is achievable
            If (-2._dp * s%big_r * std_grav_parameter / ( (n%s0ef + s%speed)**2 * s%big_r - 2._dp * std_grav_parameter ) .LT. d%sat%rp) Return  !neutron max height is insufficent to reach satellite at its lowest point
            !A more robust check for trajectories that can meet the satellite is performed later, but this saves computation time if the max achievable height is insufficent
        End If
    End If
    Call Next_Event_Trajectory(d%sat,ScatMod%Gravity,s%r,n%t,n%s0ef,s%v,path_found,rS2,dt,v1ef,v2sat,vS2)
    If (.NOT. path_found) Return
    !Check for line of sight, compute EPL to detector as side effect
    v1 = v1ef + s%v
    s1 = Vector_Length(v1)
    zeta1 = Dot_Product(Unit_Vector(s%r),Unit_Vector(v1))
    If (s%exoatmospheric) Then  !emission point is above the atmosphere
        !check for LOS above atm and through atm, if through atm compute distance along the path as a side-effect
        xEff_top_atm = 0._dp
        no_LOS = .FALSE.
        If (zeta1 .LT. 0._dp) Then  !direction is downward
            If (ScatMod%Gravity) Then
                h = s%big_r * s1 * Sqrt(1._dp - zeta1**2)
                xi = 0.5_dp * s1**2 - mu / s%big_r
                p = h*h / mu
                e = Sqrt(1._dp + 2._dp * xi * p / mu)
                r_ca = p / (1._dp + e)
                If (r_ca .LT. atm%R_top) Then !passes through atmosphere
                    !add downward and upward segments
                    Call EPL_upward(atm,r_ca-R_earth,0._dp,h,xi,p,e,r_ca,xEff_top_atm)
                    xEff_top_atm = 2._dp * xEff_top_atm
                End If
            Else
                r_ca = R_close_approach(s%big_r,zeta1)
                If (r_ca .LT. atm%R_bot) Then  !downward to earth
                    no_LOS = .TRUE.  !set flag
                Else If (r_ca .LT. atm%R_top) Then !path cuts through atmosphere
                    !add downward and upward segments
                    Call EPL_upward(atm,r_ca,0._dp,r_ca-R_Earth,xEff_top_atm)
                    xEff_top_atm = 2._dp * xEff_top_atm
                End If
            End If
        End If
    Else  !emission point is in the atmosphere
        !check for LOS, compute distance along the path as a side-effect
        If (ScatMod%Gravity) Then  !LOS is not actually checked in the gravity case, the solver does not return trajectories without line of sight
            no_LOS = Next_Event_L_to_edge(atm,s%big_r,s1,s%Z,zeta1,xEff_top_atm)
        Else
            no_LOS = Next_Event_L_to_edge(atm,s%big_r,s%Z,zeta1,xEff_top_atm)
            If (no_LOS) Return
        End If
    End If
    ScatMod%next_events(2) = ScatMod%next_events(2) + 1
    !TOTAL TIME OF FLIGHT TO ARRIVAL
    tof = n%t + dt
    !ENERGY AT ARRIVAL
    E2sat = Neutron_Energy(v2sat)
    !Check if time and energy are in range of the detector grid
    If ( E2sat .LT. d%TE_grid(2)%min .OR. &
       & E2sat .GT. d%TE_grid(2)%max .OR. &
       & tof   .LT. d%TE_grid(1)%min .OR. &
       & tof   .GT. d%TE_grid(1)%max      ) Then !No contribution from this neutron due to out of range of detector grid
        !Record reason for no contribution and return
        If (E2sat.LT.d%TE_grid(2)%min .OR. E2sat.GT.d%TE_grid(2)%max) Then  !E out of range, check for time range
            If (tof.LT.d%TE_grid(1)%min .OR. tof.GT.d%TE_grid(1)%max) Then  !t also out of range
                ScatMod%n_no_tally(1) = ScatMod%n_no_tally(1) + 1
            Else  !just E out of range
                ScatMod%n_no_tally(3) = ScatMod%n_no_tally(3) + 1
            End If
        Else If (tof.LT.d%TE_grid(1)%min .OR. tof.GT.d%TE_grid(1)%max) Then  !just t out of range
            ScatMod%n_no_tally(2) = ScatMod%n_no_tally(2) + 1
        End If
        Return
    Else  !contribution hits the grid
        ScatMod%next_events(3) = ScatMod%next_events(3) + 1
    End If
    !DIRECTION AT ARRIVAL
    Omega_hat2_sat = Unit_Vector(v2sat)
    !WEIGHT ADJUSTMENTS... Scattered angle, divergence, absorption & scatter suppression, decay
    w2 = n%weight !initial value (neutron weight at source)
    !Adjust for emission angle
    Omega_hat1_ef = Unit_Vector(v1ef)
    If (s%has_velocity) Then
        Call Scattered_Angles(s%A_hat,Omega_hat1_ef,mu0ef,omega0ef,s%B_hat,s%C_hat)
        Bn = n%s0ef / s%speed
        dmu0ef_dmu0 = Abs((1._dp + Bn * (2._dp * mu0ef + Bn))**(1.5_dp) / (Bn**2 * (Bn + mu0ef)))  !Eqn 479 from Haste-N removed material (p 39)
    Else
        dmu0ef_dmu0 = 1._dp
    End If
    w2 = w2 * dmu0ef_dmu0 * inv_FourPi 
    !Adjust for divergence
    If (ScatMod%Gravity) Then
        divEF = Div_Fact_by_shooting(s%r,Omega_hat1_ef,n%s0ef,s%v,tof,vS2,v2sat+vS2)
    Else
        divEF = Div_Fact_Straight(s%r,v1,rS2,s%v,tof,vS2)
    End If
    w2 = w2 * divEF
    !Adjust for interaction suppression on the path out of the atmosphere
    If (xEff_top_atm .GT. 0._dp) Then
        If (s%exoatmospheric) Then
            If (ScatMod%Doppler_Broaden) Then
                sigma_T1 = ScatMod%CS%sig_T_broad(Neutron_Energy(v1),atm%T(r_ca-R_earth))
            Else
                sigma_T1 = ScatMod%CS%sig_T(Neutron_Energy(v1))
            End If
        Else
            If (ScatMod%Doppler_Broaden) Then
                sigma_T1 = ScatMod%CS%sig_T_broad(Neutron_Energy(v1),atm%T(s%Z))
            Else
                sigma_T1 = ScatMod%CS%sig_T(Neutron_Energy(v1))
            End If
        End If
        w2 = w2 * Exp(-sigma_T1 * mfp_per_barn_per_km_at_seaLevel * xEff_top_atm)
    End If
    !Adjust for free neutron decay
    If (ScatMod%neutron_decay) w2 = w2 * Exp(-n_lambda * tof)
    !TALLY
    Call d%Tally_Scatter(E2sat,Omega_hat2_sat,tof,w2)
End Subroutine First_Event_Neutron

Subroutine Move_Neutron(n,ScatMod,atm,RNG,leaked)
    Use Kinds, Only: dp
    Use Global, Only: R_Earth
    Use Global, Only: mfp_per_barn_per_km_at_seaLevel
    Use Neutron_Scatter, Only: Neutron_Type
    Use Neutron_Scatter, Only: Scatter_Model_Type
    Use Neutron_Utilities, Only: Neutron_Speed
    Use Atmospheres, Only: Atmosphere_Type
    Use Atmospheres, Only: rho_SL
    Use Random_Numbers, Only: RNG_Type
    Use Global, Only: R_Earth
    Use Utilities, Only: Unit_Vector
    Use Utilities, Only: Vector_Length
    Use Pathlengths, Only: S_and_L_to_edge
    Use Pathlengths, Only: L_to_S
    Use Pathlengths, Only: EPL
    Use Pathlengths, Only: deltaZ
    Use Neutron_Scatter, Only: Air_Velocity
    Use Neutron_Scatter, Only: Apparent_Energy
    Implicit None
    Type(Neutron_Type), Intent(InOut) :: n
    Type(Scatter_Model_Type), Intent(In) :: ScatMod
    Type(Atmosphere_Type), Intent(In) :: atm
    Type(RNG_Type), Intent(InOut) :: RNG
    Logical, Intent(Out) :: leaked
    Real(dp) :: E_apparent,sigma_T
    Real(dp) :: d_to_leak  ![km] distance to leakage, maximum possible path length in scattering medium
    Real(dp) :: d  ![km] distance to next collision
    Real(dp) :: xEff_to_leak  ![km] effective seal-level path length along velocity vector to distance d_to_leak
    Real(dp) :: P_not_leak  !probability of NOT leaking from the system
    Real(dp) :: tau  !optical thickness (sampled) to point of next scatter
    Real(dp) :: xi  !random number, used if leakage is not suppressed to compute tau
    Real(dp) :: xEff  ![km] effective path length to next scatter
    Integer :: nb
    Integer, Allocatable :: bb(:)
    Real(dp), Allocatable :: Lb(:)
    Logical, Allocatable :: db(:)

    !Get total cross section upon which to base distance to next scatter
    If (ScatMod%Rotating_Earth .OR. ScatMod%Wind) Then
        E_apparent = Apparent_Energy( n%E, n%Omega_hat, Air_Velocity(ScatMod%Rotating_Earth,ScatMod%Wind,n,atm) )
    Else
        E_apparent = n%E
    End If
    If (ScatMod%Doppler_Broaden) Then
        sigma_T = ScatMod%CS%sig_T_broad(E_apparent,atm%T(n%Z)) * mfp_per_barn_per_km_at_seaLevel
    Else
        sigma_T = ScatMod%CS%sig_T(E_apparent) * mfp_per_barn_per_km_at_seaLevel
    End If
    !Compute distance (geometric and effective) to leak from system
    Call S_and_L_to_edge(atm,n%big_r,n%z,n%zeta,d_to_leak,xEff_to_leak,nb,bb,Lb,db)
    !check for leakage and leakage suppression, treat accordingly; if no leakage, sample distance to next scatter
    If (d_to_leak .LE. 0._dp) Then  !neutron cannot move without leaking
        n%weight = 0._dp
        leaked = .TRUE.
        Return
    End If
    leaked = .FALSE.
    P_not_leak = 1._dp - Exp(-sigma_T * xEff_to_leak)
    If (ScatMod%suppress_leakage) Then
        n%weight = n%weight * P_not_leak
        tau = -Log(1._dp - RNG%Get_Random() * P_not_leak)
    Else
        xi = RNG%Get_Random()
        If (xi .GE. P_not_leak) Then
            leaked = .TRUE.
            Return
        Else
            tau = -Log(1._dp - xi)
        End If
    End If
    !compute EPL to next scatter
    xEff = tau / sigma_T
    !UNDONE Fast analog approximation to avoid EPL to distance rootsolve
    !If (ScatMod%fast_analog) Then  !apply non-analog approximation to get S from L (avoids root-solving)
        !d = L_to_S(atm,xEff,n%big_r,n%z,n%zeta,nb,bb,Lb,db,sigma_T,RNG,n%weight)
    !Else  !Convert EPL to distance
        d = L_to_S(atm,xEff,n%big_r,n%z,n%zeta,nb,bb,Lb,db)
    !End If
    n%r = n%r + d * n%Omega_hat  !update position
    n%t = n%t + d / Neutron_Speed(n%E)  !update simulation time
    !update z,zeta,big_r for new location
    n%zeta = Dot_Product(Unit_Vector(n%r),n%Omega_hat)
    n%big_r = Vector_Length(n%r)
    n%Z = n%big_r - R_Earth
End Subroutine Move_Neutron

Subroutine Next_Event_Neutron(n,ScatMod,d,atm,RNG,source_w)
    Use Kinds, Only: dp
    Use Neutron_Scatter, Only: Neutron_Type
    Use Neutron_Scatter, Only: Scatter_Model_Type
    Use Neutron_Scatter, Only: Scatter_Data_Type
    Use Detectors, Only: Detector_Type
    Use Atmospheres, Only: Atmosphere_Type
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Type(Neutron_Type), Intent(In) :: n
    Type(Scatter_Model_Type), Intent(InOut) :: ScatMod
    Type(Detector_Type), Intent(InOut) :: d
    Type(Atmosphere_Type), Intent(In) :: atm
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp), Intent(In) :: source_w
    Type(Scatter_Data_Type) :: scat
    Integer :: iso,lev
    Integer :: n_lev
    Real(dp) :: E_cm
    Integer :: i_E_cm
    
    If (ScatMod%all_mat_mech) Then
        Call ScatMod%Set_Scatter_prep(scat)
        !set scatter parameters for each material/mechanism and compute next event
        Do iso = 1,ScatMod%CS%n_iso
            Call ScatMod%Set_Scatter_iso(n,atm,RNG,scat,iso,n_lev,E_cm,i_E_cm)
            If (scat%iso_cs(iso) .EQ. 0._dp) Cycle
            Do lev = 0,n_lev
                Call ScatMod%Set_Scatter_lev(scat,lev,E_cm,i_E_cm)
                Call Attempt_Next_Event(n,ScatMod,d,atm,scat,source_w*scat%iso_cs(iso)*scat%lev_cs(lev))
            End Do
            Deallocate(scat%lev_cs)
        End Do
    Else
        !compute a single next-event for the already sampled scatter parameters
        Call Attempt_Next_Event(n,ScatMod,d,atm,ScatMod%scat,source_w)
    End If
End Subroutine Next_Event_Neutron

Subroutine Attempt_Next_Event(n,ScatMod,d,atm,scat,w_scat)
    Use Kinds, Only: dp
    Use Global, Only: inv_TwoPi,inv_FourPi
    Use Global, Only: mfp_per_barn_per_km_at_seaLevel
    Use Global, Only: std_grav_parameter
    Use Global, Only: n_lambda
    Use Detectors, Only: Detector_Type
    Use Atmospheres, Only: Atmosphere_Type
    Use Legendre_Utilities, Only: Legendre_pdf
    Use Cross_Sections, Only: Tabular_Cosine_pdf
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Unit_Vector
    Use Pathlengths, Only: Next_Event_L_to_edge
    Use Neutron_Scatter, Only: Neutron_Type
    Use Neutron_Scatter, Only: Scatter_Model_Type
    Use Neutron_Scatter, Only: Scatter_Data_Type
    Use Neutron_Scatter, Only: Scattered_Angles
    Use Neutron_Utilities, Only: Neutron_Energy
    Use Divergence, Only: Div_Fact_by_shooting
    Use Divergence, Only: Div_Fact_Straight
    Use Find_Trajectory, Only: Next_Event_Trajectory
    Use Astro_Utilities, Only: SME
    Implicit None
    Type(Neutron_Type), Intent(In) :: n
    Type(Scatter_Model_Type), Intent(InOut) :: ScatMod
    Type(Detector_Type), Intent(InOut) :: d
    Type(Atmosphere_Type), Intent(In) :: atm
    Type(Scatter_Data_Type), Intent(In) :: scat
    Real(dp), Intent(In) :: w_scat
    Real(dp) :: mu0cm  !cosine of polar scatter angle for scatter in CM frame
    Real(dp) :: omega0cm  !rotational scatter angle for scatter in CM frame
    Real(dp) :: Omega_hat1_cm(1:3)  ![1 km/s]  velocity unit vector in CM frameafter collision
    Real(dp) :: v1(1:3)  ![km/s] velocity vector of neutron after collision
    Real(dp) :: v1cm(1:3)  ![km/s] velocity vector of neutron after collision in CM frame
    Real(dp) :: sigma_T1  ![km^-1]  total microscopic (seal-level) cross section after scatter
    Real(dp) :: zeta1  !cosine of the zenith angle (angle from verticle to Omega_hat1)
    Real(dp) :: xEff_top_atm  ![km] effective seal-level path length along velocity vector to distance d_top_atm
    Real(dp) :: Bn  !used to compute dmu0cm_dmu0
    Real(dp) :: dmu0cm_dmu0  !conversion from angular pdf in CM fram to lab frame
    Real(dp) :: w2  !modified weight for forced scatter to detector
    Real(dp) :: tof  !time of flight to detector, simulation time of this scatter plus time of flight from current position to the detector
    Real(dp) :: dt  ![s]  flight time from scatter to satellite
    Logical :: path_found  !indicates whether path to satellite was found
    Real(dp) :: v2sat(1:3)  ![km/s]  velocity vector of neutron at arrival to satellite in satellite frame
    Real(dp) :: Omega_hat2_sat(1:3)  ![1 km/s]  unit velocity vector of neutron at arrival to satellite in satellite frame
    Real(dp) :: E2sat  ![keV]  Energy of neutron at arrival to satellite in satellite frame
    Real(dp) :: rS2(1:3)  ![km]  position of the satellite at arrival of neutron
    Real(dp) :: vS2(1:3)  ![km/s]  velocity of satellite at arrival of neutron
    Real(dp) :: divCM  !divergence factor
    Logical :: no_LOS  !flag indicates path to detector intersects Earth
        
    ScatMod%next_events(1) = ScatMod%next_events(1) + 1
    If (ScatMod%Gravity) Then !check if energy is adequate to reach detector
        If (SME(n%big_r,scat%s1cm+scat%u_speed) .LT. 0._dp) Then !neutron's max velocity is less than escape velocity, check if satellite altitude is achievable
            If (-2._dp * n%big_r * std_grav_parameter / ( (scat%s1cm + scat%u_speed)**2 * n%big_r - 2._dp * std_grav_parameter ) .LT. d%sat%rp) Return  !neutron max height is insufficent to reach satellite at its lowest point
            !A more robust check for trajectories that can meet the satellite is performed later, but this saves computation time if the max achievable height is insufficent
        End If
    End If
    Call Next_Event_Trajectory(d%sat,ScatMod%Gravity,n%r,n%t,scat%s1cm,scat%u,path_found,rS2,dt,v1cm,v2sat,vS2)
    If (.NOT. path_found) Return
    !Check for line of sight, compute EPL along the path as a side-effect
    v1 = v1cm + scat%u
    zeta1 = Dot_Product(Unit_Vector(n%r),Unit_Vector(v1))
    If (ScatMod%Gravity) Then  !LOS is not actually checked in the gravity case, the sover does not return trajectories without line of sight
        no_LOS = Next_Event_L_to_edge(atm,n%big_r,Vector_Length(v1),n%Z,zeta1,xEff_top_atm)
    Else
        no_LOS = Next_Event_L_to_edge(atm,n%big_r,n%Z,zeta1,xEff_top_atm)
        If (no_LOS) Return
    End If
    ScatMod%next_events(2) = ScatMod%next_events(2) + 1
    !TOTAL TIME OF FLIGHT TO ARRIVAL
    tof = n%t + dt
    !ENERGY AT ARRIVAL
    E2sat = Neutron_Energy(v2sat)
    !Check if time and energy are in range of the detector grid
    If ( E2sat .LT. d%TE_grid(2)%min .OR. &
       & E2sat .GT. d%TE_grid(2)%max .OR. &
       & tof   .LT. d%TE_grid(1)%min .OR. &
       & tof   .GT. d%TE_grid(1)%max      ) Then !No contribution from this neutron due to out of range of detector grid
        !Record reason for no contribution and return
        If (E2sat.LT.d%TE_grid(2)%min .OR. E2sat.GT.d%TE_grid(2)%max) Then  !E out of range, check for time range
            If (tof.LT.d%TE_grid(1)%min .OR. tof.GT.d%TE_grid(1)%max) Then  !t also out of range
                ScatMod%n_no_tally(1) = ScatMod%n_no_tally(1) + 1
            Else  !just E out of range
                ScatMod%n_no_tally(3) = ScatMod%n_no_tally(3) + 1
            End If
        Else If (tof.LT.d%TE_grid(1)%min .OR. tof.GT.d%TE_grid(1)%max) Then  !just t out of range
            ScatMod%n_no_tally(2) = ScatMod%n_no_tally(2) + 1
        End If
        Return
    Else  !contribution hits the grid
        ScatMod%next_events(3) = ScatMod%next_events(3) + 1
    End If
    !DIRECTION AT ARRIVAL
    Omega_hat2_sat = Unit_Vector(v2sat)
    !WEIGHT ADJUSTMENTS... Scattered angle, divergence, absorption & scatter suppression, decay
    w2 = n%weight * w_scat !initial value (neutron weight multiplied by scatter weight for all mat/mech and/or exoatmospheric weight adjustment where applicable)
    !Adjust for scatter angle
    Omega_hat1_cm = Unit_Vector(v1cm)
    Call Scattered_Angles(scat%Omega_hat0_cm,Omega_hat1_cm,mu0cm,omega0cm,scat%B_hat,scat%C_hat)
    Bn = scat%s1cm / scat%u_speed
    dmu0cm_dmu0 = Abs((1._dp + Bn * (2._dp * mu0cm + Bn))**(1.5_dp) / (Bn**2 * (Bn + mu0cm)))  !Eqn 479 from Haste-N removed material (p 39)
    If (ScatMod%aniso_dist) Then
        If (scat%da_is_Legendre) Then
            w2 = w2 * dmu0cm_dmu0 * Legendre_pdf(mu0cm,scat%n_a,scat%a(0:scat%n_a)) * inv_TwoPi
        Else
            w2 = w2 * dmu0cm_dmu0 * Tabular_Cosine_pdf(mu0cm,scat%n_a1,scat%a_tab1(1:scat%n_a1,:),scat%n_a2,scat%a_tab2(1:scat%n_a2,:),scat%a_tab_Econv) * inv_TwoPi
        End If
    Else
        w2 = w2 * dmu0cm_dmu0 * inv_FourPi
    End If
    !Adjust for divergence
    If (ScatMod%Gravity) Then
        divCM = Div_Fact_by_shooting(n%r,Omega_hat1_cm,scat%s1cm,scat%u,dt,vS2,v2sat+vS2)
    Else
        divCM = Div_Fact_Straight(n%r,v1,rS2,scat%u,dt,vS2)
    End If
    w2 = w2 * divCM
    !Adjust for absorption suppression at the point of scatter
    w2 = w2 * (1._dp - scat%sig_A / scat%sig_T)
    !Adjust for interaction suppression on the path out of the atmosphere
    If (ScatMod%Doppler_Broaden) Then
        sigma_T1 = ScatMod%CS%sig_T_broad(Neutron_Energy(v1),atm%T(n%Z))
    Else
        sigma_T1 = ScatMod%CS%sig_T(Neutron_Energy(v1))
    End If
    w2 = w2 * Exp(-sigma_T1 * mfp_per_barn_per_km_at_seaLevel * xEff_top_atm)
    !Adjust for free neutron decay
    If (ScatMod%neutron_decay) w2 = w2 * Exp(-n_lambda * tof)
    !TALLY
    Call d%Tally_Scatter(E2sat,Omega_hat2_sat,tof,w2)
End Subroutine Attempt_Next_Event

Subroutine Scatter_Neutron(n,ScatMod,RNG,absorbed)
    Use Kinds, Only: dp
    Use Neutron_Scatter, Only: Neutron_Type
    Use Neutron_Scatter, Only: Scatter_Model_Type
    Use Neutron_Scatter, Only: Scattered_Direction
    Use Neutron_Utilities, Only: Neutron_Energy
    Use Random_Numbers, Only: RNG_Type
    Use Random_Directions, Only: Isotropic_Omega_hat
    Use Random_Directions, Only: Neutron_Anisotropic_mu0cm
    Use Random_Directions, Only: Isotropic_Azimuth
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Unit_Vector
    Implicit None
    Type(Neutron_Type), Intent(InOut) :: n
    Type(Scatter_Model_Type), Intent(In) :: ScatMod
    Type(RNG_Type), Intent(InOut) :: RNG
    Logical, Intent(Out) :: absorbed
    Real(dp) :: mu0cm  !anisotropic cosine of polar scatter angle for scatter in CM frame
    Real(dp) :: omega0cm  !isotropic azimuth for scatter in CM frame
    Real(dp) :: Omega_hat1_cm(1:3)  ![1 km]  velocity unit vectors in CM frame before and after collision
    Real(dp) :: v1(1:3)  ![km/s] velocity vector of neutron after collision 
        
    !Check for absorption and absorption supression
    If (ScatMod%suppress_absorption) Then !adjust weight for ignored absorption possibility
        n%weight = n%weight * (1._dp - ScatMod%scat%sig_A / ScatMod%scat%sig_T)
    Else !sample for absorption
        If (RNG%Get_Random() .LE. ScatMod%scat%sig_A / ScatMod%scat%sig_T) Then
            absorbed = .TRUE.
            Return
        End If
    End If
    absorbed = .FALSE.
    !Choose new direction based on scatter mechanic
    If (ScatMod%aniso_dist) Then
        If (ScatMod%scat%da_is_Legendre) Then
            mu0cm = Neutron_Anisotropic_mu0cm(ScatMod%scat%a(0:ScatMod%scat%n_a),ScatMod%scat%n_a,RNG)
        Else
            mu0cm = Neutron_Anisotropic_mu0cm(ScatMod%scat%n_a1,ScatMod%scat%a_tab1(1:ScatMod%scat%n_a1,:),ScatMod%scat%n_a2,ScatMod%scat%a_tab2(1:ScatMod%scat%n_a2,:),ScatMod%scat%a_tab_Econv,RNG)
        End If
        omega0cm = Isotropic_Azimuth(RNG)
        Omega_hat1_cm = Scattered_Direction(mu0cm,omega0cm,ScatMod%scat%A_hat,ScatMod%scat%B_hat,ScatMod%scat%C_hat)
    Else
        Omega_hat1_cm = Isotropic_Omega_hat(RNG)  !new isotropic direction
    End If
    v1 = Omega_hat1_cm*ScatMod%scat%s1cm + ScatMod%scat%u  !new velocity vector in lab frame
    n%Omega_hat = Unit_Vector(v1)  !new direction unit vector
    n%E = Neutron_Energy(Vector_Length(v1))!(s1)  !convert speed to energy
    n%zeta = Dot_Product(Unit_Vector(n%r),n%Omega_hat)  !update zeta for new direction
End Subroutine Scatter_Neutron

Function Kill_Neutron(t,E,t_max,E_min,n_kills) Result(kill)
    Use Kinds, Only: dp
    Implicit None
    Logical :: kill
    Real(dp), Intent(In) :: E
    Real(dp), Intent(In) :: t
    Real(dp), Intent(In) :: E_min
    Real(dp), Intent(In) :: t_max
    Integer(8), Intent(InOut) :: n_kills(1:2)  !# of kills due to (1)Time, (2)Energy
    
    kill = .FALSE.
    If (t .GT. t_max) Then
        kill = .TRUE.
        n_kills(1) = n_kills(1) + 1
    End If
    If (E .LT. E_min) Then
        kill = .TRUE.
        n_kills(2) = n_kills(2) + 1
    End If
End Function Kill_Neutron

Function Roulette_Neutron(weight,roulette_w,roulette_rate,roulette_mult,n_kills,RNG) Result(kill)
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Logical :: kill
    Real(dp), Intent(InOut) :: weight
    Real(dp), Intent(In) :: roulette_w
    Real(dp), Intent(In) :: roulette_rate
    Real(dp), Intent(In) :: roulette_mult
    Integer(8), Intent(InOut) :: n_kills  !# of kills due to roulette
    Type(RNG_Type), Intent(InOut) :: RNG
    
    kill = .FALSE.
    If (weight .GT. roulette_w) Return  !no roulette procedure
    If (RNG%Get_Random() .GT. roulette_rate) Then  !kill
        kill = .TRUE.
        n_kills = n_kills + 1
    Else  !survive
        weight = weight * roulette_mult
    End If
End Function Roulette_Neutron

End Module MC_Neutron

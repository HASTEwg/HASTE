Module Find_Trajectory
    
    Implicit None
    Private
    Public :: Next_Event_Trajectory
    Public :: Simple_Trajectory
    
Contains

Subroutine Next_Event_Trajectory(sat, Gravity, r1, t1, s1cm, u_vec, Found, r2, tof, v1cm, v2sat, vS2) 
    Use Kinds, Only: dp
    Use Satellite_Motion, Only: Satellite_Position_Type
    Use Utilities, Only: Unit_Vector
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Converged
    Use Astro_Utilities, Only: Lambert => Lambert_Gooding
    Use Astro_Utilities, Only: Hits_Earth
    Implicit None
    Type(Satellite_Position_Type), Intent(In) :: sat
    Logical, Intent(In) :: Gravity
    Real(dp), Intent(In):: r1(1:3)       ! [km]   Location of scatter
    Real(dp), Intent(In) :: t1           ! [s]    time of scatter
    Real(dp), Intent(In) :: s1cm         ! [km/s] speed of neutron in cm system after scatter
    Real(dp), Intent(In) :: u_vec(1:3)   ! [km/s] velocity of center of mass of atom and neutron    
    Logical, Intent(Out) :: Found        ! Whether solution is found (neutron can reach satellite)
    Real(dp), Intent(Out) :: r2(1:3)     ! [km]   neutron/target position at time of arrival
    Real(dp), Intent(Out) :: tof         ! [s] time of flight from r1 to r2 on transfer trajectory
    Real(dp), Intent(Out) :: v1cm(1:3)   ! [km/s] v1 in CM system
    Real(dp), Intent(Out) :: v2sat(1:3)  ! [km/s]  neutron velocity at arrival in rest frame of satellite
    Real(dp), Intent(Out) :: vS2(1:3)    ! [km/s] satellite velocity at time of arrival
    Real(dp) :: tof_old          ! [s]    copy of tof used to test for convergence of tof
    Real(dp) :: v1(1:3)          ! [km/s] velocity of neutron leaving scatter (inertial frame)
    Real(dp) :: vScm(1:3)        ! [km/s] vS in CM frame
    Real(dp) :: v2(1:3)          ! [km/s] velocity of neutron at satellite rendezvous (inertial frame)
    Real(dp) :: ds_1,ds_2,ds,ds_old ![km/s] error in s1cm at points for root-solving
    Real(dp) :: tof_1,tof_2         ![s] min and max vaules for time of flight
    Real(dp) :: s1cm_1,s1cm_2       ![km/s] speed in CM frame for tof_1 and tof_2
    Real(dp) :: u_speed             ![km/s] speed of CM frame
    Real(dp) :: m   !relaxation factor for Illinois fix to false position
    Integer :: i    !counter for Illinois fixes to false position
    Integer :: i_lim

    If (Gravity) Then
        found = .FALSE.
        u_speed = Vector_Length(u_vec)
        r2 = sat%R(t1)
        !estimate minimum TOF: starting distance to target divided by max possible closing speed
        tof_1 = Vector_Length(r2 - r1) / (s1cm + u_speed + sat%vp)  !this is less than or equal to the true minimum
        !check if this minimum TOF bounds true TOF
        Call Lambert(r1,sat%R(t1+tof_1),tof_1,v1,v2)
        s1cm_1 = Vector_Length(v1 - u_vec)  !this s1cm_i needs to be higher than s1cm
        If (s1cm_1 .LT. s1cm) Return  !there is not a trajectory that joins the two points with the available neutron speed
        !estimate maximum TOF: depends on transfer orbit type...
        Call Find_max_TOF(tof_2)
        !check if this maximum TOF bounds true TOF
        Call Lambert(r1,sat%R(t1+tof_2),tof_2,v1,v2)
        s1cm_2 = Vector_Length(v1 - u_vec)  !this s1cm_i needs to be lower than s1cm
        If (s1cm_2 .GT. s1cm) Return  !there is not a trajectory that joins the two points with the available neutron speed
        !min and max TOF are established and bracket a TOF for an achievable transfer trajectory
        !Use False-Position w/ Illinois modification to find TOF of transfer
        ds_1 = s1cm_1 - s1cm
        ds_2 = s1cm_2 - s1cm
        !False-Position starter
        Call New_TOF_and_DS(tof_1,tof_2,ds_1,ds_2,tof,ds)
        If (ds*ds_1 .GE. 0._dp) Then !ds and ds_1 have same sign
            tof_1 = tof
            ds_1 = ds
        Else  !ds and ds_2 have same sign
            tof_2 = tof
            ds_2 = ds
        End If
        !Proceed with False-Position w/ Illinois
        !HACK Iteration limit prevents stalling of the bracketing method at expense of missing some rendezvous
        Do i_lim = 1,1000
            tof_old = tof
            ds_old = ds
            !False-Position for next TOF
            Call New_TOF_and_DS(tof_1,tof_2,ds_1,ds_2,tof,ds)
            !compare to desired post-scatter speed and convergence of TOF interval
            !N2H Study convergence criteria for speed and tof, current criteria is probably overkill
            If ( (Abs(ds).LT.1.E-6_dp .AND. Converged(tof_1,tof_2,rTol=1.E-12_dp,aTol=1.E-6_dp)) & !w/in 1 cm/s of desired neutron speed (a 1 micro-eV neutron goes about 14 m/s..) AND w/in 1 microsecond of actual TOF
               & .OR. ds.EQ.0._dp .OR. tof_1.EQ.tof_2 ) Then  !OR iteration landed on answer or closed interval
                !check that trajectory does not intersect Earth
                If (Hits_Earth(r1,v1,r2,v2)) Return
                !If we get this far, a valid trajectory has been found!
                found = .TRUE.
                v2Sat = v2 - vS2
                Return
            End If
            !check for poor behavior of false position
            If (ds*ds_old .GE. 0._dp) Then !ds and ds_old has same sign, False-Position is behaving poorly, apply Illinois
                m = 0.5_dp
                If (ds*ds_1 .GE. 0._dp) Then !function value ds_2 will be modified
                    Do i = 1,5
                        !move left bracket
                        tof_1 = tof
                        ds_1 = ds
                        !False-Position-Illinois for next TOF
                        Call New_TOF_and_DS(tof_1,tof_2,ds_1,ds_2*m,tof,ds)
                        !check for sign change in ds
                        If (ds*ds_1 .LT. 0._dp) Then  !Illinois sucessfully moved BOTH brackets w/in 5 attempts
                            !move right bracket
                            tof_2 = tof
                            ds_2 = ds
                            Exit
                        End If
                        m = m * 0.5_dp
                    End Do
                Else !function value ds_1 will be modified
                    Do i = 1,5
                        !move right bracket
                        tof_2 = tof
                        ds_2 = ds
                        !False-Position-Illinois for next TOF
                        Call New_TOF_and_DS(tof_1,tof_2,ds_1*m,ds_2,tof,ds)
                        !check for sign change in ds
                        If (ds*ds_2 .LT. 0._dp) Then  !Illinois sucessfully moved BOTH brackets w/in 5 attempts
                            !move left bracket
                            tof_1 = tof
                            ds_1 = ds
                            Exit
                        End If
                        m = m * 0.5_dp
                    End Do
                End If
            Else !False-Position is behaving well, replace appropriate bracket
                If (ds*ds_1 .GE. 0._dp) Then !ds and ds_1 have same sign
                    tof_1 = tof
                    ds_1 = ds
                Else  !ds and ds_2 have same sign
                    tof_2 = tof
                    ds_2 = ds
                End If
            End If
        End Do
    Else  !straight (no gravity) trajectory
        If (sat%is_conic) Then
            !TODO Evaluate code repetition in the following with above, reduce by breaking into subroutines or something
            found = .FALSE.
            u_speed = Vector_Length(u_vec)
            r2 = sat%R(t1)
            !estimate minimum TOF: starting distance to target divided by max possible closing speed
            tof_1 = Vector_Length(r2 - r1) / (s1cm + u_speed + sat%vp)  !this is less than or equal to the true minimum
            !check if this minimum TOF bounds true TOF
            v1 = (r2 - r1) / tof_1
            s1cm_1 = Vector_Length(v1 - u_vec)  !this s1cm_i needs to be higher than s1cm
            If (s1cm_1 .LT. s1cm) Return  !there is not a trajectory that joins the two points with the available neutron speed
            !estimate maximum TOF: use same criteria when gravity is on, checked against the minimum possible closing speed without gravity
            Call Find_max_TOF(tof_2)
            !check if straight flight time at min closing speed is a better bound
            If ( Abs(s1cm - (u_speed + sat%vp)) .GT. 0._dp ) tof_2 = Min( tof_2 , Vector_Length(r2 - r1) / Abs(s1cm - (u_speed + sat%vp)) )
            !check if this maximum TOF bounds true TOF
            v1 = (r2 - r1) / tof_2
            s1cm_2 = Vector_Length(v1 - u_vec)  !this s1cm_i needs to be lower than s1cm
            If (s1cm_2 .GT. s1cm) Return  !there is not a trajectory that joins the two points with the available neutron speed
            !min and max TOF are established and bracket a TOF for an achievable transfer trajectory
            !Use False-Position w/ Illinois modification to find TOF of transfer
            ds_1 = s1cm_1 - s1cm
            ds_2 = s1cm_2 - s1cm
            !False-Position starter
            Call New_TOF_and_DS_straight(tof_1,tof_2,ds_1,ds_2,tof,ds)
            If (ds*ds_1 .GE. 0._dp) Then !ds and ds_1 have same sign
                tof_1 = tof
                ds_1 = ds
            Else  !ds and ds_2 have same sign
                tof_2 = tof
                ds_2 = ds
            End If
            !Proceed with False-Position w/ Illinois
            Do
                tof_old = tof
                ds_old = ds
                !False-Position for next TOF
                Call New_TOF_and_DS_straight(tof_1,tof_2,ds_1,ds_2,tof,ds)
                !compare to desired post-scatter speed and convergence of TOF interval
                !N2H Study convergence criteria for speed and tof, current criteria is probably overkill
                If ( (Abs(ds).LT.1.E-6_dp .AND. Converged(tof_1,tof_2,rTol=1.E-12_dp,aTol=1.E-6_dp)) & !w/in 1 cm/s of desired neutron speed (a 1 micro-eV neutron goes about 14 m/s..) AND w/in 1 microsecond of actual TOF
                   & .OR. ds.EQ.0._dp .OR. tof_1.EQ.tof_2 ) Then  !OR iteration landed on answer or closed interval
                    found = .TRUE.
                    vScm = vS2 - u_vec
                    v2Sat = v1cm - vScm
                    Return
                End If
                !check for poor behavior of false position
                If (ds*ds_old .GE. 0._dp) Then !ds and ds_old has same sign, False-Position is behaving poorly, apply Illinois
                    m = 0.5_dp
                    If (ds*ds_1 .GE. 0._dp) Then !function value ds_2 will be modified
                        Do i = 1,5
                            !move left bracket
                            tof_1 = tof
                            ds_1 = ds
                            !False-Position-Illinois for next TOF
                            Call New_TOF_and_DS_straight(tof_1,tof_2,ds_1,ds_2*m,tof,ds)
                            !check for sign change in ds
                            If (ds*ds_1 .LT. 0._dp) Then  !Illinois sucessfully moved BOTH brackets w/in 5 attempts
                                !move right bracket
                                tof_2 = tof
                                ds_2 = ds
                                Exit
                            End If
                            m = m * 0.5_dp
                        End Do
                    Else !function value ds_1 will be modified
                        Do i = 1,5
                            !move right bracket
                            tof_2 = tof
                            ds_2 = ds
                            !False-Position-Illinois for next TOF
                            Call New_TOF_and_DS_straight(tof_1,tof_2,ds_1*m,ds_2,tof,ds)
                            !check for sign change in ds
                            If (ds*ds_2 .LT. 0._dp) Then  !Illinois sucessfully moved BOTH brackets w/in 5 attempts
                                !move left bracket
                                tof_1 = tof
                                ds_1 = ds
                                Exit
                            End If
                            m = m * 0.5_dp
                        End Do
                    End If
                Else !False-Position is behaving well, replace appropriate bracket
                    If (ds*ds_1 .GE. 0._dp) Then !ds and ds_1 have same sign
                        tof_1 = tof
                        ds_1 = ds
                    Else  !ds and ds_2 have same sign
                        tof_2 = tof
                        ds_2 = ds
                    End If
                End If
            End Do
        Else
            !For stationary and linear detector motion, result from Simple_Trajectory is a valid trajectory
            Call sat%R_and_V(t1,r2,vS2)
            vScm = vS2 - u_vec
            Call Simple_Trajectory(r1, r2, s1cm, vScm, v1cm, tof, Found)
        End If
        If (Found) v2Sat = v1cm - vScm
    End If
Contains
    !Contains construct gives these subroutines access to parent routine variables and USE statements
    !NOTE:  These contained subroutines INTENTIONALLY CAUSE SIDE-EFFECTS in the calling routine other than the explicit In and Out variables
    !NOTE:  Removing these subroutines from the contained construct will change the functionality of the containing routine
    Subroutine Find_max_TOF(max_TOF)
        Use Astro_Utilities, Only: SME
        Use Astro_Utilities, Only: Parabolic_TOF
        Use Astro_Utilities, Only: Lambert_minV
        Implicit None
        Real(dp), Intent(Out) :: max_TOF
        
        If (SME(Vector_Length(r1),s1cm-u_speed) .GE. 0._dp) Then  !neutron must be on a parabolic or hyperbolic trajectory, max TOF occurs where transfer SME is zero
            !find transfer orbit TOF where SME=0 (parabolic transfer)
            !initial guess is parabolic flight time from r1 to sat at time of scatter
            max_TOF = Parabolic_TOF(r1,r2)
            If (.NOT. sat%is_stationary) Then  !need to iterate to refine
                Do
                    tof = max_TOF
                    r2 = sat%R(t1+max_TOF)  !target position after tof_2
                    max_TOF = Parabolic_TOF(r1,r2)
                    If (Converged(max_TOF,tof,rTol=1.E-9_dp,aTol=1.E-3_dp)) Exit  !w/in 1 millisecond of TOF for transfer with SME=0
                End Do
            End If
        Else  !transfer orbits include elliptical 
            !HACK Elliptical transfers and large u_speed introduce the complication of multiple roots, currently only one TOF is found
            !UNDONE The elliptical cases in this routine also do not account for complication of the available velocity function, multiple roots could pose problems...
            !find transfer orbit TOF where minV TOF is same as Lambert TOF
            !initial guess is minimum energy flight time from r1 to sat at time of scatter
            Call Lambert_minV(r1,r2,tof = max_TOF)
            If (.NOT. sat%is_stationary) Then  !need to iterate to refine
                Do
                    tof = max_TOF
                    r2 = sat%R(t1+max_TOF)  !target position after tof_max
                    Call Lambert_minV(r1,r2,tof = max_TOF)
                    If (Converged(max_TOF,tof,rTol=1.E-9_dp,aTol=1.E-3_dp)) Exit  !w/in 1 millisecond of TOF for transfer where minV TOF is same as Lambert TOF
                End Do
            End If
        End If
    End Subroutine Find_max_TOF
    Subroutine New_TOF_and_DS(tof1,tof2,ds1,ds2,tof3,ds3)
        Implicit None
        Real(dp), Intent(In) :: tof1,tof2
        Real(dp), Intent(In) :: ds1,ds2
        Real(dp), Intent(Out) :: tof3,ds3

        If (ds1 .NE. ds2) Then  !False-Position for next TOF
            tof3 = (tof1*ds2 - tof2*ds1) / (ds2 - ds1)
        Else !ds1 and ds2 too close, bisect for TOF
            tof3 = 0.5_dp * (tof1 + tof2)
        End If
        !update target position
        Call sat%R_and_V(t1+tof3,r2,vS2)
        !compute Lambert velocities
        Call Lambert(r1,r2,tof3,v1,v2)
        !convert Lambert-V into neutron speed in CM frame
        v1cm = v1 - u_vec
        ds3 = Vector_Length(v1cm) - s1cm
    End Subroutine New_TOF_and_DS
    Subroutine New_TOF_and_DS_straight(tof1,tof2,ds1,ds2,tof3,ds3)
        Implicit None
        Real(dp), Intent(In) :: tof1,tof2
        Real(dp), Intent(In) :: ds1,ds2
        Real(dp), Intent(Out) :: tof3,ds3

        If (ds1 .NE. ds2) Then  !False-Position for next TOF
            tof3 = (tof1*ds2 - tof2*ds1) / (ds2 - ds1)
        Else !ds1 and ds2 too close, bisect for TOF
            tof3 = 0.5_dp * (tof1 + tof2)
        End If
        !update target position
        Call sat%R_and_V(t1+tof3,r2,vS2)
        !compute velocity to intercept
        v1 = (r2 - r1) / tof3
        !convert intercept-V into neutron speed in CM frame
        v1cm = v1 - u_vec
        ds3 = Vector_Length(v1cm) - s1cm
    End Subroutine New_TOF_and_DS_straight
    !NOTE:  The preceeding contained subroutines INTENTIONALLY CAUSE SIDE-EFFECTS in the calling routine other than the explicit In and Out variables
    !NOTE:  Removing these subroutines from the contained construct will change the functionality of the containing routine
End Subroutine Next_Event_Trajectory
    
Subroutine Simple_Trajectory(r1, r2, s1cm, vScm, v1cm, dt, Found)
    Use Kinds, Only: dp
    Use Utilities, Only: Unit_Vector
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Cross_Product
    Implicit None
    Real(dp), Intent(In) :: r1(1:3)      ! [km]   Location of scatter
    Real(dp), Intent(In) :: r2(1:3)      ! [km]   satellite position at time of scatter (actual or adjusted)
    Real(dp), Intent(In) :: s1cm         ! [km/s] speed of neutron in cm system after scatter
    Real(dp), Intent(In) :: vScm(1:3)    ! [km/s] velocity of satellite in cm frame
    Real(dp), Intent(Out) :: v1cm(1:3)   ! [km/s] velocity of neutron leaving scatter in cm frame
    Real(dp), Intent(Out) :: dt          ! [s]    time of flight
    Logical, Intent(Out) :: Found
    ! A, B, C are basis vectors for Cartesian coordinates in CM system
    Real(dp) :: A(1:3)           ! points from scatter point to satellite position at t1
    Real(dp) :: B(1:3)           ! basis vector orthogonal to A and vScm
    Real(dp) :: C(1:3)           ! basis vector in plane of A and vScm
    Real(dp) :: s_Closing        ! [km/s] speed of neutron at arrival in satellite frame
    Real(dp) :: vScm_A, vScm_C   ! components of vScm (No B component by construction of A, B, C)
    Real(dp) :: v1cm_A, v1cm_C   ! components of v1cm (No B component by construction of A, B, C)

    Found = .FALSE.
    A = Unit_Vector(r2-r1)
    If (Any(vScm .NE. 0._dp)) Then
        ! Criteria for neutron to meet satellite are: v1cm_A > vScm and v1cm_A = vScm_A
        B = Unit_Vector(Cross_Product(vScm, A))
        C = Cross_Product(A, B)
        vScm_A = Dot_Product(vScm, A)
        vScm_C = Dot_Product(vScm, C)
        v1cm_C = vScm_C     ! Criterion for neutron to meet satellite
        If (s1cm .LT. v1cm_C) Return
        v1cm_A = Sqrt(s1cm**2 - v1cm_C**2)  ! No B component by construction of A, B, and C
        v1cm = v1cm_A * A + v1cm_C * C      ! needed for computing scatter cosines mu0cm and mu0
        s_Closing = v1cm_A - vScm_A
        If (s_Closing .LE. 0._dp) Return
        dt = Vector_Length(r2-r1) / s_Closing
        Found = .TRUE.
    Else !special case for apparently stationary target
        v1cm = s1cm * A
        dt = Vector_Length(r2-r1) / s1cm
        Found = .TRUE.
    End If
End Subroutine Simple_Trajectory

End Module Find_Trajectory

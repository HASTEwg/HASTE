Program testPrevTraj

Use Kinds, Only: dp
Use Satellite_Motion, Only: Satellite_Position_Type
Use Satellite_Motion, Only: Initialize_Satellite_Motion
Use Global, Only: X_hat,Y_hat,Z_hat
Use Neutron_Utilities, Only: Neutron_Speed
Use Neutron_Utilities, Only: Neutron_Energy
Use Find_Trajectory, Only: Prev_Event_Trajectory
Use Diverge_Approx, Only: Div_Fact_by_shooting
Use Diverge_Approx, Only: Div_Fact_straight

Use Global, Only: Esc_speed,R_center
Use Utilities, Only: Unit_Vector
Use Utilities, Only: Cross_Product
Use Utilities, Only: Vector_Length

Use Astro_Utilities, Only: Kepler_Gooding

Implicit None

Type(Satellite_Position_Type) :: sat
Logical :: Gravity  !flag to set gravity on or off
Real(dp) :: t2  !time relative to epoch of intercept event
Real(dp) :: r_sat(1:3),v_sat(1:3)  !position and velocity of the satellite at t2
Real(dp) :: D_hat(1:3),N_hat(1:3),F_hat(1:3)  !basis vectors for satellite frame of reference
Real(dp) :: Omega_hat2(1:3,1:6) !direction of neutron arrival in satellite frame
Character(3) :: dir_names(1:6)
Real(dp) :: En(1:4) ![keV] neutron energy at arrival in satellite frame
Logical :: Found  !flag for whether a trajectory was found
Real(dp) :: r1(1:3),v1(1:3),tof !position,velocity, time of flight defining flight from the surface of the central body
Real(dp) :: DFact !divergence factor for the intercepting flight from emission to intercept
Real(dp) :: r2(1:3),v2(1:3)
Integer :: test_unit,trace_unit,stat
Integer :: i,j
Logical :: test_sat_trace = .FALSE.

Gravity = .TRUE.
Call Initialize_Satellite_Motion('','Conic_tab ',sat)
!epoch (time of detection) at 13:13 on 13 Jan 1998
t2 = 220380._dp  !This many seconds into mission start
!Satellite DNF basis vectors
Call sat%R_and_V(t2,r_sat,v_sat)
D_hat = -Unit_Vector(r_sat)
N_hat = Unit_Vector(Cross_Product(D_hat,v_sat))
F_hat = Cross_Product(N_hat,D_hat)

En = (/ 1000._dp,1._dp,1.E-3_dp,0.025E-3_dp /)
Omega_hat2 = 0._dp
Omega_hat2(:,1) = -Unit_Vector(D_hat)  !FROM downward
Omega_hat2(:,2) = -Unit_Vector(D_hat-F_hat)  !FROM down and backward
Omega_hat2(:,3) = -Unit_Vector(D_hat+F_hat)  !FROM down and forward
Omega_hat2(:,4) = -Unit_Vector(N_hat+D_hat)  !FROM off to the side and down
Omega_hat2(:,5) = -Unit_Vector(N_hat+D_hat-F_hat)  !FROM off to the side and down and backward
Omega_hat2(:,6) = -Unit_Vector(N_hat+D_hat+F_hat)  !FROM off to the side and down and forward
dir_names = (/ 'D  ', & 
             & 'DB ', & 
             & 'DF ', & 
             & 'DN ', & 
             & 'DNB', & 
             & 'DNF'  /)

Open( NEWUNIT = test_unit , FILE = 'prevTraj_test.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE' ,  IOSTAT = stat )
Do j = 1,6
    Write(test_unit,'(A)') '--------------------------------------------------------------------------------'
    Write(test_unit,'(A)') 'TEST CASE '//dir_names(j)
    Write(test_unit,'(A)') '--------------------------------------------------------------------------------'
    Do i = 1,4
        Write(test_unit,'(A,ES10.3E3,A)') 'Energy ',En(i),' keV from direction '//dir_names(j)
        Write(*,'(A,ES10.3E3,A)',ADVANCE='NO') 'Energy ',En(i),' keV from direction '//dir_names(j)//'... '
        Call Prev_Event_Trajectory(sat, Gravity, t2, Omega_hat2(:,j)*Neutron_Speed(En(i)), Found, r1, v1, tof)
        If (Found) Then
            Call Kepler_Gooding(r1,v1,tof,r2,v2)
            DFact = Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)
        End If
        If (Found) Then
            Write(*,'(A)') 'Found'
            Write(test_unit,'(A)') 'This is r, v, En, tof, and D-factor for the emission:'
            Write(test_unit,'(A,3ES25.16E3)') 'r = ',r1
            Write(test_unit,'(A,3ES25.16E3)') 'v = ',v1
            Write(test_unit,'(A,ES25.16E3)') 'En =  ',Neutron_Energy(v1)
            Write(test_unit,'(A,ES25.16E3)') 'tof = ',tof
            Write(test_unit,'(A,ES25.16E3)') 'D =   ',DFact
            Write(test_unit,'(A)') 'Radius of the moon and radius of emission should match:'
            Write(test_unit,'(A,ES25.16E3)') 'Rc = ',R_center
            Write(test_unit,'(A,ES25.16E3)') 'Re = ',Vector_Length(r1)
            Write(test_unit,'(A)') 'Check the propagation...'
            Write(test_unit,'(A)') 'R-vectors should match:'
            Write(test_unit,'(A,3ES25.16E3)') 'Rn = ',r2
            Write(test_unit,'(A,3ES25.16E3)') 'Rs = ',r_sat
            Write(test_unit,'(A)') 'V-vectors should match:'
            Write(test_unit,'(A,3ES25.16E3)') 'Vn = ',v2-v_sat
            Write(test_unit,'(A,3ES25.16E3)') 'Vd = ',Omega_hat2(:,j)*Neutron_Speed(En(i))
            Write(test_unit,'(A)') 'Energy at detector should match:'
            Write(test_unit,'(A,3ES25.16E3)') 'En = ',Neutron_Energy(v2-v_sat)
            Write(test_unit,'(A,3ES25.16E3)') 'Ed = ',En(i)
        Else
            Write(*,'(A)') 'NOT Found'
            Write(test_unit,'(A)') 'Intercept not found.'
        End If
        Write(test_unit,*)
    End Do
    Write(test_unit,*)
End Do
Close(test_unit)

If (test_sat_trace) Then !test the sat_trace functionality
    Call sat%cleanup()
    Call Initialize_Satellite_Motion('','Conic_tab ',sat)
    Open( NEWUNIT = trace_unit , FILE = 'trace_test.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE' ,  IOSTAT = stat )
    t2 = -60._dp
    i = 0
    Do
        Call sat%r_and_v(t2,r2,v2)
        Write(trace_unit,'(7ES27.16E3)') t2,r2,v2
        t2 = t2 + 60._dp
        i = i + 1
        If (t2 .GT. sat%ts(sat%nt)+60._dp) Exit
    End Do
    Close(trace_unit)
End If

End Program

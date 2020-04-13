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
Real(dp) :: Omega_hat2(1:3) !direction of neutron arrival in satellite frame
Real(dp) :: En ![keV] neutron energy at arrival in satellite frame
Logical :: Found  !flag for whether a trajectory was found
Real(dp) :: r1(1:3),v1(1:3),tof !position,velocity, time of flight defining flight from the surface of the central body
Real(dp) :: DFact !divergence factor for the intercepting flight from emission to intercept
Real(dp) :: r2(1:3),v2(1:3)

!epoch (t=0) at 13:13 on 13 Jan 1998
sat%r0 = (/ -427.81333973957254_dp , &
          & -1488.4686817418115_dp , &
          &  1052.6158529878853_dp   /)
sat%v0 = (/  0.2549511459741853_dp , & 
          &  0.8912224006586242_dp , &
          &  1.3197229103204031_dp   /)
Call Initialize_Satellite_Motion('Conic     ',sat)
Gravity = .TRUE.

t2 = 0._dp
!Satellite DNF basis vectors
Call sat%R_and_V(t2,r_sat,v_sat)
D_hat = -Unit_Vector(r_sat)
N_hat = Unit_Vector(Cross_Product(D_hat,v_sat))
F_hat = Cross_Product(N_hat,D_hat)

!En = 1000._dp  !1MeV for starters
!En = 10._dp  !10keV for starters
!En = 1._dp  !1keV for starters
!En = 1.E-3_dp  !1eV for starters
En = 0.025E-3_dp  !thermal for starters
Omega_hat2 = -Unit_Vector(N_hat+D_hat-F_hat)  !FROM off to the side and down and backward for starters

Call Prev_Event_Trajectory(sat, Gravity, t2, Omega_hat2*Neutron_Speed(En), Found, r1, v1, tof)
If (Found) Then
    Call Kepler_Gooding(r1,v1,tof,r2,v2)
    DFact = Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)
End If

If (Found) Then
    Print*,Found
    Print*,'These should match'
    Print*,R_center
    Print*,Vector_Length(r1)
    Print*,'This is r,v,tof for the emission'
    Print*,r1
    Print*,v1,Neutron_Energy(v1)
    Print*,tof
    Print*,'Check the propagation...'
    Print*,'These should match'
    Print*,r2
    Print*,r_sat
    Print*,'These should match'
    Print*,v2-v_sat,Neutron_Energy(v2-v_sat)
    Print*,Omega_hat2*Neutron_Speed(En)
    Print*,'D-Factor'
    Print*,DFact
    Print*,Div_Fact_Straight(r1,v1,r2,(/0._dp,0._dp,0._dp/),tof,v_sat)
End If

End Program

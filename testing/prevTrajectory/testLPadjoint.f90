Program testPrevTraj

Use Kinds, Only: dp
Use Satellite_Motion, Only: Satellite_Position_Type
Use Satellite_Motion, Only: Initialize_Satellite_Motion
Use Neutron_Utilities, Only: Neutron_Speed
Use Neutron_Utilities, Only: Neutron_Energy
Use Find_Trajectory, Only: Prev_Event_Trajectory
Use Diverge_Approx, Only: Div_Fact_by_shooting
Use Diverge_Approx, Only: Div_Fact_straight

Use Global, Only: Pi
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
Real(dp) :: mu,omega
Real(dp) :: Omega_hat2(1:3) !direction of neutron arrival in satellite frame
Real(dp) :: En(1:15) ![keV] neutron energy at arrival in satellite frame
Logical :: Found  !flag for whether a trajectory was found
Real(dp) :: r1(1:3),v1(1:3),tof !position,velocity, time of flight defining flight from the surface of the central body
Real(dp) :: v2(1:3)
Real(dp) :: DFact !divergence factor for the intercepting flight from emission to intercept
Integer :: test_unit,stat
Integer :: n_mu_bins,n_omega_bins
Integer :: e,i,j

Write(*,*)

Gravity = .TRUE.
Call Initialize_Satellite_Motion('','Conic_tab ',sat)
t2 = 212640._dp  !This many seconds into mission start
!Satellite DNF basis vectors
Call sat%R_and_V(t2,r_sat,v_sat)
D_hat = -Unit_Vector(r_sat)
N_hat = Unit_Vector(Cross_Product(D_hat,v_sat))
F_hat = Cross_Product(N_hat,D_hat)
En = 1000._dp * (/ 1.e-8_dp,    & 
                 & 2.5e-8_dp,   & 
                 & 1.e-7_dp,    & 
                 & 2.e-7_dp,    & 
                 & 3.e-7_dp,    & 
                 & 4.e-7_dp,    & 
                 & 5.e-7_dp,    & 
                 & 6.e-7_dp,    & 
                 & 8.e-7_dp,    & 
                 & 1.e-6_dp,    & 
                 & 3.16e-6_dp,  & 
                 & 1.e-5_dp,    & 
                 & 1.e-4_dp,    & 
                 & 1.e-3_dp,    & 
                 & 1.e-2_dp     /)
n_mu_bins = 2000
n_omega_bins = 180
Open( NEWUNIT = test_unit , FILE = 'prevLPemissionPoints.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE' ,  IOSTAT = stat )
Write(test_unit,'(16ES25.16E3)') t2,r_sat,v_sat,D_hat,N_hat,F_hat
Do e = 1,15
    Do i = 0,n_mu_bins-1
        mu = -1._dp + (1._dp + Real(i,dp)*2._dp) / Real(n_mu_bins,dp)
        Do j = 0,n_omega_bins-1
            omega = (Pi + Real(i,dp)*2._dp*Pi) / Real(n_omega_bins,dp)
            Omega_hat2 = mu * D_hat + Sqrt(1._dp - mu**2) * (Cos(omega) * N_hat + Sin(omega) * F_hat)
            Call Prev_Event_Trajectory(sat, Gravity, t2, -Omega_hat2*Neutron_Speed(En(e)), Found, r1, v1, tof)
            If (found) Then
                v2 = -Omega_hat2*Neutron_Speed(En(e)) + v_sat
                DFact = Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)
                Write(test_unit,'(13ES25.16E3)') En(e),Omega_hat2,tof,r1,v1,Neutron_Energy(v1),DFact
            End If
        End Do
    End Do
End Do
Close(test_unit)

End Program

Module Satellite_Motion
    
    Use Kinds, Only: dp
    Implicit none
    
! Cartesian coordinate basis vectors are inertial
!   and are aligned with the rotational position of the Earth at time t = 0:
!   XHat = (1,0,0)  Points toward the equator at 0 deg Longitude (Greenwich meridian)
!   YHat = (0,1,0)  Points toward the equator at 90 deg E Longitude
!   ZHat = (0,0,1)  Points toward the north pole
! This system is used so that all the coordinate transformations
!   that are needed can be done once,
!   in establishing the satellites orbital parameters in this system,
!   and are not required while tracking neutrons in the simulation.
    
    Private
    Public :: Satellite_Position_Type
    Public :: Initialize_Satellite_Motion
    
    Type Satellite_Position_Type
        Real(dp) :: r0(1:3)  !position vector at t=0
        Real(dp) :: v0(1:3)  !velocity vector at t=0
        Real(dp) :: rp
        Real(dp) :: vp
        Real(dp) :: per
        Logical :: is_stationary
        Logical :: is_conic
    Contains
        Procedure, Pass :: R_and_V => R_V_Satellite
        Procedure, Pass :: R => R_Satellite
        Procedure, Pass :: Cleanup => Cleanup_Satellite_position
    End Type

Contains

Subroutine Initialize_Satellite_Motion(motion_type,sat)
    Use Kinds, Only: dp
    Use Global, Only: std_grav_parameter
    Use Global, Only: rot_Earth
    Use Global, Only: Z_hat
    Use Global, Only: R_earth
    Use Astro_Utilities, Only: Radius_of_Perigee
    Use Astro_Utilities, Only: Velocity_of_Perigee
    Use Astro_Utilities, Only: Period
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Unit_Vector
    Use Utilities, Only: Cross_Product
    Use Utilities, Only: Cube_Root
    Use FileIO_Utilities, Only: Output_Message
    Implicit None
    Character(10), Intent(In) :: motion_type
    Type(Satellite_Position_Type), Intent(InOut) :: sat
    
    Select Case(motion_type)
        Case('Stationary')
            !sat%motion_index = Sat_Motion_stationary
            sat%is_stationary = .TRUE.
            sat%is_conic = .FALSE.
            sat%rp = Vector_Length(sat%r0)
            sat%vp = 0._dp
        Case('Linear')
            !sat%motion_index = Sat_Motion_linear
            sat%is_stationary = .FALSE.
            sat%is_conic = .FALSE.
            If (Dot_Product(sat%r0,sat%v0) .LT. 0._dp) Then
                sat%rp = -Vector_Length(sat%r0) * Dot_Product(sat%r0,sat%v0)
            Else
                sat%rp = Vector_Length(sat%r0)
            End If
            sat%vp = Vector_Length(sat%v0)
        Case('Conic')
            !sat%motion_index = Sat_Motion_conic
            sat%is_stationary = .FALSE.
            sat%is_conic = .TRUE.
            sat%rp = Radius_of_Perigee(sat%r0,sat%v0)
            sat%vp = Velocity_of_Perigee(sat%r0,sat%v0)
            sat%per = Period(sat%r0,sat%v0)
        Case('GeoStat')
            !sat%motion_index = Sat_Motion_conic
            sat%is_stationary = .FALSE.
            sat%is_conic = .TRUE.
            sat%r0(3) = 0._dp  !make sure r0 is in equatorial plane
            sat%r0 = Unit_Vector(sat%r0) * Cube_Root(std_grav_parameter / rot_Earth**2)  !make sure r0 has correct magnitude
            sat%v0 = Unit_Vector(Cross_Product(Z_hat,sat%r0)) * Cube_Root(std_grav_parameter / rot_Earth**2) * rot_Earth  !set v0
            sat%rp = Vector_Length(sat%r0)
            sat%vp = Velocity_of_Perigee(sat%r0,sat%v0)
            sat%per = Period(sat%r0,sat%v0)
        Case Default
            Call Output_Message('ERROR:  Satellite_Motion: Initialize_Satellite_Motion:  Unknown motion type.',kill=.TRUE.)
    End Select
End Subroutine Initialize_Satellite_Motion

Subroutine R_V_Satellite(s,t,r,v)
    Use Kinds, Only: dp
    Use Astro_Utilities, Only: Kepler_Gooding
    Implicit none
    Class(Satellite_Position_Type), Intent(In) :: s
    Real(dp), Intent(In) :: t
    Real(dp), Intent(Out) :: r(1:3),v(1:3)
    
    If (s%is_conic) Then
        Call Kepler_Gooding(s%r0,s%v0,t,r,v)
    Else If (s%is_stationary) Then
        r = s%r0
        v = 0._dp
    Else !s%is_linear
        r = s%r0 + t*s%v0
        v = s%v0
    End If
End Subroutine R_V_Satellite

Function R_Satellite(s,t) Result(r)
    Use Kinds, Only: dp
    Use Astro_Utilities, Only: Kepler_Gooding
    Implicit none
    Real(dp) :: r(1:3)
    Class(Satellite_Position_Type), Intent(In) :: s
    Real(dp), Intent(In) :: t
    Real(dp) :: v(1:3)
    
    If (s%is_conic) Then
        Call Kepler_Gooding(s%r0,s%v0,t,r,v)
    Else If (s%is_stationary) Then
        r = s%r0
    Else !s%is_linear
        r = s%r0 + t*s%v0
    End If
End Function R_Satellite

Subroutine Cleanup_Satellite_Position(s)
    Use Kinds, Only: dp
    Implicit None
    Class(Satellite_Position_Type), Intent(InOut) :: s
    
    s%r0 = 0._dp
    s%v0 = 0._dp
End Subroutine Cleanup_Satellite_Position
    
End Module Satellite_Motion

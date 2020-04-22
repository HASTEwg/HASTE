!-------------------------------------------------------------------------------
!   Copyright (C) 2017  Whitman T. Dailey
!   
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License version 3 as 
!   published by the Free Software Foundation.
!   
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!   
!   You should have received a copy of the GNU General Public License
!   along with this program.  If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------
Module Sources
    
    Use Kinds, Only: dp
    Implicit None
    Private
    Public :: Source_Type
    Public :: Setup_Source
    Public :: Celest_to_XYZ
    Public :: Write_Source
    
    Type :: Tab_1d_Type  !stores a 1-d tabulated distribution
        Integer :: n
        Real(dp), Allocatable :: b(:)  !has dimension 0:n, list of bin boundaries
        Real(dp), Allocatable :: p(:)  !has dimension 1:n, list of bin intensities (normalized to sum to 1)
    End Type
    
    Type, Extends(Tab_1d_Type) :: Tab_2d_Type  !stores a 2-d tabulated distribution
        Type(Tab_1d_Type), Allocatable :: d2(:)  !has dimesion 1:n, list of lists for second dimension
    End Type
    
    Type :: Source_Type
        Integer :: geom_index
        Real(dp) :: rad  ![km] Radius of spherical or albedo source
        Real(dp) :: r(1:3)     ![km] cartesian (x,y,z) position of source
        Real(dp) :: v(1:3)     ![km/s] velocity of source
        Real(dp) :: d(1:3)     ![1 km] unit vector in direction of 'front' of source
        Real(dp) :: z
        Real(dp) :: big_r
        Real(dp) :: speed
        Logical :: has_velocity  !T indicates source has velocity to be added to new neutrons
        Logical :: exoatmospheric  !T indicates source is outside atmosphere, F indicates source IN the atmosphere
        Integer :: E_dist_index,A_dist_index
        Logical :: E_A_dist_coupled  !T indicates energy and direction (in the rest frame of the source) are coupled
        Logical :: aniso_dist  !T indicates non-uniform direction dist of emitted particles (in the rest frame of the source)
        Real(dp) :: E_high   ![keV] energy description
                             !E_high=E_low for 'Point', specifies range for 'Uniform', specifies cutoff energies for 'Watt235'
        Real(dp) :: E_low
        Logical :: point_time
        Real(dp) :: t_start
        Real(dp) :: delta_t
        Real(dp) :: A_hat(1:3),B_hat(1:3),C_hat(1:3)  !basis vectors for distribution of emission angles for direct contributions
        Type(Tab_1d_Type) :: Etab
        Type(Tab_1d_Type) :: Atab
        Type(Tab_2d_Type) :: EAtab
        Logical :: source_data
        Integer :: max_source_data
        Integer :: source_data_c
        Integer :: source_unit
    Contains
        Procedure, Pass :: sample => Sample_Source
    End Type

    !Integer designator for source geometry choice, can have value equal to one of the following parameters
    Integer, Parameter :: source_geom_point = 91
    Integer, Parameter :: source_geom_Sphere_S = 93
    Integer, Parameter :: source_geom_Sphere_V = 95
    Integer, Parameter :: source_geom_Sphere_V_unif = 97
    Integer, Parameter :: source_geom_Albedo = 99

    !Integer designator for source energy distribution choice, can have value equal to one of the following parameters
    Integer, Parameter :: source_E_dist_Line = 71
    Integer, Parameter :: source_E_dist_Unif = 73
    Integer, Parameter :: source_E_dist_Watt235 = 75
    Integer, Parameter :: source_E_dist_Type3 = 77
    Integer, Parameter :: source_E_dist_Type5 = 79
    Integer, Parameter :: source_E_dist_Type8 = 81
    Integer, Parameter :: source_E_dist_Type13 = 83
    Integer, Parameter :: source_E_dist_tab = 85

    !Integer designator for source angular distribution choice, can have value equal to one of the following parameters
    Integer, Parameter :: source_A_dist_iso = 72
    Integer, Parameter :: source_A_dist_top = 74
    Integer, Parameter :: source_A_dist_side = 76
    Integer, Parameter :: source_A_dist_bot = 78
    Integer, Parameter :: source_A_dist_tab = 80    

    Integer, Parameter :: source_EA_dist_tab = 70

Contains

Function Setup_Source(setup_file_name,run_file_name,source_file_name,R_top_atm) Result(s)
    Use Kinds, Only: dp
    Use Global, Only: Z_hat,X_hat,Y_hat
    Use Global, Only: Rc => R_center
    Use Utilities, Only: Unit_Vector
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Cross_Product
    Use FileIO_Utilities, Only: Worker_Index
    Use FileIO_Utilities, Only: Output_Message
    Implicit None
    Type(Source_Type) :: s
    Character(*), Intent(In) :: setup_file_name,run_file_name,source_file_name
    Real(dp), Intent(In) :: R_top_atm
    Real(dp) :: r_source
    Real(dp) :: x_source,y_source,z_source  ![km]  x,y,z coordinates of source
    Real(dp) :: declination_source,right_ascension_source
    Real(dp) :: v_E_source,v_N_source,v_U_source
    Real(dp) :: d_E_source,d_N_source,d_U_source
    Real(dp) :: E_high,E_low
    Real(dp) :: t_start,t_stop
    Character(10) :: source_E_dist  !Line,Uniform,Watt235,Type03,Type05,Type08,Type13,Egroups,EAgroups
    Character(10) :: source_A_dist  !Iso,Top,Side,Bottom,Agroups,EAgroups
    Character(10) :: source_geometry,position_geometry
    Character(10) :: source_t_dist
    Logical :: collect_source_data
    Integer :: source_data_limit
    Integer :: setup_unit,stat
    Real(dp) :: E_hat(1:3),N_hat(1:3),U_hat(1:3)
    
    NameList /NeutronSourceList/ source_geometry,r_source, & 
                                 & position_geometry,x_source,y_source,z_source, &
                                 & declination_source,right_ascension_source, &
                                 & v_E_source,v_N_source,v_U_source, &
                                 & d_E_source,d_N_source,d_U_source, &
                                 & source_E_dist,E_high,E_low,source_A_dist, &
                                 & source_t_dist,t_start,t_stop, & 
                                 & collect_source_data,source_data_limit
    
    Open(NEWUNIT = setup_unit , FILE = setup_file_name , STATUS = 'OLD' , ACTION = 'READ' , IOSTAT = stat)
    If (stat .NE. 0) Call Output_Message( 'ERROR:  Neutron_Source: Initialize_Neutron_Source:  File open error, ' & 
                                        & //setup_file_name//', IOSTAT=',stat,kill=.TRUE. )
    Read(setup_unit,NML = NeutronSourceList)
    Close(setup_unit)
    Select Case(source_geometry)
        Case('Point')
            s%geom_index = source_geom_point
            s%rad = 0._dp
        Case('Sphere_S')
            s%geom_index = source_geom_Sphere_S
            s%rad = r_source
        Case('Sphere_V')
            s%geom_index = source_geom_Sphere_V
            s%rad = r_source
        Case('Sphere_Vu')
            s%geom_index = source_geom_Sphere_V_unif
            s%rad = r_source
        Case('Albedo')
            s%geom_index = source_geom_Albedo
            s%rad = Rc + r_source
        Case Default
            Call Output_Message('ERROR:  Sources: Setup_Source:  Unknown source geometry.',kill=.TRUE.)
    End Select
    Select Case(position_geometry)
        Case('Celestial')
            s%r = Celest_to_XYZ(z_source,right_ascension_source,declination_source)
        Case('Cartesian')
            s%r = (/ x_source, y_source, z_source /)
        Case Default
            Call Output_Message('ERROR:  Sources: Setup_Source:  Unknown position geometry.',kill=.TRUE.)
    End Select
    If (s%geom_index .NE. source_geom_Albedo) Then
        s%big_r = Vector_Length(s%r)
        s%z = s%big_r - Rc
    Else !albedo source is always centered at the origin
        s%r = 0._dp
        s%big_r = 0._dp
        s%z = r_source
    End If
    !check for exoatmospheric source
    If (s%geom_index .NE. source_geom_Albedo) Then
        If (s%big_r .LT. R_top_atm) Then
            s%exoatmospheric = .FALSE.
        Else
            s%exoatmospheric = .TRUE.
        End If
    Else  !special criteria for Albedo source geometry
        If (s%rad .LT. R_top_atm) Then
            s%exoatmospheric = .FALSE.
        Else
            s%exoatmospheric = .TRUE.
        End If
    End If
    !Set source velocity
    s%v = (/ v_E_source, &
           & v_N_source, &
           & v_U_source /)
    If (Any(s%v .NE. 0._dp) .AND. s%geom_index.NE.source_geom_Albedo) Then
        s%has_velocity = .TRUE.
        U_hat = Unit_Vector(s%r)
        E_hat = Unit_Vector(Cross_Product(Z_hat,U_hat))
        N_hat = Cross_Product(U_hat,E_hat)
        s%v = E_hat * Dot_Product(s%v,E_hat) + &
            & N_hat * Dot_Product(s%v,N_hat) + &
            & U_hat * Dot_Product(s%v,U_hat)
        s%speed = Vector_Length(s%v)
        s%A_hat = Unit_Vector(s%v)
        If (Abs(Dot_Product(s%A_hat,Z_Hat)) .LT. 0.5_dp) Then
            s%B_hat = Unit_Vector(Cross_Product(Z_Hat,s%A_hat))
        Else
            s%B_hat = Unit_Vector(Cross_Product(X_Hat,s%A_hat))
        End If
        s%C_hat = Cross_Product(s%A_hat,s%B_hat)
    Else  !stationary source or albedo source
        s%has_velocity = .FALSE.
        s%v = 0._dp
        s%speed = 0._dp
        s%A_hat = Z_hat
        s%B_hat = X_hat
        s%C_hat = Y_hat
    End If
    !Set source direction
    s%d = (/ d_E_source, &
           & d_N_source, &
           & d_U_source /)
    If (Any(s%d .NE. 0._dp) .AND. s%geom_index.NE.source_geom_Albedo) Then
        U_hat = Unit_Vector(s%r)
        E_hat = Unit_Vector(Cross_Product(Z_hat,U_hat))
        N_hat = Cross_Product(U_hat,E_hat)
        s%d = E_hat * Dot_Product(s%d,E_hat) + &
            & N_hat * Dot_Product(s%d,N_hat) + &
            & U_hat * Dot_Product(s%d,U_hat)
    Else
        s%d = Z_hat
    End If
    !Set energy and angle distribution properties
    If (source_A_dist.EQ.'EAgroups' .OR. source_E_dist.EQ.'EAgroups') Then  !Coupled energy-angle distribution
        s%E_A_dist_coupled = .TRUE.
        s%A_dist_index = source_EA_dist_tab
        s%E_dist_index = source_EA_dist_tab
        s%aniso_dist = .TRUE.
        !UNDONE Coupled 2-d tabulated enegy-angle distribution
        Call Output_Message('ERROR:  Sources: Setup_Source: Coupled Energy-Angle distribution not yet implemented.',kill=.TRUE.)
    Else  !independednt energy and angle distributions
        s%E_A_dist_coupled = .FALSE.
        Select Case (source_A_dist)
            Case ('Iso')
                s%A_dist_index = source_A_dist_Iso
                s%aniso_dist = .FALSE.
            Case ('Top')
                s%A_dist_index = source_A_dist_Top
                s%aniso_dist = .TRUE.
            Case ('Side')
                s%A_dist_index = source_A_dist_Side
                s%aniso_dist = .TRUE.
            Case ('Bottom')
                s%A_dist_index = source_A_dist_Bot
                s%aniso_dist = .TRUE.
            !UNDONE Tabulated angular source distribution type
            Case Default
                Call Output_Message( 'ERROR:  Sources: Setup_Source: Undefined source angular distribution',kill=.TRUE. )
        End Select
        Select Case (source_E_dist)
            Case ('Line')
                s%E_dist_index = source_E_dist_Line
                s%E_high = E_high
                s%E_low = E_high
            Case ('Uniform')
                s%E_dist_index = source_E_dist_Unif
                s%E_high = E_high
                s%E_low = E_low
            Case ('Watt235')
                s%E_dist_index = source_E_dist_Watt235
                s%E_high = E_high
                s%E_low = 0._dp
            Case ('Egroups')
                s%E_dist_index = source_E_dist_tab
            !UNDONE Source distributions for Type 3, 5, 8, and 13
            Case Default
                Call Output_Message( 'ERROR:  Sources: Setup_Source: Undefined source energy distribution',kill=.TRUE.)
        End Select
    End If
    If (All(s%d .EQ. 0._dp) .AND. s%aniso_dist) Then  !no source direction is specified, but an anisotropic distribution is selected
        !source direction is needed for anisotropic angular distributions of emitted particles
        Call Output_Message('ERROR:  Sources: Setup_Source: Source forward direction must be specified for angular dist', & 
                           & kill=.TRUE.)
    End If
    Select Case (source_t_dist)
        Case ('Point')
            s%point_time = .TRUE.
            s%t_start = t_start
            s%delta_t = 0._dp
        Case ('Uniform')
            s%point_time = .FALSE.
            s%t_start = t_start
            s%delta_t = t_stop - t_start
        Case Default
            Call Output_Message( 'ERROR:  Sources: Setup Source: Undefined source time distribution',kill=.TRUE.)
    End Select
    If (Worker_Index() .EQ. 1) Then
        !set up source data collection
        If (collect_source_data) Then
            s%source_data = .TRUE.
            s%max_source_data = source_data_limit
            s%source_data_c = 0
            Open( NEWUNIT = S%source_unit , FILE = source_file_name , STATUS = 'REPLACE' , ACTION = 'WRITE' , IOSTAT = stat)
        Else
            s%source_data = .FALSE.
        End If
        !write out processed source information
        Open(NEWUNIT = setup_unit , FILE = run_file_name , STATUS = 'OLD' , ACTION = 'WRITE' , POSITION = 'APPEND' , IOSTAT = stat)
        If (stat .NE. 0) Call Output_Message( 'ERROR:  Sources: Setup_Source:  File open error, '//run_file_name// & 
                                            & ', IOSTAT=',stat,kill=.TRUE. )
        Write(setup_unit,NML = NeutronSourceList)
        Write(setup_unit,*)
        Close(setup_unit)
    Else
        s%source_data = .FALSE.
    End If
End Function Setup_Source

Function Celest_to_XYZ(alt,RA_deg,DEC_deg) Result(r)
    Use Kinds, Only: dp
    Use Global, Only: Pi
    Use Global, Only: Rc => R_center
    Implicit None
    Real(dp) :: r(1:3)
    Real(dp), Intent(In) :: alt,RA_deg,DEC_deg
    Real(dp) :: RA,DEC
    
    RA = RA_deg * Pi / 180._dp
    DEC = DEC_deg * Pi / 180._dp
    !convert celestial to cartesian soordinates
    r = (/  (Rc + alt) * Cos(RA) * Cos(DEC), &
         & -(Rc + alt) * Sin(RA) * Cos(DEC), &
         &  (Rc + alt) * Sin(DEC) /)
End Function Celest_to_XYZ

Subroutine Sample_Source(s,RNG,n)
    Use Kinds, Only: dp
    Use Global, Only: R_center
    Use Random_Numbers, Only: RNG_Type
    Use Neutron_Scatter, Only: Neutron_Type
    Use Random_Directions, Only: Isotropic_Omega_hat
    Use Random_Directions, Only: Isotropic_Azimuth
    Use Random_Directions, Only: mu_omega_2_OmegaHat
    Use Utilities, Only: Cube_Root
    Use Utilities, Only: Unit_Vector
    Use Utilities, Only: Vector_Length
    Use Neutron_Utilities, Only: Neutron_Speed
    Use Neutron_Utilities, Only: Neutron_Energy
    Use FileIO_Utilities, Only: Output_Message
    Implicit None
    Class(Source_Type), Intent(InOut) :: s
    Type(RNG_type), Intent(InOut) :: RNG
    Type(Neutron_Type), Intent(InOut) :: n
    Real(dp) :: mu,omega
    Real(dp) :: v(1:3)
    Real(dp), Parameter :: one_third = 1._dp / 3._dp
    Real(dp), Parameter :: two_thirds = 2._dp / 3._dp
    
    !Sample emission time
    If (s%point_time) Then
        n%t = s%t_start
    Else
        n%t = s%t_start + RNG%Get_Random() * s%delta_t
    End If
    !Sample location
    Select Case (s%geom_index)
        Case (source_geom_point)
            n%r = s%r
        Case (source_geom_Sphere_S)
            n%r = s%r + s%rad * Isotropic_Omega_hat(RNG)
        Case (source_geom_Sphere_V)
            n%r = s%r + RNG%Get_Random() * s%rad * Isotropic_Omega_hat(RNG)
        Case (source_geom_Sphere_V_unif)
            n%r = s%r + Cube_root(RNG%Get_Random()) * s%rad * Isotropic_Omega_hat(RNG)
        Case (source_geom_Albedo)
            n%r = s%rad * Isotropic_Omega_hat(RNG)
    End Select
    If (.NOT.s%point_time .AND. s%has_velocity) Then  !update source position based on source velocity and elapsed time
        !N2H This implementation assumes linear motion of the source (no other motion types are presently supported)
        n%r = n%r + n%t * s%v
    End If
    !Sample Energy and Direction
    If (s%E_A_dist_coupled) Then  !coupled energy-angle distribution
        Call Output_Message( 'ERROR:  Sources: Sample_Source: Coupled energy-angle dist not yet implemented',kill=.TRUE.)
    Else  !independent energy and angle distributions
        Select Case (s%E_dist_index)
            Case (source_E_dist_Line)
                n%E0ef = s%E_high
            Case (source_E_dist_Unif)
                n%E0ef = Sample_Uniform(RNG,s%E_low,s%E_high)
            Case (source_E_dist_Watt235)
                n%E0ef = Sample_Watt235(RNG,s%E_high)
        End Select
        Select Case (s%A_dist_index)
            Case (source_A_dist_Iso)
                n%Omega_hat = Isotropic_Omega_Hat(RNG)
            Case (source_A_dist_top)
                mu = -(one_third + RNG%Get_random() * two_thirds)
                omega = Isotropic_Azimuth(RNG)
                n%Omega_hat = mu_omega_2_OmegaHat(mu,omega)
            Case (source_A_dist_side)
                mu = -one_third + RNG%Get_random() * two_thirds
                omega = Isotropic_Azimuth(RNG)
                n%Omega_hat = mu_omega_2_OmegaHat(mu,omega)
            Case (source_A_dist_bot)
                mu = one_third + RNG%Get_random() * two_thirds
                omega = Isotropic_Azimuth(RNG)
                n%Omega_hat = mu_omega_2_OmegaHat(mu,omega)
        End Select
    End If
    n%weight = 1._dp
    If (s%source_data) Then
        Write(s%source_unit,'(9ES27.16E3)') n%t,n%r,n%E0ef,n%Omega_hat,n%weight
        s%source_data_c = s%source_data_c + 1
        If (s%source_data_c .GE. s%max_source_data) Then
            Close(s%source_unit)
            s%source_data = .FALSE.
        End If
    End If
    !Populate derived neutron properties
    n%s0ef = Neutron_Speed(n%E0ef)
    If (s%has_velocity) Then  !move energy & direction into ECI frame
        v = n%Omega_hat * n%s0ef + s%v
        n%E = Neutron_Energy(v)
        n%Omega_hat = Unit_Vector(v)
    Else  !otherwise, emission frame is ECI frame
        n%E = n%E0ef
    End If
    n%zeta = Dot_Product(Unit_Vector(n%r),n%Omega_hat)
    n%big_r = Vector_Length(n%r)
    n%Z = n%big_r - R_center
End Subroutine Sample_Source

Function Sample_Uniform(RNG,E_min,E_max) Result(E)
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp) :: E  ![keV]
    Type(RNG_Type), Intent(InOut) :: RNG  !random number generator
    Real(dp), Intent(In) :: E_min,E_max  ![keV]  energy range

    E = E_min + RNG%Get_Random() * (E_max - E_min)
End Function Sample_Uniform

Function Sample_Watt235(RNG,E_max) Result(E)
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp) :: E  ![keV]
    Type(RNG_Type), Intent(InOut) :: RNG  !random number generator
    Real(dp), Intent(In) :: E_max  ![keV]  max energy allowed
    Real(dp) :: w

    !N2H Investigate faster and/or alternate watt sampling schemes
    Do
        E = E_max * RNG%Get_Random()
        w = 1.26463489196562161942_dp * Watt235func(E)
        If (w .GT. RNG%Get_Random()) Exit
    End Do
End Function Sample_Watt235

Function Watt235func(E) Result(w)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: w
    Real(dp), Intent(In) :: E
    
    w = Exp(-0.001036_dp * E) * Sinh(0.047853944456021595545_dp * Sqrt(E))
End Function Watt235func

Subroutine Write_Source(s,file_name)
    Use Global, Only: Pi
    Use Global, Only: Rc => R_center
    Use Utilities, Only: Vector_Length
    Use FileIO_Utilities, Only: Output_Message
    Use FileIO_Utilities, Only: half_dash_line
    Implicit None
    Type(Source_Type), Intent(In) :: s
    Character(*), Intent(In) :: file_name
    Integer :: unit,stat
    
    Open(NEWUNIT = unit , FILE = file_name , STATUS = 'UNKNOWN' , ACTION = 'WRITE' , POSITION = 'APPEND' , IOSTAT = stat)
    If (stat .NE. 0) Call Output_Message( 'ERROR:  Sources: Write_Source:  File open error, '//file_name// & 
                                        & ', IOSTAT=',stat,kill=.TRUE.)
    Write(unit,'(A)') half_dash_line
    Write(unit,'(A)') 'SOURCE INFORMATION'
    Write(unit,'(A)') half_dash_line
    Write(unit,'(A)') '  Geometry:'
    Select Case (s%geom_index)
        Case (source_geom_point)
            Write(unit,'(A)')             '    Point'
        Case (source_geom_Sphere_S)
            Write(unit,'(A,ES24.16E3,A)') '    Sphere-Surface w/ r = ',s%rad,' km'
        Case (source_geom_Sphere_V)
            Write(unit,'(A,ES24.16E3,A)') '    Sphere-Volume w/ r = ',s%rad,' km'
        Case (source_geom_Sphere_V_unif)
            Write(unit,'(A,ES24.16E3,A)') '    Sphere-Uniform Volume w/ r = ',s%rad,' km'
        Case (source_geom_Albedo)
            Write(unit,'(A,ES24.16E3,A)') '    Albedo w/ r = ',s%rad,' km'
    End Select
    Write(unit,'(A)') '  Position:'
    Write(unit,'(A,ES24.16E3,A)') '    x = ',s%r(1),' km'
    Write(unit,'(A,ES24.16E3,A)') '    y = ',s%r(2),' km'
    Write(unit,'(A,ES24.16E3,A)') '    z = ',s%r(3),' km'
    If (s%geom_index .NE. source_geom_Albedo) Then
        Write(unit,'(A,ES24.16E3,A)') '    Right Ascension = ', & 
                                    & Acos(s%r(1)/(Vector_Length(s%r)*Cos(Asin(s%r(3)/Vector_Length(s%r))))) / (Pi/180._dp), & 
                                    & ' deg'
        Write(unit,'(A,ES24.16E3,A)') '    Declination     = ',Asin(s%r(3)/Vector_Length(s%r)) / (Pi/180._dp),' deg'
    Else
        Write(unit,'(A,ES24.16E3,A)') '    Right Ascension = ',0._dp,' deg'
        Write(unit,'(A,ES24.16E3,A)') '    Declination     = ',0._dp,' deg'
    End If
    Write(unit,'(A,ES24.16E3,A)') '    Geometric Alt   = ',s%Z,' km'
    If (s%exoatmospheric) Then
        Write(unit,'(A)') '    Source is EXOatmospheric'
    Else
        Write(unit,'(A)') '    Source is ENDOatmospheric'
    End If
    Write(unit,'(A)') '  Velocity:'
    Write(unit,'(A,ES24.16E3,A)') '    x = ',s%v(1),' km/s'
    Write(unit,'(A,ES24.16E3,A)') '    y = ',s%v(2),' km/s'
    Write(unit,'(A,ES24.16E3,A)') '    z = ',s%v(3),' km/s'
    Write(unit,*)
    Write(unit,'(A)') '  Energy Distribution:'
    Select Case (s%E_dist_index)
        Case (source_E_dist_Line)
            Write(unit,'(A,ES24.16E3,A)') '    Line, ',s%E_high,' keV'
        Case (source_E_dist_Unif)
            Write(unit,'(A,ES24.16E3,A,ES24.16E3,A)') '    Uniform, ',s%E_low,' keV to ',s%E_high,' keV'
        Case (source_E_dist_Watt235)
            Write(unit,'(A,ES24.16E3,A)') '    Watt-235, ',s%E_high,' keV max'
        Case (source_E_dist_Type3)
            Write(unit,'(A)') '    Type 3, fission'
        Case (source_E_dist_Type5)
            Write(unit,'(A)') '    Type 5, boosted fission'
        Case (source_E_dist_Type8)
            Write(unit,'(A)') '    Type 8, thermonuclear'
        Case (source_E_dist_Type13)
            Write(unit,'(A)') '    Type 13, thermonuclear, enhanced radiation'
        Case Default
            Call Output_Message( 'ERROR:  Sources: Write_Source:  Undefined source energy distribution',kill=.TRUE.)
    End Select
    Write(unit,*)
    Write(unit,'(A)') '  Angular Distribution:'
    Select Case (s%A_dist_index)
        Case (source_A_dist_iso)
            Write(unit,'(A)') '    Isotropic'
        Case (source_A_dist_top)
            Write(unit,'(A)') '    Upward 1/3'
        Case (source_A_dist_side)
            Write(unit,'(A)') '    Lateral 1/3'
        Case (source_A_dist_bot)
            Write(unit,'(A)') '    Downward 1/3'
        Case (source_A_dist_tab)
            Write(unit,'(A)') '    Tabular'
        Case Default
            Call Output_Message( 'ERROR:  Sources: Write_Source:  Undefined source angular distribution',kill=.TRUE.)
    End Select
    Write(unit,*)
    Write(unit,'(A)') '  Time Distribution:'
    If (s%point_time) Then
        Write(unit,'(A)') '    Point'
    Else
        Write(unit,'(A,ES24.16E3,A,ES24.16E3,A)') '    Uniform from ',s%t_start,' to ',s%t_start+s%delta_t,' sec'
    End If
    Write(unit,*)
    Write(unit,*)
    Close(unit)
End Subroutine Write_Source

End Module Sources

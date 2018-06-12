Module Neutron_Utilities
    
    Implicit None
    Private
    Public :: Neutron_Energy
    Public :: Neutron_Speed

    Interface Neutron_Energy
        Module Procedure Neutron_Energy_from_Speed
        Module Procedure Neutron_Energy_from_Velocity
    End Interface
    
Contains

Function Neutron_Speed(E) Result(v)
    Use Kinds, Only: dp
    Use Global, Only: neutron_speed_conversion
    Implicit None
    Real(dp):: v                ! neutron speed  [km/s]
    Real(dp), Intent(In):: E    ! neutron energy [keV]

    v = neutron_speed_conversion * Sqrt(E)
End Function Neutron_Speed

Function Neutron_Energy_From_Speed(v) Result(E)
    Use Kinds, Only: dp
    Use Global, Only: neutron_speed_conversion
    Implicit None
    Real(dp):: E                ! neutron energy [keV]
    Real(dp), Intent(In):: v    ! neutron speed  [km/s]
    
    E = (v / neutron_speed_conversion)**2
End Function Neutron_Energy_From_Speed

Function Neutron_Energy_From_Velocity(v) Result(E)
    Use Kinds, Only: dp
    Use Utilities, Only: Vector_Length
    Implicit None
    Real(dp):: E                ! neutron energy [keV]
    Real(dp), Intent(In):: v(1:3)    ! neutron velocity  [km/s]
    
    E = Neutron_Energy_From_Speed(Vector_Length(v))
End Function Neutron_Energy_From_Velocity

End Module Neutron_Utilities
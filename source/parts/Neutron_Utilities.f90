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

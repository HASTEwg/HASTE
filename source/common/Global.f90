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
Module Global
    
    Use Kinds, Only: dp
    Implicit None
    Public
    
! Mathematical Constants
    Real(dp), Parameter :: pi = 3.141592653589793238462643383279502884_dp  !36 digits carried in case needed for quad precision
    Real(dp), Parameter :: TwoPi = 2._dp * pi
    Real(dp), Parameter :: FourPi = 4._dp * pi
    Real(dp), Parameter :: HalfPi = 0.5_dp * pi
    Real(dp), Parameter :: inv_TwoPi = 0.5_dp / pi
    Real(dp), Parameter :: inv_FourPi = 0.25_dp / Pi
    Real(dp), Parameter :: SqrtPi = Sqrt(pi)
    Real(dp), Parameter :: X_Hat(1:3) = (/ 1._dp , 0._dp , 0._dp /)
    Real(dp), Parameter :: Y_Hat(1:3) = (/ 0._dp , 1._dp , 0._dp /)
    Real(dp), Parameter :: Z_Hat(1:3) = (/ 0._dp , 0._dp , 1._dp /)
    Complex(dp), Parameter :: imag_i = ( 0._dp , 1._dp )

! Physical Constants and Unit Conversions
    Real(dp), Parameter :: r2deg = 180._dp / Pi
    Real(dp), Parameter :: deg2r = Pi / 180._dp
    Real(dp), Parameter :: m0c2 = 510.998928_dp  ![keV] Rest mass energy of an electron
    Real(dp), Parameter :: speed_of_light = 299792.458_dp ![km/s]
    Real(dp), Parameter :: keV_per_Joule = 6.241509126E15_dp  ![keV/J]
    Real(dp), Parameter :: k_Boltzmann = 1.38064852E-23_dp  ![J/K] Boltzmann constant
    Real(dp), Parameter :: nA = 6.022140857E23_dp  ![1/mol] Avagadro's number
    Real(dp), Parameter :: h_Planck = 6.62607015E-34_dp  ![J*s] Planck constant
    Real(dp), Parameter :: h_bar_Planck = h_Planck / TwoPi  ![J*s] reduced Planck constant

!  Sun and Gravitation
    Real(dp), Parameter :: R_sun = 695700._dp     ![km] Mean volumetric radius of the sun
    Real(dp), Parameter :: grav_parameter_sun = 132712.440018E6_dp  ![km^3 / s^2]  standard gravitational parameter of Earth
    Real(dp), Parameter :: Escape_speed_sun = &  !minimum speed to escape the sun's gravitational influence from the surface
                               & Sqrt(2._dp * grav_parameter_sun / R_sun)  ![km/s]
    Real(dp), Parameter :: g0_sun = grav_parameter_sun / R_sun  ![km/s^2] solar acceleration due to gravity
    Real(dp), Parameter :: Sun_mass = 1988500.E24_dp  ![kg]
    Real(dp), Parameter :: Sun_SOI = HUGE(Sun_SOI)  ![km]

!  Earth and Gravitation
    Real(dp), Parameter :: R_Earth = 6371.00079_dp     ![km] Mean volumetric radius of earth
    Real(dp), Parameter :: std_grav_parameter = 398600.4418_dp  ![km^3 / s^2]  standard gravitational parameter of Earth
    Real(dp), Parameter :: Escape_speed = &  !minimum speed to escape earth's gravitational influence from the surface
                               & Sqrt(2._dp * std_grav_parameter / R_Earth)  ![km/s]
    Real(dp), Parameter :: rot_Earth = 7.292115E-5_dp  ![rad/s] Mean rotational speed of the Earth (radians per SIDEREAL second)
    Real(dp), Parameter :: Sid_Day_sec = 86164.09053_dp  !number of seconds in a sidereal day
    Real(dp), Parameter :: g0_earth = 9.80665E-3_dp  ![km/s^2] standard acceleration due to gravity !std_grav_parameter / R_earth
    Real(dp), Parameter :: Earth_mass = 5.9724E24_dp  ![kg]
    Real(dp), Parameter :: Earth_orbit_a = 149.6E6_dp ![km]
    Real(dp), Parameter :: Earth_SOI = Earth_orbit_a * (Earth_mass / Sun_mass)**0.4_dp  ![km]


!  Moon and Gravitation
    Real(dp), Parameter :: R_moon = 1737.4_dp  ![km] Mean volumetric radius of the moon
    Real(dp), Parameter :: grav_parameter_moon = 4904.8695_dp  ![km^3 / s^2]  gravitational parameter of the moon
    Real(dp), Parameter :: Escape_speed_moon = &  !minimum speed to escape the moon's gravitational influence from the surface
                               & Sqrt(2._dp * grav_parameter_moon / R_moon)  ![km/s]
    Real(dp), Parameter :: g0_moon = grav_parameter_moon / R_moon  ![km/s^2] lunar acceleration due to gravity
    Real(dp), Parameter :: Moon_mass = 0.07346E24_dp  ![kg]
    Real(dp), Parameter :: Moon_orbit_a = 0.3844E6_dp ![km]
    Real(dp), Parameter :: Moon_SOI = Moon_orbit_a * (Moon_mass / Earth_mass)**0.4_dp  ![km]

!   Conditionally compiled constants to set either the Earth or the moon as the gravitaional body for gravity calculations
#   if LUNA
        Real(dp), Parameter :: R_center = R_moon
        Real(dp), Parameter :: grav_param = grav_parameter_moon
        Real(dp), Parameter :: Esc_speed = Escape_speed_moon
        Real(dp), Parameter :: g0 = g0_moon
        Real(dp), Parameter :: SOI_center = Moon_SOI
#   elif SOL
        Real(dp), Parameter :: R_center = R_sun
        Real(dp), Parameter :: grav_param = grav_parameter_sun
        Real(dp), Parameter :: Esc_speed = Escape_speed_sun
        Real(dp), Parameter :: g0 = g0_sun
        Real(dp), Parameter :: SOI_center = Sun_SOI
#   else
        Real(dp), Parameter :: R_center = R_earth
        Real(dp), Parameter :: grav_param = std_grav_parameter
        Real(dp), Parameter :: Esc_speed = Escape_speed
        Real(dp), Parameter :: g0 = g0_earth
        Real(dp), Parameter :: SOI_center = Earth_SOI
#   endif

! NEUTRON constants
Real(dp), Parameter :: neutron_mass = 1.674927471E-27_dp  ![kg]
Real(dp), Parameter :: neutron_mass_E = neutron_mass * speed_of_light**2 * 1000._dp**2 ![J]  Mass-energy of a neutron
Real(dp), Parameter :: neutron_speed_conversion = &  !When multiplied by Sqrt(Energy in keV), gives speed of neutron in km/s
                           & Sqrt( (2._dp / keV_per_Joule) / (neutron_mass) ) / 1000._dp  ![km/s per Sqrt(keV)]
Real(dp), Parameter :: neutron_PE_conversion = &  !When multiplied by change in height, gives change in PE in keV
                           & g0 * neutron_mass * keV_per_Joule  ![kg keV m/s^2 per J]
Real(dp), Parameter :: mfp_per_barn_per_km_at_seaLevel = 5.07_dp
! With 1 barn microscopic cross section, a path length of 1 km in sea level air
!   is an optical path length of 5.07 (mean free paths)
!   So this is "mean free paths per barn per km at sea level"
! Note: EPL (effective path length) is the path length at sea level
!   that has the same optical thickness as the actual path
Real(dp), Parameter :: n_lambda = Log(2._dp) / 611._dp  ![1/s] ln(2) divided by neutron half-life
Real(dp), Parameter :: n_kill_weight = &  !weight of 1 particular neutron having chance to exist from a 1MT source
                           & 1._dp / (0.35_dp * nA * 1000._dp)  !based on rule-of-thumb of 0.35 mol of neutrons per kT yield

End Module Global

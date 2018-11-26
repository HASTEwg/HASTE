Module US_Std_Atm_1976
    
    Use Kinds, Only: dp
    Implicit None    
    Private
    Public :: find_base_layer
    Public :: T
    Public :: P
    Public :: rho    
    Public :: rho_N    
    Public :: Zb
#   if INTEGRAND_STOPS
        Public :: nN2_power_stops
        Public :: nO1_O2_power_stops
        Public :: nAr_He_power_stops
#   endif
#   if GL_POINTS
        Public :: nN2_GLpoints
        Public :: nO1_O2_GLpoints
        Public :: nAr_He_GLpoints
#   endif

    !US Standard Atmosphere 1976 parameters
    !The following constants are defined here to ensure consistency with 1976 atmosphere model definition.
    !There may be more modern values for these constants, but these values ensure agreement with the 1976 US Std Atmosphere model as published
    Real(dp), Parameter :: g0 = 9.80665_dp  ![m / s^2]  accelleration due to gravity
    Real(dp), Parameter :: R_Earth = 6356.766_dp  ![km]  Radius of earth (nominal) at 45 deg latitude, used to relate geometric and geopotential heights, from US Std Atmosphere 1976
    Real(dp), Parameter :: R_star = 8.31432_dp  ![J / (mol*K)]  Universal gas constant as defined in US Standard Atmosphere 1976
    Real(dp), Parameter :: M0 = 28.964425912034_dp  ![kg / kmol] Average molecular weight of the ten most abundant species in air, weighted by relative abundance, from US Std Atmosphere 1976
    Real(dp), Parameter :: Hb(0:7) = (/ 0._dp,  &  ![km] geopotential heights of layer boundaries, US Standard Atmosphere 1976 table 4 
                                      & 11._dp, & 
                                      & 20._dp, & 
                                      & 32._dp, & 
                                      & 47._dp, & 
                                      & 51._dp, & 
                                      & 71._dp, &
                                      & 86._dp * R_Earth / (86._dp + R_Earth) /)
    Real(dp), Parameter :: Zb(0:11) = (/ 0._dp, &  ![km] geometric heights of layer boundaries, US Standard Atmosphere 1976 
                                       & Hb(1) * R_Earth / (R_Earth - Hb(1)), & 
                                       & Hb(2) * R_Earth / (R_Earth - Hb(2)), & 
                                       & Hb(3) * R_Earth / (R_Earth - Hb(3)), & 
                                       & Hb(4) * R_Earth / (R_Earth - Hb(4)), & 
                                       & Hb(5) * R_Earth / (R_Earth - Hb(5)), & 
                                       & Hb(6) * R_Earth / (R_Earth - Hb(6)), &
                                       & 86._dp,  &
                                       & 91._dp,  &
                                       & 110._dp, &
                                       & 120._dp, &
                                       & 1000._dp /)
    Real(dp), Parameter :: Lb(0:11) = (/ -6.5_dp, &  ![K/km] temperature lapse rates in each layer, US Standard Atmosphere 1976 table 4 
                                       &  0._dp,  & 
                                       &  1._dp,  & 
                                       &  2.8_dp, & 
                                       &  0._dp,  & 
                                       & -2.8_dp, & 
                                       & -2._dp,  &
                                       &  0._dp,  &
                                       &  0._dp,  &
                                       &  12._dp, &
                                       &  0._dp,  &
                                       &  0._dp   /)
    Real(dp), Parameter :: Tb(0:11) = (/ 288.15_dp, &  ![K] Computed temperature at layer boundaries 
                                       & 216.65_dp, &  !Molecular & Kinetic Temperature
                                       & 216.65_dp, &  !Molecular & Kinetic Temperature
                                       & 228.65_dp, &  !Molecular & Kinetic Temperature
                                       & 270.65_dp, &  !Molecular & Kinetic Temperature
                                       & 270.65_dp, &  !Molecular & Kinetic Temperature 
                                       & 214.65_dp, &  !Molecular & Kinetic Temperature
                                       & 186.8671666936082608_dp, &  !Kinetic Temperature
                                       & 186.8671666936082608_dp, &  !Kinetic Temperature
                                       & 240._dp, &  !Kinetic Temperature
                                       & 360._dp, &  !Kinetic Temperature
                                       & 1000._dp /)  !Kinetic Temperature
    Real(dp), Parameter :: Pb(0:7) = (/ 101325._dp, &   ![Pa] Computed pressure at layer boundaries
                                      & 22632.0336238972840275_dp, & 
                                      & 5474.87437675730708586_dp, & 
                                      & 868.014988510785148131_dp, & 
                                      & 110.905629143702212828_dp, & 
                                      & 66.9384346263881217465_dp, & 
                                      & 3.95638449983647254755_dp, &
                                      & 0.37337628269333201966_dp  /)
    Logical, Parameter :: Lb_nonzero(0:11) = (/ .TRUE.,  & 
                                              & .FALSE., & 
                                              & .TRUE.,  & 
                                              & .TRUE.,  & 
                                              & .FALSE., & 
                                              & .TRUE.,  & 
                                              & .TRUE.,  &
                                              & .FALSE., &
                                              & .FALSE., &
                                              & .TRUE.,  &
                                              & .FALSE., &
                                              & .FALSE.  /)  !flags indicating non-zero lapse rate
    Logical, Parameter :: T_linear_by_H(0:11) = (/ .TRUE.,  & 
                                                 & .FALSE., & 
                                                 & .TRUE.,  & 
                                                 & .TRUE.,  & 
                                                 & .FALSE., & 
                                                 & .TRUE.,  & 
                                                 & .TRUE.,  &
                                                 & .FALSE., &
                                                 & .FALSE., &
                                                 & .FALSE., &
                                                 & .FALSE., &
                                                 & .FALSE.  /)  !flags indicating linear temperature by geopotential height
    Logical, Parameter :: T_elliptical(0:11) = (/ .FALSE., & 
                                                & .FALSE., & 
                                                & .FALSE., & 
                                                & .FALSE., & 
                                                & .FALSE., & 
                                                & .FALSE., & 
                                                & .FALSE., &
                                                & .FALSE., &
                                                & .TRUE.,  &
                                                & .FALSE., &
                                                & .FALSE., &
                                                & .FALSE.  /)  !flags indicating elliptical temperature by geometric height
    Logical, Parameter :: T_exponential(0:11) = (/ .FALSE., & 
                                                 & .FALSE., & 
                                                 & .FALSE., & 
                                                 & .FALSE., & 
                                                 & .FALSE., & 
                                                 & .FALSE., & 
                                                 & .FALSE., &
                                                 & .FALSE., &
                                                 & .FALSE., &
                                                 & .FALSE., &
                                                 & .TRUE.,  &
                                                 & .FALSE.  /)  !flags indicating exponential temperature by geometric height
    Logical, Parameter :: P_rho_not_by_N(0:11) = (/ .TRUE.,  & 
                                                  & .TRUE.,  & 
                                                  & .TRUE.,  & 
                                                  & .TRUE.,  & 
                                                  & .TRUE.,  & 
                                                  & .TRUE.,  & 
                                                  & .TRUE.,  &
                                                  & .FALSE., &
                                                  & .FALSE., &
                                                  & .FALSE., &
                                                  & .FALSE., &
                                                  & .FALSE.  /)  !flags indicating Pressure and Density computed by OTHER than number density
    Real(dp), Parameter :: Tb_minus_LbHb(0:7) = Tb(0:7) - Lb(0:7)*Hb(0:7)  !precomputed quantity for 1976 temperature calculations
    Real(dp), Parameter :: L_star = g0 * M0 / R_star  !precomputed quantity for 1976 pressure calculations
    Real(dp), Parameter :: L_star_Lb(0:7) = (/ L_star / Lb(0), &  !precomputed quantity for 1976 pressure calculations
                                             & 0._dp,          & 
                                             & L_star / Lb(2), & 
                                             & L_star / Lb(3), & 
                                             & 0._dp,          & 
                                             & L_star / Lb(5), & 
                                             & L_star / Lb(6), &
                                             & 0._dp           /)
    Real(dp), Parameter :: Pb_Tb_L_star_Lb(0:7) = (/ Pb(0) * Tb(0)**L_star_Lb(0), &  !precomputed quantity for 1976 pressure calculations
                                                   & Pb(1),                       & 
                                                   & Pb(2) * Tb(2)**L_star_Lb(2), & 
                                                   & Pb(3) * Tb(3)**L_star_Lb(3), & 
                                                   & Pb(4),                       & 
                                                   & Pb(5) * Tb(5)**L_star_Lb(5), & 
                                                   & Pb(6) * Tb(6)**L_star_Lb(6), &
                                                   & Pb(7)                        /)
    Real(dp), Parameter :: L_star_Tb(0:7) = -L_star / Tb(0:7)  !precomputed quantity for 1976 pressure calculations
    Real(dp), Parameter :: rho_star = M0 / R_star  !precomputed quantity for 1976 density calculations
    Real(dp), Parameter :: Tc = (Lb(9) * (Zb(9)-Zb(8)) * Tb(9) + Tb(8)**2 - Tb(9)**2) / &
                              & (Lb(9) * (Zb(9)-Zb(8)) + 2._dp * Tb(8) - 2._dp * Tb(9))  !US Standard Atmosphere 1976 equation B-8
    Real(dp), Parameter :: big_A = Tb(8) - Tc  !US Standard Atmosphere 1976 equation B-5
    Real(dp), Parameter :: little_A = (Zb(9)-Zb(8)) * big_A / Sqrt(big_A**2 - (Tb(9)-Tc)**2)  !US Standard Atmosphere 1976 equation B-9
    Real(dp), Parameter :: T_inf = 1000._dp
    Real(dp), Parameter :: lambda = Lb(9) / (T_inf - Tb(10))  !precomputed quantity for 1976 temperature calculations
    Real(dp), Parameter :: R_Z9 = R_Earth + Zb(7)
    Real(dp), Parameter :: R_Z9 = R_Earth + Zb(9)
    Real(dp), Parameter :: R_Z10 = R_Earth + Zb(10)
    Real(dp), Parameter :: Na = 6.022169E26_dp  ![1/kmol] Avagadro's Number
    Real(dp), Parameter :: inv_Na = 1._dp / Na
    Real(dp), Parameter :: N_star = R_star / Na
    Real(dp), Parameter :: K0 = 1.2E2_dp
    Real(dp), Parameter :: Mi(1:6) = (/ 28.0134_dp, &  !N2
                                      & 15.9994_dp, &  !O1
                                      & 31.9988_dp, &  !O2
                                      & 39.948_dp,  &  !Ar
                                      &  4.0026_dp, &  !He
                                      &  0.5_dp * 2.01594_dp  /) !H1  !US Standard Atmosphere 1976 table 3
    Real(dp), Parameter :: alphaHe = -0.40_dp  !He  !US Standard Atmosphere 1976 table 6
    Real(dp), Parameter :: alphaH1 = -0.25_dp  !H1  !US Standard Atmosphere 1976 table 6
    Real(dp), Parameter :: ai(2:6) = (/ 6.986E20_dp, &  !O1
                                      & 4.863E20_dp, &  !O2
                                      & 4.487E20_dp, &  !Ar
                                      & 1.700E21_dp, &  !He
                                      & 3.305E21_dp  /) !H1  !US Standard Atmosphere 1976 table 6
    Real(dp), Parameter :: bi(2:5) = (/ 0.750_dp, &  !O1
                                      & 0.750_dp, &  !O2
                                      & 0.870_dp, &  !Ar
                                      & 0.691_dp  /) !He  !US Standard Atmosphere 1976 table 6
    Real(dp), Parameter :: bigQi(2:5) = (/ -5.809644E-4_dp, &  !O1
                                         &  1.366212E-4_dp, &  !O2
                                         &  9.434079E-5_dp, &  !Ar
                                         & -2.457369E-4_dp  /) !He  !US Standard Atmosphere 1976 table 7
    Real(dp), Parameter :: bigUi(2:5) = (/ 56.90311_dp, &  !O1
                                         & 86._dp,      &  !O2
                                         & 86._dp,      &  !Ar
                                         & 86._dp       /) !He  !US Standard Atmosphere 1976 table 7
    Real(dp), Parameter :: bigWi(2:5) = (/ 2.706240E-5_dp, &  !O1
                                         & 8.333333E-5_dp, &  !O2
                                         & 8.333333E-5_dp, &  !Ar
                                         & 6.666667E-4_dp  /) !He  !US Standard Atmosphere 1976 table 7
    Real(dp), Parameter :: littleQi = -3.416248E-3_dp !only defined for O1  !US Standard Atmosphere 1976 table 7
    Real(dp), Parameter :: littleUi = 97._dp          !only defined for O1  !US Standard Atmosphere 1976 table 7
    Real(dp), Parameter :: littleWi = 5.008765E-4_dp  !only defined for O1  !US Standard Atmosphere 1976 table 7
    Real(dp), Parameter :: N7(1:5) = (/ 1.129794E20_dp, &  !N2
                                      & 8.6E16_dp,      &  !O1
                                      & 3.030898E19_dp, &  !O2
                                      & 1.351400E18_dp, &  !Ar
                                      & 7.5817E14_dp    /) !He  !US Standard Atmosphere 1976 table 9    
    Real(dp), Parameter :: nH500 = 8.E10_dp
    Real(dp), Parameter :: T500 = 999.2356017626150686_dp
    Real(dp), Parameter :: phiH = 7.2E11_dp
    !Convergence criteria for Romberg Quadrature routines
#   if (INTEGRAND_STOPS || GL_POINTS)
        Real(dp), Parameter :: rTol_tier1 = 1.E-15_dp  !N2
        Real(dp), Parameter :: rTol_tier2 = 1.E-14_dp  !O1 and O2
        Real(dp), Parameter :: rTol_tier3 = 1.E-13_dp  !Ar and He
#   else
        Real(dp), Parameter :: rTol_tier1 = 1.E-8_dp  !N2
        Real(dp), Parameter :: rTol_tier2 = 1.E-7_dp  !O1 and O2
        Real(dp), Parameter :: rTol_tier3 = 1.E-6_dp  !Ar and He
#   endif
    Real(dp), Parameter :: rTol_tier4a = 1.E-5_dp  !H
    Real(dp), Parameter :: rTol_tier4b = 1.E-4_dp  !H
    
    Interface rho_N
        Module Procedure N_densities
        Module Procedure N_density
    End Interface rho_N
    
Contains

Function Find_Base_Layer(Z,iZb) Result(b)
    Use Kinds, Only: dp
    Use Utilities, Only: Bisection_Search
    Implicit None
    Integer :: b
    Real(dp), Intent(In) :: Z
    Integer, Intent(In), Optional :: iZb(1:3)
    
    If (Present(iZb)) Then
        b = (iZb(1)-1) + Bisection_Search(Z,Zb(iZb(1):iZb(2)),iZb(3)) - 1  !subtract 1 to get index for layer below Z
    Else
        b = Bisection_Search(Z,Zb(1:10),10) - 1  !subtract 1 to get index for layer below Z
    End If
End Function Find_Base_Layer

Function T(Z,layer,layer_range)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: T  ![K] kinetic temperature at geometric altitude Z
    Real(dp), Intent(In) :: Z ![km]
    Integer, Intent(In), Optional :: layer  !layer in which Z falls
    Integer, Intent(In), Optional :: layer_range(1:3)  !layer range over which to search
    Integer :: b  !index of base for layer
    
    !find atmospheric base layer
    If (Present(layer)) Then
        b = layer - 1
    Else If (Present(layer_range)) Then
        b = Find_Base_Layer(Z,layer_range)
    Else
        b = Find_Base_Layer(Z)
    End If
    If (Lb_nonzero(b)) Then
        If (T_linear_by_H(b)) Then
            T = Tb_minus_LbHb(b) + Lb(b) * Z_to_H(Z)  !US Standard Atmosphere 1976 equation 23
            If (b.EQ.6 .AND. Z.GT.80._dp) T = T * T_M0_correction(Z)  !US Standard Atmosphere 1976 equation 22
        Else
            T = Tb(b) + Lb(b) * (Z - Zb(b))  !US Standard Atmosphere 1976 equation 29
        End If
    Else If (T_exponential(b)) Then
        T = T_inf - (T_inf - Tb(b)) * Exp(-lambda * (Z - Zb(b)) * R_Z10 / (R_Earth + Z))  !US Standard Atmosphere 1976 equation 31
    Else If (T_elliptical(b)) Then
        T = Tc + big_A * Sqrt(1._dp - ((Z - Zb(b)) / little_A)**2)  !US Standard Atmosphere 1976 equation 27
    Else !zero lapse rate
        T = Tb(b)
    End If
End Function T

Function dT_dZ(Z,layer,layer_range)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: dT_dZ  ![K/km]
    Real(dp), Intent(In) :: Z ![km]
    Integer, Intent(In), Optional :: layer
    Integer, Intent(In), Optional :: layer_range(1:3)
    Integer :: b
    
    !find atmospheric base layer
    If (Present(layer)) Then
        b = layer - 1
    Else If (Present(layer_range)) Then
        b = Find_Base_Layer(Z,layer_range)
    Else
        b = Find_Base_Layer(Z)
    End If
    If (Lb_nonzero(b)) Then
        dT_dZ = Lb(b)
        If (b.EQ.6 .AND. Z.GT.80._dp) dT_dZ = dT_dZ * T_M0_correction(Z)  !US Standard Atmosphere 1976 equation 22
    Else If (T_exponential(b)) Then  !b=10
        dT_dZ = lambda * (T_inf - Tb(10)) * ((R_Earth + Zb(10)) / (R_Earth + Z))**2 * Exp(-lambda * (Z - Zb(10)) * R_Z10 / (R_Earth + Z))  !US Standard Atmosphere 1976 equation 32
    Else If (T_elliptical(b)) Then  !b=8
        dT_dZ = -big_A * (Z - Zb(8)) / ((little_A**2) * Sqrt(1._dp - ((Z - Zb(8)) / little_A)**2))  !US Standard Atmosphere 1976 equation 28
    Else
        dT_dZ = 0._dp
    End If
End Function dT_dZ

Function T_M0_correction(Z) Result(c)
    Use Kinds, Only: dp
    Use Interpolation, Only: Linear_Interp
    Implicit None
    Real(dp) :: c
    Real(dp), Intent(In) :: Z
    Integer :: i
    Real(dp), Parameter :: Zm_corr(0:12) = (/ 80._dp,  &
                                            & 80.5_dp, &
                                            & 81._dp,  &
                                            & 81.5_dp, &
                                            & 82._dp,  &
                                            & 82.5_dp, &
                                            & 83._dp,  &
                                            & 83.5_dp, &
                                            & 84._dp,  &
                                            & 84.5_dp, &
                                            & 85._dp,  &
                                            & 85.5_dp, &
                                            & 86._dp   /)
    Real(dp), Parameter :: M0_corr(0:12) = (/ 1._dp,       &
                                            & 0.999996_dp, &
                                            & 0.999989_dp, &
                                            & 0.999971_dp, &
                                            & 0.999941_dp, &
                                            & 0.999909_dp, &
                                            & 0.999870_dp, &
                                            & 0.999829_dp, &
                                            & 0.999786_dp, &
                                            & 0.999741_dp, &
                                            & 0.999694_dp, &
                                            & 0.999641_dp, &
                                            & 0.999579_dp /)
    
    i = Ceiling(2._dp * (Z - 80._dp))
    c = Linear_Interp(Z,Zm_corr(i-1),Zm_corr(i),M0_corr(i-1),M0_corr(i))
End Function T_M0_correction

Function g(Z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: g
    Real(dp), Intent(In) :: Z
    
    g = g0 * (R_Earth / (R_Earth + Z))**2  !US Standard Atmosphere 1976 equation 17
End Function g

Function nN2_power(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp), Parameter :: xb(8:10) = (/ 0.8891738712368935_dp, &  !Z = 91km
                                       & 3.9815997728018484_dp, &  !Z = 110km
                                       & 5.0588195691573041_dp  /) !Z = 120km
    Real(dp), Parameter :: xb_100 = 2.4639390409132487_dp  !Z = 100km
    ! Real(dp), Parameter :: xb_185 = 8.4805605838919416_dp  !Z = 185km
    ! Real(dp), Parameter :: xb_250 = 10.7467415074578128_dp  !Z = 250km
    ! Real(dp), Parameter :: xb_500 = 18.2163216195700670_dp  !Z = 500km
    Logical, Parameter :: no_sublayers(7:10) = (/ .TRUE.,  &
                                                & .FALSE., &
                                                & .TRUE.,  &
                                                & .TRUE.   /)
    Real(dp), Parameter :: rho_star_N2 = Mi(1) / R_star
    !Precomputed parameters for b = 7
    Real(dp), Parameter :: c7 = rho_star * g0 * R_Earth * (R_earth/R_Z7) / Tb(7)
    !precomputed parameters for b=9
    Real(dp), Parameter :: c9a = rho_star_N2 * Lb(9)
    Real(dp), Parameter :: c9b = -Log(Tb(9)/R_Z9)
    !precomputed parameters for b=10
    Real(dp), Parameter :: c10a = rho_star_N2 * g0 * (R_Earth/R_Z10)**2 / (T_inf * lambda)
    Real(dp), Parameter :: c10b = -lambda * R_z10**2
    Real(dp), Parameter :: c10c = lambda * R_z10 - Log(Tb(10))

    If (no_sublayers(b)) Then !b=7, 9, or 10
        If (b .EQ. 7) Then !b=7
            !direct evaluation
            x = c7 * (Z - Zb(7)) / (R_Earth + Z)
            ! x = rho_star * Romberg_Quad_nN2(Zb(7),Z,7)
            ! x = rho_star * GL_Quad_nN2_7(Z)
        Else If (b .EQ. 9) Then !b=9
            !direct evaluation
            x = xb(9) + c9a * ( ((Log(T(Z,10)/(R_Earth+Z)) * R_Z9 - (Z-Zb(9))) / (R_Earth +Z)) + c9b )
            ! x = xb(9) + rho_star_N2 * Romberg_Quad_nN2(Zb(9),Z,9)
            ! x = xb(9) + rho_star_N2 * GL_Quad_nN2_9(Z)
        Else !b=10
            !direct evaluation
            x = xb(10) + c10a * (Log(T(Z,11)) + c10b / (R_Earth + Z) + c10c)
            ! x = xb(10) + rho_star_N2 * Romberg_Quad_nN2(Zb(10),Z,10)
            ! !Layer 11 (b=10) is further subdivided to keep number of quad points manageable
            ! If (Z .LT. 185._dp) Then
            !     x = xb(10) + rho_star_N2 * GL_Quad_nN2_10a(Z)
            ! Else If (Z .LT. 250._dp) Then
            !     x = xb_185 + rho_star_N2 * GL_Quad_nN2_10b(Z)
            ! Else If (Z .LT. 500._dp) Then
            !     x = xb_250 + rho_star_N2 * GL_Quad_nN2_10c(Z)
            ! Else !Z = 500 to 1000 km
            !     x = xb_500 + rho_star_N2 * GL_Quad_nN2_10d(Z)
            ! End If
        End If
    Else !b=8
        If (Z .LT. 100._dp) Then
            ! x = xb(8) + rho_star * Romberg_Quad_nN2(Zb(8),Z,8)
            x = xb(8) + rho_star * GL_Quad_nN2_8a(Z)
        Else
            ! x = xb_100 + rho_star_N2 * Romberg_Quad_nN2(100._dp,Z,8)
            x = xb_100 + rho_star_N2 * GL_Quad_nN2_8b(Z)
        End If
    End If
End Function nN2_power

Function nN2_integrand(z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x
    Real(dp), Intent(In) :: z
    Integer, Intent(In) :: b

    x = g(z) / T(z,b+1)
End Function nN2_integrand

! Function GL_Quad_nN2_7(z) Result(q)  !for 86 to 91 km
!     Use Kinds, Only: dp
!     Implicit None
!     Real(dp):: q    !the result of the integration
!     Real(dp), Intent(In) :: z    !limit of integration
!     Integer, Parameter :: n = 3
!     Real(dp), Parameter :: wi(1:n) = (/  0.5555555555555555555555555555555555555555555555555555555555555556_dp, &
!                                       &  0.8888888888888888888888888888888888888888888888888888888888888889_dp, &
!                                       &  0.5555555555555555555555555555555555555555555555555555555555555556_dp /)
!     Real(dp), Parameter :: xi(1:n) = (/ -0.7745966692414833770358530799564799221665843410583181653175147532_dp, &
!                                       &  0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
!                                       &  0.7745966692414833770358530799564799221665843410583181653175147532_dp /)
!     Real(dp) :: fi(1:n)  !function values
!     Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
!     Integer :: i

!     c1 = 0.5_dp * (z-Zb(7))
!     c2 = 0.5_dp * (z+Zb(7))
!     Do i = 1,n
!         fi(i) = nN2_integrand(c1 * xi(i) + c2,7)
!     End Do
!     q = c1 * Dot_Product(wi,fi)
! End Function GL_Quad_nN2_7

Function GL_Quad_nN2_8a(z) Result(q)  !for 91 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 8
    Real(dp), Parameter :: wi(1:n) = (/  0.1012285362903762591525313543099621901153940910516849570590036981_dp, &
                                      &  0.2223810344533744705443559944262408844301308700512495647259092893_dp, &
                                      &  0.3137066458778872873379622019866013132603289990027349376902639451_dp, &
                                      &  0.3626837833783619829651504492771956121941460398943305405248230676_dp, &
                                      &  0.3626837833783619829651504492771956121941460398943305405248230676_dp, &
                                      &  0.3137066458778872873379622019866013132603289990027349376902639451_dp, &
                                      &  0.2223810344533744705443559944262408844301308700512495647259092893_dp, &
                                      &  0.1012285362903762591525313543099621901153940910516849570590036981_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9602898564975362316835608685694729904282352343014520382716397774_dp, &
                                      & -0.7966664774136267395915539364758304368371717316159648320701702950_dp, &
                                      & -0.5255324099163289858177390491892463490419642431203928577508570993_dp, &
                                      & -0.1834346424956498049394761423601839806667578129129737823171884737_dp, &
                                      &  0.1834346424956498049394761423601839806667578129129737823171884737_dp, &
                                      &  0.5255324099163289858177390491892463490419642431203928577508570993_dp, &
                                      &  0.7966664774136267395915539364758304368371717316159648320701702950_dp, &
                                      &  0.9602898564975362316835608685694729904282352343014520382716397774_dp /)
    Real(dp) :: fi(1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(8))
    c2 = 0.5_dp * (z+Zb(8))
    Do i = 1,n
        fi(i) = nN2_integrand(c1 * xi(i) + c2,8)
    End Do
    q = c1 * Dot_Product(wi,fi)
End Function GL_Quad_nN2_8a

Function GL_Quad_nN2_8b(z) Result(q)  !for 100 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 22
    Real(dp), Parameter :: wi(1:n) = (/  0.0146279952982722006849910980471854451902119491243237309244975914_dp, &
                                      &  0.0337749015848141547933022468659129013491693144744746604328831946_dp, &
                                      &  0.0522933351526832859403120512732112561121500935692363161227100833_dp, &
                                      &  0.0697964684245204880949614189302176573987750529550805273863113718_dp, &
                                      &  0.0859416062170677274144436813727028661891444053490797832865700110_dp, &
                                      &  0.1004141444428809649320788378305362823508110887676996257849609143_dp, &
                                      &  0.1129322960805392183934006074217843191142633321209947042725975462_dp, &
                                      &  0.1232523768105124242855609861548144719594449990049679724284219719_dp, &
                                      &  0.1311735047870623707329649925303074458757418941880306531253797034_dp, &
                                      &  0.1365414983460151713525738312315173965863676529886861681094697349_dp, &
                                      &  0.1392518728556319933754102483418099578739202174574258581261978771_dp, &
                                      &  0.1392518728556319933754102483418099578739202174574258581261978771_dp, &
                                      &  0.1365414983460151713525738312315173965863676529886861681094697349_dp, &
                                      &  0.1311735047870623707329649925303074458757418941880306531253797034_dp, &
                                      &  0.1232523768105124242855609861548144719594449990049679724284219719_dp, &
                                      &  0.1129322960805392183934006074217843191142633321209947042725975462_dp, &
                                      &  0.1004141444428809649320788378305362823508110887676996257849609143_dp, &
                                      &  0.0859416062170677274144436813727028661891444053490797832865700110_dp, &
                                      &  0.0697964684245204880949614189302176573987750529550805273863113718_dp, &
                                      &  0.0522933351526832859403120512732112561121500935692363161227100833_dp, &
                                      &  0.0337749015848141547933022468659129013491693144744746604328831946_dp, &
                                      &  0.0146279952982722006849910980471854451902119491243237309244975914_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9942945854823992920730314211612989803930541983960315349770229250_dp, &
                                      & -0.9700604978354287271239509867652687108059689231378029695597382123_dp, &
                                      & -0.9269567721871740005206929392590531966353296533720519820445731915_dp, &
                                      & -0.8658125777203001365364256370193787290847967555884461561270933693_dp, &
                                      & -0.7878168059792081620042779554083515213881786713315631581216933977_dp, &
                                      & -0.6944872631866827800506898357622567712673422899774351611217337754_dp, &
                                      & -0.5876404035069115929588769276386473488776156035737178397700338214_dp, &
                                      & -0.4693558379867570264063307109664063460953437911307365477961000297_dp, &
                                      & -0.3419358208920842251581474204273796195591732991706873439968623837_dp, &
                                      & -0.2078604266882212854788465339195457342156751090979336496686533044_dp, &
                                      & -0.0697392733197222212138417961186280818222962994802314747066819465_dp, &
                                      &  0.0697392733197222212138417961186280818222962994802314747066819465_dp, &
                                      &  0.2078604266882212854788465339195457342156751090979336496686533044_dp, &
                                      &  0.3419358208920842251581474204273796195591732991706873439968623837_dp, &
                                      &  0.4693558379867570264063307109664063460953437911307365477961000297_dp, &
                                      &  0.5876404035069115929588769276386473488776156035737178397700338214_dp, &
                                      &  0.6944872631866827800506898357622567712673422899774351611217337754_dp, &
                                      &  0.7878168059792081620042779554083515213881786713315631581216933977_dp, &
                                      &  0.8658125777203001365364256370193787290847967555884461561270933693_dp, &
                                      &  0.9269567721871740005206929392590531966353296533720519820445731915_dp, &
                                      &  0.9700604978354287271239509867652687108059689231378029695597382123_dp, &
                                      &  0.9942945854823992920730314211612989803930541983960315349770229250_dp /)
    Real(dp) :: fi(1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-100._dp)
    c2 = 0.5_dp * (z+100._dp)
    Do i = 1,n
        fi(i) = nN2_integrand(c1 * xi(i) + c2,8)
    End Do
    q = c1 * Dot_Product(wi,fi)
End Function GL_Quad_nN2_8b

! Function GL_Quad_nN2_9(z) Result(q)  !for 110 to 120 km
!     Use Kinds, Only: dp
!     Implicit None
!     Real(dp):: q    !the result of the integration
!     Real(dp), Intent(In) :: z    !limit of integration
!     Integer, Parameter :: n = 8
!     Real(dp), Parameter :: wi(1:n) = (/  0.1012285362903762591525313543099621901153940910516849570590036981_dp, &
!                                       &  0.2223810344533744705443559944262408844301308700512495647259092893_dp, &
!                                       &  0.3137066458778872873379622019866013132603289990027349376902639451_dp, &
!                                       &  0.3626837833783619829651504492771956121941460398943305405248230676_dp, &
!                                       &  0.3626837833783619829651504492771956121941460398943305405248230676_dp, &
!                                       &  0.3137066458778872873379622019866013132603289990027349376902639451_dp, &
!                                       &  0.2223810344533744705443559944262408844301308700512495647259092893_dp, &
!                                       &  0.1012285362903762591525313543099621901153940910516849570590036981_dp /)
!     Real(dp), Parameter :: xi(1:n) = (/ -0.9602898564975362316835608685694729904282352343014520382716397774_dp, &
!                                       & -0.7966664774136267395915539364758304368371717316159648320701702950_dp, &
!                                       & -0.5255324099163289858177390491892463490419642431203928577508570993_dp, &
!                                       & -0.1834346424956498049394761423601839806667578129129737823171884737_dp, &
!                                       &  0.1834346424956498049394761423601839806667578129129737823171884737_dp, &
!                                       &  0.5255324099163289858177390491892463490419642431203928577508570993_dp, &
!                                       &  0.7966664774136267395915539364758304368371717316159648320701702950_dp, &
!                                       &  0.9602898564975362316835608685694729904282352343014520382716397774_dp /)
!     Real(dp) :: fi(1:n)  !function values
!     Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
!     Integer :: i

!     c1 = 0.5_dp * (z-Zb(9))
!     c2 = 0.5_dp * (z+Zb(9))
!     Do i = 1,n
!         fi(i) = nN2_integrand(c1 * xi(i) + c2,9)
!     End Do
!     q = c1 * Dot_Product(wi,fi)
! End Function GL_Quad_nN2_9

! Function GL_Quad_nN2_10a(z) Result(q)  !for 120 to 185 km
!     Use Kinds, Only: dp
!     Implicit None
!     Real(dp):: q    !the result of the integration
!     Real(dp), Intent(In) :: z    !limit of integration
!     Integer, Parameter :: n = 16
!     Real(dp), Parameter :: wi(1:n) = (/  0.0271524594117540948517805724560181035122673755667607979906103191_dp, &
!                                       &  0.0622535239386478928628438369943776942749865083529068579013035158_dp, &
!                                       &  0.0951585116824927848099251076022462263552635031837126581568222872_dp, &
!                                       &  0.1246289712555338720524762821920164201448868592222026799447505904_dp, &
!                                       &  0.1495959888165767320815017305474785489704910682078364668054219622_dp, &
!                                       &  0.1691565193950025381893120790303599622116394734160282817450829357_dp, &
!                                       &  0.1826034150449235888667636679692199393835562236546492824184951444_dp, &
!                                       &  0.1894506104550684962853967232082831051469089883959029750375132452_dp, &
!                                       &  0.1894506104550684962853967232082831051469089883959029750375132452_dp, &
!                                       &  0.1826034150449235888667636679692199393835562236546492824184951444_dp, &
!                                       &  0.1691565193950025381893120790303599622116394734160282817450829357_dp, &
!                                       &  0.1495959888165767320815017305474785489704910682078364668054219622_dp, &
!                                       &  0.1246289712555338720524762821920164201448868592222026799447505904_dp, &
!                                       &  0.0951585116824927848099251076022462263552635031837126581568222872_dp, &
!                                       &  0.0622535239386478928628438369943776942749865083529068579013035158_dp, &
!                                       &  0.0271524594117540948517805724560181035122673755667607979906103191_dp /)
!     Real(dp), Parameter :: xi(1:n) = (/ -0.9894009349916499325961541734503326274262740716576451300512239047_dp, &
!                                       & -0.9445750230732325760779884155346083450911392725910726009255536521_dp, &
!                                       & -0.8656312023878317438804678977123931323873353848475267081035114256_dp, &
!                                       & -0.7554044083550030338951011948474422683538136564575030097817571769_dp, &
!                                       & -0.6178762444026437484466717640487910189918822177656577941037973555_dp, &
!                                       & -0.4580167776572273863424194429835775735400316130355234909011547509_dp, &
!                                       & -0.2816035507792589132304605014604961064860694907705998005488347340_dp, &
!                                       & -0.0950125098376374401853193354249580631303530556890654566972198172_dp, &
!                                       &  0.0950125098376374401853193354249580631303530556890654566972198172_dp, &
!                                       &  0.2816035507792589132304605014604961064860694907705998005488347340_dp, &
!                                       &  0.4580167776572273863424194429835775735400316130355234909011547509_dp, &
!                                       &  0.6178762444026437484466717640487910189918822177656577941037973555_dp, &
!                                       &  0.7554044083550030338951011948474422683538136564575030097817571769_dp, &
!                                       &  0.8656312023878317438804678977123931323873353848475267081035114256_dp, &
!                                       &  0.9445750230732325760779884155346083450911392725910726009255536521_dp, &
!                                       &  0.9894009349916499325961541734503326274262740716576451300512239047_dp /)
!     Real(dp) :: fi(1:n)  !function values
!     Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
!     Integer :: i

!     c1 = 0.5_dp * (z-Zb(10))
!     c2 = 0.5_dp * (z+Zb(10))
!     Do i = 1,n
!         fi(i) = nN2_integrand(c1 * xi(i) + c2,10)
!     End Do
!     q = c1 * Dot_Product(wi,fi)
! End Function GL_Quad_nN2_10a

! Function GL_Quad_nN2_10b(z) Result(q)  !for 185 to 250 km
!     Use Kinds, Only: dp
!     Implicit None
!     Real(dp):: q    !the result of the integration
!     Real(dp), Intent(In) :: z    !limit of integration
!     Integer, Parameter :: n = 9
!     Real(dp), Parameter :: wi(1:n) = (/  0.0812743883615744119718921581105236506756617207824107507111076769_dp, &
!                                       &  0.1806481606948574040584720312429128095143378217320404844983359065_dp, &
!                                       &  0.2606106964029354623187428694186328497718402044372999519399970021_dp, &
!                                       &  0.3123470770400028400686304065844436655987548612619046455540111656_dp, &
!                                       &  0.3302393550012597631645250692869740488788107835726883345930964979_dp, &
!                                       &  0.3123470770400028400686304065844436655987548612619046455540111656_dp, &
!                                       &  0.2606106964029354623187428694186328497718402044372999519399970021_dp, &
!                                       &  0.1806481606948574040584720312429128095143378217320404844983359065_dp, &
!                                       &  0.0812743883615744119718921581105236506756617207824107507111076769_dp /)
!     Real(dp), Parameter :: xi(1:n) = (/ -0.9681602395076260898355762029036728700494048004919253295500233118_dp, &
!                                       & -0.8360311073266357942994297880697348765441067181246759961043719796_dp, &
!                                       & -0.6133714327005903973087020393414741847857206049405646928728129423_dp, &
!                                       & -0.3242534234038089290385380146433366085719562607369730888270474768_dp, &
!                                       &  0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
!                                       &  0.3242534234038089290385380146433366085719562607369730888270474768_dp, &
!                                       &  0.6133714327005903973087020393414741847857206049405646928728129423_dp, &
!                                       &  0.8360311073266357942994297880697348765441067181246759961043719796_dp, &
!                                       &  0.9681602395076260898355762029036728700494048004919253295500233118_dp /)
!     Real(dp) :: fi(1:n)  !function values
!     Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
!     Integer :: i

!     c1 = 0.5_dp * (z-185._dp)
!     c2 = 0.5_dp * (z+185._dp)
!     Do i = 1,n
!         fi(i) = nN2_integrand(c1 * xi(i) + c2,10)
!     End Do
!     q = c1 * Dot_Product(wi,fi)
! End Function GL_Quad_nN2_10b

! Function GL_Quad_nN2_10c(z) Result(q)  !for 250 to 500 km
!     Use Kinds, Only: dp
!     Implicit None
!     Real(dp):: q    !the result of the integration
!     Real(dp), Intent(In) :: z    !limit of integration
!     Integer, Parameter :: n = 12
!     Real(dp), Parameter :: wi(1:n) = (/  0.0471753363865118271946159614850170603170290739948470895605053470_dp, &
!                                       &  0.1069393259953184309602547181939962242145701734703248800051260421_dp, &
!                                       &  0.1600783285433462263346525295433590718720117304908641779098995442_dp, &
!                                       &  0.2031674267230659217490644558097983765065181472745901463985945658_dp, &
!                                       &  0.2334925365383548087608498989248780562594099721997548747305234978_dp, &
!                                       &  0.2491470458134027850005624360429512108304609025696188313953510031_dp, &
!                                       &  0.2491470458134027850005624360429512108304609025696188313953510031_dp, &
!                                       &  0.2334925365383548087608498989248780562594099721997548747305234978_dp, &
!                                       &  0.2031674267230659217490644558097983765065181472745901463985945658_dp, &
!                                       &  0.1600783285433462263346525295433590718720117304908641779098995442_dp, &
!                                       &  0.1069393259953184309602547181939962242145701734703248800051260421_dp, &
!                                       &  0.0471753363865118271946159614850170603170290739948470895605053470_dp /)
!     Real(dp), Parameter :: xi(1:n) = (/ -0.9815606342467192506905490901492808229601551998137315104626821218_dp, &
!                                       & -0.9041172563704748566784658661190961925375967092132975465540757607_dp, &
!                                       & -0.7699026741943046870368938332128180759849257500189316376644190642_dp, &
!                                       & -0.5873179542866174472967024189405342803690985140480524815102708797_dp, &
!                                       & -0.3678314989981801937526915366437175612563601413354096213117998795_dp, &
!                                       & -0.1252334085114689154724413694638531299833969163054442732129217547_dp, &
!                                       &  0.1252334085114689154724413694638531299833969163054442732129217547_dp, &
!                                       &  0.3678314989981801937526915366437175612563601413354096213117998795_dp, &
!                                       &  0.5873179542866174472967024189405342803690985140480524815102708797_dp, &
!                                       &  0.7699026741943046870368938332128180759849257500189316376644190642_dp, &
!                                       &  0.9041172563704748566784658661190961925375967092132975465540757607_dp, &
!                                       &  0.9815606342467192506905490901492808229601551998137315104626821218_dp /)
!     Real(dp) :: fi(1:n)  !function values
!     Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
!     Integer :: i

!     c1 = 0.5_dp * (z-250._dp)
!     c2 = 0.5_dp * (z+250._dp)
!     Do i = 1,n
!         fi(i) = nN2_integrand(c1 * xi(i) + c2,10)
!     End Do
!     q = c1 * Dot_Product(wi,fi)
! End Function GL_Quad_nN2_10c

! Function GL_Quad_nN2_10d(z) Result(q)  !for 500 to 1000 km
!     Use Kinds, Only: dp
!     Implicit None
!     Real(dp):: q    !the result of the integration
!     Real(dp), Intent(In) :: z    !limit of integration
!     Integer, Parameter :: n = 11
!     Real(dp), Parameter :: wi(1:n) = (/  0.0556685671161736664827537204425485787285156256968981483483842857_dp, &
!                                       &  0.1255803694649046246346942992239401001976157913954035006639340108_dp, &
!                                       &  0.1862902109277342514260976414316558916912847480402034117815064042_dp, &
!                                       &  0.2331937645919904799185237048431751394317981723169585090273197221_dp, &
!                                       &  0.2628045445102466621806888698905091953727646776031445563800553715_dp, &
!                                       &  0.2729250867779006307144835283363421891560419698947837475976004115_dp, &
!                                       &  0.2628045445102466621806888698905091953727646776031445563800553715_dp, &
!                                       &  0.2331937645919904799185237048431751394317981723169585090273197221_dp, &
!                                       &  0.1862902109277342514260976414316558916912847480402034117815064042_dp, &
!                                       &  0.1255803694649046246346942992239401001976157913954035006639340108_dp, &
!                                       &  0.0556685671161736664827537204425485787285156256968981483483842857_dp /)
!     Real(dp), Parameter :: xi(1:n) = (/ -0.9782286581460569928039380011228573907714224089197844154258010660_dp, &
!                                       & -0.8870625997680952990751577693039272666316757512253143849674110555_dp, &
!                                       & -0.7301520055740493240934162520311534580496430620261303119783783397_dp, &
!                                       & -0.5190961292068118159257256694586095544802271151199284890209226115_dp, &
!                                       & -0.2695431559523449723315319854008615246796218624390522816239256319_dp, &
!                                       &  0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
!                                       &  0.2695431559523449723315319854008615246796218624390522816239256319_dp, &
!                                       &  0.5190961292068118159257256694586095544802271151199284890209226115_dp, &
!                                       &  0.7301520055740493240934162520311534580496430620261303119783783397_dp, &
!                                       &  0.8870625997680952990751577693039272666316757512253143849674110555_dp, &
!                                       &  0.9782286581460569928039380011228573907714224089197844154258010660_dp /)
!     Real(dp) :: fi(1:n)  !function values
!     Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
!     Integer :: i

!     c1 = 0.5_dp * (z-500._dp)
!     c2 = 0.5_dp * (z+500._dp)
!     Do i = 1,n
!         fi(i) = nN2_integrand(c1 * xi(i) + c2,10)
!     End Do
!     q = c1 * Dot_Product(wi,fi)
! End Function GL_Quad_nN2_10d

Function nO1_O2_powers(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Logical :: Z_below_97
    Real(dp), Parameter :: xb(1:2,8:10) = Reshape( (/ -1.2335158785531963_dp, &  !O1, Z = 91km
                                                    &  0.8987089660301266_dp, &  !O2, Z = 91km
                                                    & -1.2350403922105473_dp, &  !O1, Z = 110km
                                                    &  4.5003526937771792_dp, &  !O2, Z = 110km
                                                    & -0.7312338738839582_dp, &  !O1, Z = 120km
                                                    &  5.8804681169361661_dp  /), &  !O2, Z = 120km
                                                    & (/2,3/) )
    Real(dp), Parameter :: xb_95(1:2) =  (/ -1.6326227572400906_dp, &  !O1, Z = 95km
                                          &  1.6401385731339722_dp  /) !O2, Z = 95km
    Real(dp), Parameter :: xb_97(1:2) =  (/ -1.6736396506936235_dp, &  !O1, Z = 97km
                                          &  2.0206764985042174_dp  /) !O2, Z = 97km
    Real(dp), Parameter :: xb_100(1:2) = (/ -1.6519505201748657_dp, &  !O1, Z = 100km
                                          &  2.6026369578525212_dp  /) !O2, Z = 100km
    Real(dp), Parameter :: xb_115(1:2) = (/ -0.9801497240119917_dp, &  !O1, Z = 115km
                                          &  5.2767050379951348_dp  /) !O2, Z = 115km
    Real(dp), Parameter :: xb_185(1:2) = (/  1.2150532731374728_dp, &  !O1, Z = 185km
                                          &  9.8096715732225821_dp  /) !O2, Z = 185km
    Real(dp), Parameter :: xb_250(1:2) = (/  2.5093458284324299_dp, &  !O1, Z = 250km
                                          &  12.3982566838124963_dp  /) !O2, Z = 250km
    Real(dp), Parameter :: xb_500(1:2) = (/  6.7754756108126095_dp, &  !O1, Z = 500km
                                          &  20.9305162485728573_dp  /) !O2, Z = 500km
    Logical, Parameter :: no_sublayers(7:10) = (/ .TRUE.,  &
                                                & .FALSE., &
                                                & .FALSE., &
                                                & .TRUE.   /)
    !precomputed parameters for b=10
    Real(dp), Parameter :: R_Earth_sq = R_earth**2
    Real(dp), Parameter :: R0sq_over_lambda_Rz10sq = R_earth**2 / (lambda * R_Z10**2)
    Real(dp), Parameter :: g0_over_Tinf = g0 / T_inf
    Real(dp), Parameter :: M_over_R(1:2) = Mi(2:3) / R_star

    If (Z .LE. 97._dp) Then
        Z_below_97 = .TRUE.
    Else
        Z_below_97 = .FALSE.
    End If
    If (no_sublayers(b)) Then
        If (Z_below_97) Then !b=7
            ! x = Romberg_Quad_nO1_O2(nO1_O2_integrand1,Zb(7),Z,7)
            x = GL_Quad_nO1_O2_7(Z)
        Else !b=10
            !partial direct evaluation
            x = g0_over_Tinf * (R0sq_over_lambda_Rz10sq * Log(T(Z,11)) - (R_Earth_sq / (R_Earth + Z)))
            ! x = xb(:,10) + Romberg_Quad_nO1_O2(nO1_O2_integrand5,Zb(10),Z,10)
            !Layer 11 (b=10) is further subdivided to keep number of quad points manageable
            If (Z .LT. 185._dp) Then
                x = xb(:,10) + M_over_R * (x + GL_Quad_nO1_O2_10a(Z))
                ! x = xb(:,10) + GL_Quad_nO1_O2_10a(Z)
            Else If (Z .LT. 250._dp) Then
                x = xb_185 + M_over_R * (x + GL_Quad_nO1_O2_10b(Z))
                ! x = xb_185 + GL_Quad_nO1_O2_10b(Z)
            Else If (Z .LT. 500._dp) Then
                x = xb_250 + M_over_R * (x + GL_Quad_nO1_O2_10c(Z))
                ! x = xb_250 + GL_Quad_nO1_O2_10c(Z)
            Else !Z = 500 to 1000 km
                x = xb_500 + M_over_R * (x + GL_Quad_nO1_O2_10d(Z))
                ! x = xb_500 + GL_Quad_nO1_O2_10d(Z)
            End If
        End If
    Else
        If (Z_below_97) Then !b=8
            If (Z .LT. 95._dp) Then
                ! x = xb(:,8) + Romberg_Quad_nO1_O2(nO1_O2_integrand1,Zb(8),Z,8)
                x = xb(:,8) + GL_Quad_nO1_O2_8a(Z)
            Else
                ! x = xb_95 + Romberg_Quad_nO1_O2(nO1_O2_integrand2,95._dp,Z,8)
                x = xb_95 + GL_Quad_nO1_O2_8b(Z)
            End If
        Else !b=8 OR b=9
            If (Z .LT. 100._dp) Then !b=8
                ! x = xb_97 + Romberg_Quad_nO1_O2(nO1_O2_integrand2,97._dp,Z,8)
                x = xb_97 + GL_Quad_nO1_O2_8c(Z)
            Else If (b .EQ. 8) Then !b=8
                ! x = xb_100 + Romberg_Quad_nO1_O2(nO1_O2_integrand3,100._dp,Z,8)
                x = xb_100 + GL_Quad_nO1_O2_8d(Z)
            Else !b=9
                If (Z .LT. 115._dp) Then
                    ! x = xb(:,9) + Romberg_Quad_nO1_O2(nO1_O2_integrand4,Zb(9),Z,9)
                    x = xb(:,9) + GL_Quad_nO1_O2_9a(Z)
                Else
                    ! x = xb_115 + Romberg_Quad_nO1_O2(nO1_O2_integrand5,115._dp,Z,9)
                    x = xb_115 + GL_Quad_nO1_O2_9b(Z)
                End If
            End If
        End If
    End If
End Function nO1_O2_powers

Function K95to115(Z) Result(K)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: K
    Real(dp), Intent(In) :: Z
    Real(dp) :: x
    
    If (Z .LT. 95._dp) Then
        K = K0
    Else If (Z .LT. 115._dp) Then
        x = (Z - 95._dp)**2
        K = K0 * Exp(-x / (400._dp - x))  !US Standard Atmosphere 1976 equation 7b
    Else
        K = 0._dp
    End If
End Function K95to115

Function nO1_O2_integrand1(Z,b) Result(f)  !for 86 to 95 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    
    Tz = T(Z,b+1)
    D = ai(2:3) * (Tz / 273.15_dp)**bi(2:3) / (N7(1) * Tb(7) * Exp(-nN2_power(Z,b)) / Tz)
    f = g(Z) * D * (Mi(2:3) + M0*K0/D) / (R_star * Tz * (D + K0)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
    f(1) = f(1) + littleQi * (littleUi - Z)**2 * Exp(-littleWi*(littleUi - Z)**3)
End Function nO1_O2_integrand1

Function nO1_O2_integrand2(Z,b) Result(f)  !for 95 to 97 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    Real(dp) :: K
    
    Tz = T(Z,b+1)
    D = ai(2:3) * (Tz / 273.15_dp)**bi(2:3) / (N7(1) * Tb(7) * Exp(-nN2_power(Z,b)) / Tz)
    K = K95to115(Z)
    f = g(Z) * D * (Mi(2:3) + M0*K/D) / (R_star * Tz * (D + K)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
    f(1) = f(1) + littleQi * (littleUi - Z)**2 * Exp(-littleWi*(littleUi - Z)**3)
End Function nO1_O2_integrand2

Function nO1_O2_integrand3(Z,b) Result(f)  !for 97 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    Real(dp) :: K

    Tz = T(Z,b+1)
    D = ai(2:3) * (Tz / 273.15_dp)**bi(2:3) / (N7(1) * Tb(7) * Exp(-nN2_power(Z,b)) / Tz)
    K = K95to115(Z)
    f = g(Z) * D * (Mi(2:3) + M0*K/D) / (R_star * Tz * (D + K)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
End Function nO1_O2_integrand3

Function nO1_O2_integrand4(Z,b) Result(f)  !for 100 to 115 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    Real(dp) :: K
    
    Tz = T(Z,b+1)
    D = ai(2:3) * (Tz / 273.15_dp)**bi(2:3) / (N7(1) * Tb(7) * Exp(-nN2_power(Z,b)) / Tz)
    K = K95to115(Z)
    f = g(Z) * D * (Mi(2:3) + Mi(1)*K/D) / (R_star * Tz * (D + K)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
End Function nO1_O2_integrand4

Function nO1_O2_integrand5(Z,b) Result(f)  !for 115 to 1000 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b

    ! f = g(Z) * Mi(2:3) / (R_star * T(Z,b+1)) + & 
    !   & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
    f = bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
End Function nO1_O2_integrand5

Function GL_Quad_nO1_O2_7(z) Result(q)  !for 86 to 91 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 8
    Real(dp), Parameter :: wi(1:n) = (/  0.1012285362903762591525313543099621901153940910516849570590036981_dp, &
                                      &  0.2223810344533744705443559944262408844301308700512495647259092893_dp, &
                                      &  0.3137066458778872873379622019866013132603289990027349376902639451_dp, &
                                      &  0.3626837833783619829651504492771956121941460398943305405248230676_dp, &
                                      &  0.3626837833783619829651504492771956121941460398943305405248230676_dp, &
                                      &  0.3137066458778872873379622019866013132603289990027349376902639451_dp, &
                                      &  0.2223810344533744705443559944262408844301308700512495647259092893_dp, &
                                      &  0.1012285362903762591525313543099621901153940910516849570590036981_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9602898564975362316835608685694729904282352343014520382716397774_dp, &
                                      & -0.7966664774136267395915539364758304368371717316159648320701702950_dp, &
                                      & -0.5255324099163289858177390491892463490419642431203928577508570993_dp, &
                                      & -0.1834346424956498049394761423601839806667578129129737823171884737_dp, &
                                      &  0.1834346424956498049394761423601839806667578129129737823171884737_dp, &
                                      &  0.5255324099163289858177390491892463490419642431203928577508570993_dp, &
                                      &  0.7966664774136267395915539364758304368371717316159648320701702950_dp, &
                                      &  0.9602898564975362316835608685694729904282352343014520382716397774_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(7))
    c2 = 0.5_dp * (z+Zb(7))
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand1(c1 * xi(i) + c2,7)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_7

Function GL_Quad_nO1_O2_8a(z) Result(q)  !for 91 to 95 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 7
    Real(dp), Parameter :: wi(1:n) = (/  0.1294849661688696932706114326790820183285874022599466639772086387_dp, &
                                      &  0.2797053914892766679014677714237795824869250652265987645370140327_dp, &
                                      &  0.3818300505051189449503697754889751338783650835338627347510834510_dp, &
                                      &  0.4179591836734693877551020408163265306122448979591836734693877551_dp, &
                                      &  0.3818300505051189449503697754889751338783650835338627347510834510_dp, &
                                      &  0.2797053914892766679014677714237795824869250652265987645370140327_dp, &
                                      &  0.1294849661688696932706114326790820183285874022599466639772086387_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9491079123427585245261896840478512624007709376706177835487691039_dp, &
                                      & -0.7415311855993944398638647732807884070741476471413902601199553520_dp, &
                                      & -0.4058451513773971669066064120769614633473820140993701263870432518_dp, &
                                      &  0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                      &  0.4058451513773971669066064120769614633473820140993701263870432518_dp, &
                                      &  0.7415311855993944398638647732807884070741476471413902601199553520_dp, &
                                      &  0.9491079123427585245261896840478512624007709376706177835487691039_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(8))
    c2 = 0.5_dp * (z+Zb(8))
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand1(c1 * xi(i) + c2,8)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_8a

Function GL_Quad_nO1_O2_8b(z) Result(q)  !for 95 to 97 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 6
    Real(dp), Parameter :: wi(1:n) = (/  0.1713244923791703450402961421727328935268225014840439823986354398_dp, &
                                      &  0.3607615730481386075698335138377161116615218927467454822897392402_dp, &
                                      &  0.4679139345726910473898703439895509948116556057692105353116253200_dp, &
                                      &  0.4679139345726910473898703439895509948116556057692105353116253200_dp, &
                                      &  0.3607615730481386075698335138377161116615218927467454822897392402_dp, &
                                      &  0.1713244923791703450402961421727328935268225014840439823986354398_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9324695142031520278123015544939946091347657377122898248725496165_dp, &
                                      & -0.6612093864662645136613995950199053470064485643951700708145267059_dp, &
                                      & -0.2386191860831969086305017216807119354186106301400213501813951646_dp, &
                                      &  0.2386191860831969086305017216807119354186106301400213501813951646_dp, &
                                      &  0.6612093864662645136613995950199053470064485643951700708145267059_dp, &
                                      &  0.9324695142031520278123015544939946091347657377122898248725496165_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-95._dp)
    c2 = 0.5_dp * (z+95._dp)
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand2(c1 * xi(i) + c2,8)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_8b

Function GL_Quad_nO1_O2_8c(z) Result(q)  !for 97 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 6
    Real(dp), Parameter :: wi(1:n) = (/  0.1713244923791703450402961421727328935268225014840439823986354398_dp, &
                                      &  0.3607615730481386075698335138377161116615218927467454822897392402_dp, &
                                      &  0.4679139345726910473898703439895509948116556057692105353116253200_dp, &
                                      &  0.4679139345726910473898703439895509948116556057692105353116253200_dp, &
                                      &  0.3607615730481386075698335138377161116615218927467454822897392402_dp, &
                                      &  0.1713244923791703450402961421727328935268225014840439823986354398_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9324695142031520278123015544939946091347657377122898248725496165_dp, &
                                      & -0.6612093864662645136613995950199053470064485643951700708145267059_dp, &
                                      & -0.2386191860831969086305017216807119354186106301400213501813951646_dp, &
                                      &  0.2386191860831969086305017216807119354186106301400213501813951646_dp, &
                                      &  0.6612093864662645136613995950199053470064485643951700708145267059_dp, &
                                      &  0.9324695142031520278123015544939946091347657377122898248725496165_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-97._dp)
    c2 = 0.5_dp * (z+97._dp)
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand3(c1 * xi(i) + c2,8)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_8c

Function GL_Quad_nO1_O2_8d(z) Result(q)  !for 100 to 110 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 20
    Real(dp), Parameter :: wi(1:n) = (/  0.0176140071391521183118619623518528163621431055433367325243493267_dp, &
                                      &  0.0406014298003869413310399522749321098790906399899515368176068546_dp, &
                                      &  0.0626720483341090635695065351870416063516010765784363640995843454_dp, &
                                      &  0.0832767415767047487247581432220462061001778285831632907448820608_dp, &
                                      &  0.1019301198172404350367501354803498761666916560233925562619716162_dp, &
                                      &  0.1181945319615184173123773777113822870050412195489687754468899520_dp, &
                                      &  0.1316886384491766268984944997481631349161105111469835269964364937_dp, &
                                      &  0.1420961093183820513292983250671649330345154133920203033373670830_dp, &
                                      &  0.1491729864726037467878287370019694366926799040813683164962112178_dp, &
                                      &  0.1527533871307258506980843319550975934919486451123785972747010498_dp, &
                                      &  0.1527533871307258506980843319550975934919486451123785972747010498_dp, &
                                      &  0.1491729864726037467878287370019694366926799040813683164962112178_dp, &
                                      &  0.1420961093183820513292983250671649330345154133920203033373670830_dp, &
                                      &  0.1316886384491766268984944997481631349161105111469835269964364937_dp, &
                                      &  0.1181945319615184173123773777113822870050412195489687754468899520_dp, &
                                      &  0.1019301198172404350367501354803498761666916560233925562619716162_dp, &
                                      &  0.0832767415767047487247581432220462061001778285831632907448820608_dp, &
                                      &  0.0626720483341090635695065351870416063516010765784363640995843454_dp, &
                                      &  0.0406014298003869413310399522749321098790906399899515368176068546_dp, &
                                      &  0.0176140071391521183118619623518528163621431055433367325243493267_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9931285991850949247861223884713202782226471309016558961481841312_dp, &
                                      & -0.9639719272779137912676661311972772219120603278061888560635375939_dp, &
                                      & -0.9122344282513259058677524412032981130491847974236917747958822192_dp, &
                                      & -0.8391169718222188233945290617015206853296293650656373732524927255_dp, &
                                      & -0.7463319064601507926143050703556415903107306795691764441395459061_dp, &
                                      & -0.6360536807265150254528366962262859367433891167993684639394466225_dp, &
                                      & -0.5108670019508270980043640509552509984254913292024268334723486199_dp, &
                                      & -0.3737060887154195606725481770249272373957463217056827118279486135_dp, &
                                      & -0.2277858511416450780804961953685746247430889376829274723146357392_dp, &
                                      & -0.0765265211334973337546404093988382110047962668134975008047952444_dp, &
                                      &  0.0765265211334973337546404093988382110047962668134975008047952444_dp, &
                                      &  0.2277858511416450780804961953685746247430889376829274723146357392_dp, &
                                      &  0.3737060887154195606725481770249272373957463217056827118279486135_dp, &
                                      &  0.5108670019508270980043640509552509984254913292024268334723486199_dp, &
                                      &  0.6360536807265150254528366962262859367433891167993684639394466225_dp, &
                                      &  0.7463319064601507926143050703556415903107306795691764441395459061_dp, &
                                      &  0.8391169718222188233945290617015206853296293650656373732524927255_dp, &
                                      &  0.9122344282513259058677524412032981130491847974236917747958822192_dp, &
                                      &  0.9639719272779137912676661311972772219120603278061888560635375939_dp, &
                                      &  0.9931285991850949247861223884713202782226471309016558961481841312_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-100._dp)
    c2 = 0.5_dp * (z+100._dp)
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand4(c1 * xi(i) + c2,8)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_8d

Function GL_Quad_nO1_O2_9a(z) Result(q)  !for 110 to 115 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 26
    Real(dp), Parameter :: wi(1:n) = (/  0.0105513726173430071556511876852519779043457371796946145500554276_dp, &
                                      &  0.0244178510926319087896158275197884002404959261972703420520468951_dp, &
                                      &  0.0379623832943627639503031412488504946907704217440489576753118247_dp, &
                                      &  0.0509758252971478119983199007240733795258793185429719348238376860_dp, &
                                      &  0.0632740463295748355394536899070450957222228420126945106660958866_dp, &
                                      &  0.0746841497656597458870757961028483386387471618810406807790308417_dp, &
                                      &  0.0850458943134852392104477650799816965839203167766782761860561814_dp, &
                                      &  0.0942138003559141484636648830673031985491657459142072798633149024_dp, &
                                      &  0.1020591610944254232384140702534307923272088113542420103318738194_dp, &
                                      &  0.1084718405285765906565794267279975822894382034213066445296057086_dp, &
                                      &  0.1133618165463196665494407184425981452459503629250152139882879299_dp, &
                                      &  0.1166604434852965820446625075403622136319648812219242164601626972_dp, &
                                      &  0.1183214152792622765163710857004686846498902708289053180943201994_dp, &
                                      &  0.1183214152792622765163710857004686846498902708289053180943201994_dp, &
                                      &  0.1166604434852965820446625075403622136319648812219242164601626972_dp, &
                                      &  0.1133618165463196665494407184425981452459503629250152139882879299_dp, &
                                      &  0.1084718405285765906565794267279975822894382034213066445296057086_dp, &
                                      &  0.1020591610944254232384140702534307923272088113542420103318738194_dp, &
                                      &  0.0942138003559141484636648830673031985491657459142072798633149024_dp, &
                                      &  0.0850458943134852392104477650799816965839203167766782761860561814_dp, &
                                      &  0.0746841497656597458870757961028483386387471618810406807790308417_dp, &
                                      &  0.0632740463295748355394536899070450957222228420126945106660958866_dp, &
                                      &  0.0509758252971478119983199007240733795258793185429719348238376860_dp, &
                                      &  0.0379623832943627639503031412488504946907704217440489576753118247_dp, &
                                      &  0.0244178510926319087896158275197884002404959261972703420520468951_dp, &
                                      &  0.0105513726173430071556511876852519779043457371796946145500554276_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9958857011456169290032169593229060259568578551195596673965278094_dp, &
                                      & -0.9783854459564709911005803543119263054469165637473185024854148613_dp, &
                                      & -0.9471590666617142501359152835180367545207103376843152700182677796_dp, &
                                      & -0.9026378619843070742176655992312102513411240053729053725551656548_dp, &
                                      & -0.8454459427884980187975070614678381615720751020913295694301764729_dp, &
                                      & -0.7763859488206788561929672472422838669762182562551948721631684216_dp, &
                                      & -0.6964272604199572648638139137294246978607533177908191517045574538_dp, &
                                      & -0.6066922930176180632319787469168870544352645722659098903786433125_dp, &
                                      & -0.5084407148245057176957030647255691753763067541857508137211004696_dp, &
                                      & -0.4030517551234863064810773770988831036599374096993161825891411335_dp, &
                                      & -0.2920048394859568951428353820778302968847193869629903782123441919_dp, &
                                      & -0.1768588203568901839690577484183447499773837638012642614803476998_dp, &
                                      & -0.0592300934293132070937185751984033607902347353890355821542722917_dp, &
                                      &  0.0592300934293132070937185751984033607902347353890355821542722917_dp, &
                                      &  0.1768588203568901839690577484183447499773837638012642614803476998_dp, &
                                      &  0.2920048394859568951428353820778302968847193869629903782123441919_dp, &
                                      &  0.4030517551234863064810773770988831036599374096993161825891411335_dp, &
                                      &  0.5084407148245057176957030647255691753763067541857508137211004696_dp, &
                                      &  0.6066922930176180632319787469168870544352645722659098903786433125_dp, &
                                      &  0.6964272604199572648638139137294246978607533177908191517045574538_dp, &
                                      &  0.7763859488206788561929672472422838669762182562551948721631684216_dp, &
                                      &  0.8454459427884980187975070614678381615720751020913295694301764729_dp, &
                                      &  0.9026378619843070742176655992312102513411240053729053725551656548_dp, &
                                      &  0.9471590666617142501359152835180367545207103376843152700182677796_dp, &
                                      &  0.9783854459564709911005803543119263054469165637473185024854148613_dp, &
                                      &  0.9958857011456169290032169593229060259568578551195596673965278094_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(9))
    c2 = 0.5_dp * (z+Zb(9))
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand4(c1 * xi(i) + c2,9)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_9a

Function GL_Quad_nO1_O2_9b(z) Result(q)  !for 115 to 120 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 6
    Real(dp), Parameter :: wi(1:n) = (/  0.1713244923791703450402961421727328935268225014840439823986354398_dp, &
                                      &  0.3607615730481386075698335138377161116615218927467454822897392402_dp, &
                                      &  0.4679139345726910473898703439895509948116556057692105353116253200_dp, &
                                      &  0.4679139345726910473898703439895509948116556057692105353116253200_dp, &
                                      &  0.3607615730481386075698335138377161116615218927467454822897392402_dp, &
                                      &  0.1713244923791703450402961421727328935268225014840439823986354398_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9324695142031520278123015544939946091347657377122898248725496165_dp, &
                                      & -0.6612093864662645136613995950199053470064485643951700708145267059_dp, &
                                      & -0.2386191860831969086305017216807119354186106301400213501813951646_dp, &
                                      &  0.2386191860831969086305017216807119354186106301400213501813951646_dp, &
                                      &  0.6612093864662645136613995950199053470064485643951700708145267059_dp, &
                                      &  0.9324695142031520278123015544939946091347657377122898248725496165_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-115._dp)
    c2 = 0.5_dp * (z+115._dp)
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand5(c1 * xi(i) + c2,9)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_9b

Function GL_Quad_nO1_O2_10a(z) Result(q)  !for 120 to 185 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 19
    Real(dp), Parameter :: wi(1:n) = (/  0.0194617882297264770363120414644384357529066090692866407926804725_dp, &
                                      &  0.0448142267656996003328381574019942119517542274678576020858545268_dp, &
                                      &  0.0690445427376412265807082580060130449618480316876131311229004277_dp, &
                                      &  0.0914900216224499994644620941238396526609116512965987846649893449_dp, &
                                      &  0.1115666455473339947160239016817659974813318538398937755214611429_dp, &
                                      &  0.1287539625393362276755157848568771170558395770934630345471043846_dp, &
                                      &  0.1426067021736066117757461094419029724756683448244738609265716559_dp, &
                                      &  0.1527660420658596667788554008976629984610082672364286235231015531_dp, &
                                      &  0.1589688433939543476499564394650472016787801581951260957511749153_dp, &
                                      &  0.1610544498487836959791636253209167350399025585785169021283231527_dp, &
                                      &  0.1589688433939543476499564394650472016787801581951260957511749153_dp, &
                                      &  0.1527660420658596667788554008976629984610082672364286235231015531_dp, &
                                      &  0.1426067021736066117757461094419029724756683448244738609265716559_dp, &
                                      &  0.1287539625393362276755157848568771170558395770934630345471043846_dp, &
                                      &  0.1115666455473339947160239016817659974813318538398937755214611429_dp, &
                                      &  0.0914900216224499994644620941238396526609116512965987846649893449_dp, &
                                      &  0.0690445427376412265807082580060130449618480316876131311229004277_dp, &
                                      &  0.0448142267656996003328381574019942119517542274678576020858545268_dp, &
                                      &  0.0194617882297264770363120414644384357529066090692866407926804725_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9924068438435844031890176702532604935893164014032107866796794391_dp, &
                                      & -0.9602081521348300308527788406876515266150915032741381059178163344_dp, &
                                      & -0.9031559036148179016426609285323124878093939340573558176950324248_dp, &
                                      & -0.8227146565371428249789224867127139017745384862068300413698638886_dp, &
                                      & -0.7209661773352293786170958608237816296571418329086667749385904714_dp, &
                                      & -0.6005453046616810234696381649462392798683220827322925675865867257_dp, &
                                      & -0.4645707413759609457172671481041023679762857146241365969843088776_dp, &
                                      & -0.3165640999636298319901173288498449178922852191328872451557289868_dp, &
                                      & -0.1603586456402253758680961157407435495048735004708753788746434516_dp, &
                                      &  0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                      &  0.1603586456402253758680961157407435495048735004708753788746434516_dp, &
                                      &  0.3165640999636298319901173288498449178922852191328872451557289868_dp, &
                                      &  0.4645707413759609457172671481041023679762857146241365969843088776_dp, &
                                      &  0.6005453046616810234696381649462392798683220827322925675865867257_dp, &
                                      &  0.7209661773352293786170958608237816296571418329086667749385904714_dp, &
                                      &  0.8227146565371428249789224867127139017745384862068300413698638886_dp, &
                                      &  0.9031559036148179016426609285323124878093939340573558176950324248_dp, &
                                      &  0.9602081521348300308527788406876515266150915032741381059178163344_dp, &
                                      &  0.9924068438435844031890176702532604935893164014032107866796794391_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(10))
    c2 = 0.5_dp * (z+Zb(10))
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand5(c1 * xi(i) + c2,10)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_10a

Function GL_Quad_nO1_O2_10b(z) Result(q)  !for 185 to 250 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 9
    Real(dp), Parameter :: wi(1:n) = (/  0.0812743883615744119718921581105236506756617207824107507111076769_dp, &
                                      &  0.1806481606948574040584720312429128095143378217320404844983359065_dp, &
                                      &  0.2606106964029354623187428694186328497718402044372999519399970021_dp, &
                                      &  0.3123470770400028400686304065844436655987548612619046455540111656_dp, &
                                      &  0.3302393550012597631645250692869740488788107835726883345930964979_dp, &
                                      &  0.3123470770400028400686304065844436655987548612619046455540111656_dp, &
                                      &  0.2606106964029354623187428694186328497718402044372999519399970021_dp, &
                                      &  0.1806481606948574040584720312429128095143378217320404844983359065_dp, &
                                      &  0.0812743883615744119718921581105236506756617207824107507111076769_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9681602395076260898355762029036728700494048004919253295500233118_dp, &
                                      & -0.8360311073266357942994297880697348765441067181246759961043719796_dp, &
                                      & -0.6133714327005903973087020393414741847857206049405646928728129423_dp, &
                                      & -0.3242534234038089290385380146433366085719562607369730888270474768_dp, &
                                      &  0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                      &  0.3242534234038089290385380146433366085719562607369730888270474768_dp, &
                                      &  0.6133714327005903973087020393414741847857206049405646928728129423_dp, &
                                      &  0.8360311073266357942994297880697348765441067181246759961043719796_dp, &
                                      &  0.9681602395076260898355762029036728700494048004919253295500233118_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-185._dp)
    c2 = 0.5_dp * (z+185._dp)
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand5(c1 * xi(i) + c2,10)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_10b

Function GL_Quad_nO1_O2_10c(z) Result(q)  !for 250 to 500 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 12
    Real(dp), Parameter :: wi(1:n) = (/  0.0471753363865118271946159614850170603170290739948470895605053470_dp, &
                                      &  0.1069393259953184309602547181939962242145701734703248800051260421_dp, &
                                      &  0.1600783285433462263346525295433590718720117304908641779098995442_dp, &
                                      &  0.2031674267230659217490644558097983765065181472745901463985945658_dp, &
                                      &  0.2334925365383548087608498989248780562594099721997548747305234978_dp, &
                                      &  0.2491470458134027850005624360429512108304609025696188313953510031_dp, &
                                      &  0.2491470458134027850005624360429512108304609025696188313953510031_dp, &
                                      &  0.2334925365383548087608498989248780562594099721997548747305234978_dp, &
                                      &  0.2031674267230659217490644558097983765065181472745901463985945658_dp, &
                                      &  0.1600783285433462263346525295433590718720117304908641779098995442_dp, &
                                      &  0.1069393259953184309602547181939962242145701734703248800051260421_dp, &
                                      &  0.0471753363865118271946159614850170603170290739948470895605053470_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9815606342467192506905490901492808229601551998137315104626821218_dp, &
                                      & -0.9041172563704748566784658661190961925375967092132975465540757607_dp, &
                                      & -0.7699026741943046870368938332128180759849257500189316376644190642_dp, &
                                      & -0.5873179542866174472967024189405342803690985140480524815102708797_dp, &
                                      & -0.3678314989981801937526915366437175612563601413354096213117998795_dp, &
                                      & -0.1252334085114689154724413694638531299833969163054442732129217547_dp, &
                                      &  0.1252334085114689154724413694638531299833969163054442732129217547_dp, &
                                      &  0.3678314989981801937526915366437175612563601413354096213117998795_dp, &
                                      &  0.5873179542866174472967024189405342803690985140480524815102708797_dp, &
                                      &  0.7699026741943046870368938332128180759849257500189316376644190642_dp, &
                                      &  0.9041172563704748566784658661190961925375967092132975465540757607_dp, &
                                      &  0.9815606342467192506905490901492808229601551998137315104626821218_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-250._dp)
    c2 = 0.5_dp * (z+250._dp)
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand5(c1 * xi(i) + c2,10)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_10c

Function GL_Quad_nO1_O2_10d(z) Result(q)  !for 500 to 1000 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Parameter :: n = 10
    Real(dp), Parameter :: wi(1:n) = (/  0.0666713443086881375935688098933317928578648343201581451286948816_dp, &
                                      &  0.1494513491505805931457763396576973324025566396694273678354772688_dp, &
                                      &  0.2190863625159820439955349342281631924587718705226770898809565436_dp, &
                                      &  0.2692667193099963550912269215694693528597599384608837958005632762_dp, &
                                      &  0.2955242247147528701738929946513383294210467170268536013543080298_dp, &
                                      &  0.2955242247147528701738929946513383294210467170268536013543080298_dp, &
                                      &  0.2692667193099963550912269215694693528597599384608837958005632762_dp, &
                                      &  0.2190863625159820439955349342281631924587718705226770898809565436_dp, &
                                      &  0.1494513491505805931457763396576973324025566396694273678354772688_dp, &
                                      &  0.0666713443086881375935688098933317928578648343201581451286948816_dp /)
    Real(dp), Parameter :: xi(1:n) = (/ -0.9739065285171717200779640120844520534282699466923821192312120667_dp, &
                                      & -0.8650633666889845107320966884234930485275430149653304525219597318_dp, &
                                      & -0.6794095682990244062343273651148735757692947118348094676648171890_dp, &
                                      & -0.4333953941292471907992659431657841622000718376562464965027015131_dp, &
                                      & -0.1488743389816312108848260011297199846175648594206916957079892535_dp, &
                                      &  0.1488743389816312108848260011297199846175648594206916957079892535_dp, &
                                      &  0.4333953941292471907992659431657841622000718376562464965027015131_dp, &
                                      &  0.6794095682990244062343273651148735757692947118348094676648171890_dp, &
                                      &  0.8650633666889845107320966884234930485275430149653304525219597318_dp, &
                                      &  0.9739065285171717200779640120844520534282699466923821192312120667_dp /)
    Real(dp) :: fi(1:2,1:n)  !function values
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-500._dp)
    c2 = 0.5_dp * (z+500._dp)
    Do i = 1,n
        fi(:,i) = nO1_O2_integrand5(c1 * xi(i) + c2,10)
    End Do
    q(1) = c1 * Dot_Product(wi,fi(1,:))
    q(2) = c1 * Dot_Product(wi,fi(2,:))
End Function GL_Quad_nO1_O2_10d

Function nAr_He_powers(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Logical :: Z_below_97
    Real(dp), Parameter :: xb(1:2,8:10) = Reshape( (/  0.902943388752_dp, &  !Ar, Z = 91km
                                                    &  0.796321374781_dp, &  !He, Z = 91km
                                                    &  4.611292962115_dp, &  !Ar, Z = 110km
                                                    &  2.316170326929_dp, &  !He, Z = 110km
                                                    &  6.241723061484_dp, &  !Ar, Z = 120km
                                                    &  2.314236028135_dp  /), &  !He, Z = 120km
                                                    & (/2,3/) )
    Real(dp), Parameter :: xb_95(1:2) =  (/ 1.646377632330_dp, &  !Ar, Z = 95km
                                          & 1.337838548403_dp  /) !He, Z = 95km
    Real(dp), Parameter :: xb_97(1:2) =  (/ 2.027478460758_dp, &  !Ar, Z = 97km
                                          & 1.567057244770_dp  /) !He, Z = 97km
    Real(dp), Parameter :: xb_100(1:2) = (/ 2.611934471721_dp, &  !Ar, Z = 100km
                                          & 1.858021164734_dp  /) !He, Z = 100km
    Real(dp), Parameter :: xb_115(1:2) = (/ 5.516391259890_dp, &  !Ar, Z = 115km
                                          & 2.318014288266_dp  /) !He, Z = 115km
    Logical, Parameter :: no_sublayers(7:10) = (/ .TRUE.,  &
                                                & .FALSE., &
                                                & .FALSE., &
                                                & .TRUE.   /)
    
    If (Z .LE. 97._dp) Then
        Z_below_97 = .TRUE.
    Else
        Z_below_97 = .FALSE.
    End If
    If (no_sublayers(b)) Then
        If (Z_below_97) Then !b=7
            x = Romberg_Quad_nAr_He(nAr_He_integrand1,Zb(7),Z,7)
        Else !b=10
            x = xb(:,10) + Romberg_Quad_nAr_He(nAr_He_integrand5,Zb(10),Z,10)
        End If
    Else
        If (Z_below_97) Then !b=8
            If (Z .LT. 95._dp) Then
                x = xb(:,8) + Romberg_Quad_nAr_He(nAr_He_integrand1,Zb(8),Z,8)
            Else
                x = xb_95 + Romberg_Quad_nAr_He(nAr_He_integrand2,95._dp,Z,8)
            End If
        Else !b=8 OR b=9
            If (Z .LT. 100._dp) Then !b=8
                x = xb_97 + Romberg_Quad_nAr_He(nAr_He_integrand2,97._dp,Z,8)
            Else If (b .EQ. 8) Then !b=8
                x = xb_100 + Romberg_Quad_nAr_He(nAr_He_integrand4,100._dp,Z,8)
            Else !b=9
                If (Z .LT. 115._dp) Then
                    x = xb(:,9) + Romberg_Quad_nAr_He(nAr_He_integrand4,Zb(9),Z,9)
                Else
                    x = xb_115 + Romberg_Quad_nAr_He(nAr_He_integrand5,115._dp,Z,9)
                End If
            End If
        End If
    End If
End Function nAr_He_powers

Function nAr_He_integrand1(Z,b) Result(f)  !for 86 to 95 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: Nb(1:3)
    Real(dp) :: D(1:2)
    Real(dp) :: y(1:2)
    
    Tz = T(Z,b+1)
    Nb(1) = N7(1) * Tb(7) * Exp(-nN2_power(Z,b))
    Nb(2:3) = N7(2:3) * Tb(7) * Exp(-nO1_O2_powers(Z,b))
    Nb = Nb / Tz
    D = ai(4:5) * (Tz / 273.15_dp)**bi(4:5) / Sum(Nb)
    y = D / (R_star * Tz * (D + K0))
    f = g(Z) * y * (Mi(4:5) + M0*K0/D) + & 
      & bigQi(4:5) * (Z - bigUi(4:5))**2 * Exp(-bigWi(4:5)*(Z - bigUi(4:5))**3)
    f(2) = f(2) + y(2) * alphaHe * R_star * dT_dZ(Z,b+1)
End Function nAr_He_integrand1

Function nAr_He_integrand2(Z,b) Result(f)  !for 95 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: Nb(1:3)
    Real(dp) :: D(1:2)
    Real(dp) :: y(1:2)
    Real(dp) :: K
    
    Tz = T(Z,b+1)
    Nb(1) = N7(1) * Tb(7) * Exp(-nN2_power(Z,b))
    Nb(2:3) = N7(2:3) * Tb(7) * Exp(-nO1_O2_powers(Z,b))
    Nb = Nb / Tz
    D = ai(4:5) * (Tz / 273.15_dp)**bi(4:5) / Sum(Nb)
    K = K95to115(Z)
    y = D / (R_star * Tz * (D + K))
    f = g(Z) * y * (Mi(4:5) + M0*K/D) + & 
      & bigQi(4:5) * (Z - bigUi(4:5))**2 * Exp(-bigWi(4:5)*(Z - bigUi(4:5))**3)
    f(2) = f(2) + y(2) * alphaHe * R_star * dT_dZ(Z,b+1)
End Function nAr_He_integrand2

Function nAr_He_integrand4(Z,b) Result(f)  !for 100 to 115 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: Nb(1:3)
    Real(dp) :: D(1:2)
    Real(dp) :: y(1:2)
    Real(dp) :: K
    
    Tz = T(Z,b+1)
    Nb(1) = N7(1) * Tb(7) * Exp(-nN2_power(Z,b))
    Nb(2:3) = N7(2:3) * Tb(7) * Exp(-nO1_O2_powers(Z,b))
    Nb = Nb / Tz
    D = ai(4:5) * (Tz / 273.15_dp)**bi(4:5) / Sum(Nb)
    K = K95to115(Z)
    y = D / (R_star * Tz * (D + K))
    f = g(Z) * y * (Mi(4:5) + (Sum(Nb*Mi(1:3))/Sum(Nb))*K/D) + & 
      & bigQi(4:5) * (Z - bigUi(4:5))**2 * Exp(-bigWi(4:5)*(Z - bigUi(4:5))**3)
    f(2) = f(2) + y(2) * alphaHe * R_star * dT_dZ(Z,b+1)
End Function nAr_He_integrand4

Function nAr_He_integrand5(Z,b) Result(f)  !for 115 to 1000 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: y

    y = 1._dp / (R_star * T(Z,b+1))
    f = g(Z) * y * Mi(4:5) + & 
      & bigQi(4:5) * (Z - bigUi(4:5))**2 * Exp(-bigWi(4:5)*(Z - bigUi(4:5))**3)
    f(2) = f(2) + y * alphaHe * R_star * dT_dZ(Z,b+1)
End Function nAr_He_integrand5

Subroutine N_densities(Z,Tz,b,N)
    !returns number density of each atmospheric constituent above 86km geometric altitude
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: Z
    Real(dp), Intent(In) :: Tz
    Integer, Intent(In) :: b
    Real(dp), Intent(Out) :: N(1:5)!(1:6)
    Real(dp) :: x(1:5)
    !UNDONE Extend N_density (and other functionality in this module) to compute N for H1
    
    !N2 power
    x(1) = nN2_power(Z,b)
    !O1 & O2 powers
    x(2:3) = nO1_O2_powers(Z,b)
    !Ar & He powers
    x(4:5) = nAr_He_powers(Z,b)
    !compute number densities of each species
    N(1:5) = N7(1:5) * Tb(7) * Exp(-x) / Tz
    !N(6) = nH(Z)
End Subroutine N_densities

Subroutine N_density(Z,Tz,b,N)
    !returns total number density of atmosphere above 86km geometric altitude
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: Z
    Real(dp), Intent(In) :: Tz
    Integer, Intent(In) :: b
    Real(dp), Intent(Out) :: N
    Real(dp) :: Ns(1:5)!(1:6)
    
    Call N_densities(Z,Tz,b,Ns)
    N = Sum(Ns)
End Subroutine N_density

Function Romberg_Quad_nN2(a,b,p) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: a,b    !limits of integration
    Integer, Intent(In) :: p
    Integer, Parameter :: Tmax = 20  !maximum number of extrapolations in the table
    Real(dp) :: Ti(0:Tmax)  !Extrapolation table previous row
    Real(dp) :: Tk0,Tk  !Extrapolation table current row values
    Integer :: i,j,k  !counters: i for table row, j for quadrature ordinates, k for table column
    Integer :: n      !number of intervals
    Real(dp) :: h0,h  !spacing between quadrature ordinates
    Real(dp) :: fk    !multiplier for extrapolation steps
    Real(dp) :: s     !sum of function values at quadrature ordinates
    Real(dp) :: aj    !the j-th ordinate

    !Initial trapezoid estimate: T0(0)
    n = 1
    s = 0.5_dp * (g(a) / T(a,p+1) + g(b) / T(b,p+1))
    h0 = b - a
    Ti(0) = h0 * s
    Do i = 1,Tmax !up to Tmax rows in the table
        !Trapezoid estimate i-th row of table: Ti(0)
        n = n * 2
        h = h0 / Real(n,dp)
        Do j = 1,n-1,2  !Odd values of j are NEW points at which to evaluate f
            aj = a + Real(j,dp)*h
            s = s + g(aj) / T(aj,p+1)
        End Do
        Tk0 = h * s
        !Fill i-th row, columns k = 1:i, with extrapolated estimates
        fk = 1._dp
        Do k = 1,i
            fk = fk * 4._dp
            Tk = (fk * Tk0 - Ti(k-1)) / (fk - 1._dp)
            If (k .EQ. i) Then
                Exit !skip storage steps if working final column
            Else
                Ti(k-1) = Tk0  !store Tk0 for next i
                Tk0 = Tk  !store Tk for next k
            End If
        End Do
        !Check for convergence
        If ( Abs(Ti(i-1) - Tk) .LE. rTol_tier1 * Abs(Tk) ) Then
            q = Tk
            Return  !Normal exit
        Else !store Tk0 and Tk for next i
            Ti(i-1) = Tk0
            Ti(i) = Tk
        End If
    End Do
    !If we get this far, we did not converge
    Write(*,'(A,I0,A)')        'ERROR:  US_Std_Atm_1976: Romberg_Quad_nN2:  Failed to converge in ',Tmax,' extrapolations.'
    Write(*,'(A,F0.16)')       '        Final estimated value: ',Tk
    Write(*,'(A,2(ES10.3,A))') '        Final Extrapolation Error: ',Abs(Tk-Ti(Tmax-1)),' (abs), ',Abs(Tk-Ti(Tmax-1))/Tk,' (rel)'
    Write(*,'(A,2(ES10.3,A))') '        Convergence Criteria:      ',0._dp,             ' (abs), ',rTol_tier1,           ' (rel)'
    ERROR STOP
End Function Romberg_Quad_nN2

Function Romberg_Quad_nO1_O2(f,a,b,p) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Interface
        Function f(x,k)    !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: f(1:2)
            Real(dp), Intent(In) :: x
            Integer, Intent(In) :: k
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Integer, Intent(In) :: p
    Integer, Parameter :: Tmax = 16  !maximum number of extrapolations in the table
    Real(dp) :: Ti(1:2,0:Tmax)  !Extrapolation table previous row
    Real(dp) :: Tk0(1:2),Tk(1:2)  !Extrapolation table current row values
    Integer :: i,j,k  !counters: i for table row, j for quadrature ordinates, k for table column
    Integer :: n      !number of intervals
    Real(dp) :: h0,h  !spacing between quadrature ordinates
    Real(dp) :: fk    !multiplier for extrapolation steps
    Real(dp) :: s(1:2)     !sum of function values at quadrature ordinates

    !Initial trapezoid estimate: T0(0)
    n = 1
    s = 0.5_dp * (f(a,p) + f(b,p))
    h0 = b - a
    Ti(:,0) = h0 * s
    Do i = 1,Tmax !up to Tmax rows in the table
        !Trapezoid estimate i-th row of table: Ti(0)
        n = n * 2
        h = h0 / Real(n,dp)
        Do j = 1,n-1,2  !Odd values of j are NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h,p)
        End Do
        Tk0 = h * s
        !Fill i-th row, columns k = 1:i, with extrapolated estimates
        fk = 1._dp
        Do k = 1,i
            fk = fk * 4._dp
            Tk = (fk * Tk0 - Ti(:,k-1)) / (fk - 1._dp)
            If (k .EQ. i) Then
                Exit !skip storage steps if working final column
            Else
                Ti(:,k-1) = Tk0  !store Tk0 for next i
                Tk0 = Tk  !store Tk for next k
            End If
        End Do
        !Check for convergence
        If ( Any(Abs(Ti(:,i-1) - Tk) .LE. rTol_tier2 * Abs(Tk)) ) Then
            q = Tk
            Return  !Normal exit
        Else !store Tk0 and Tk for next i
            Ti(:,i-1) = Tk0
            Ti(:,i) = Tk
        End If
    End Do
    !If we get this far, we did not converge
    Print*,p
    Print*,a,b
    Print*,Ti
    Write(*,'(A,I0,A)')      'ERROR:  US_Std_Atm_1976: Romberg_Quad_nO1_O2:  Failed to converge in ',Tmax,' extrapolations.'
    Write(*,'(A,F0.16)')     '          Final estimated value: ',Tk(1)
    Write(*,'(A,F0.16)')     '                                 ',Tk(2)
    Write(*,'(2(A,ES10.3))') '          Final Extrapolation Error: ',Abs(Tk(1)-Ti(1,Tmax-1)),' (abs), ',Abs(Tk(1)-Ti(1,Tmax-1))/Tk(1),' (rel)'
    Write(*,'(2(A,ES10.3))') '                                     ',Abs(Tk(2)-Ti(2,Tmax-1)),' (abs), ',Abs(Tk(2)-Ti(2,Tmax-1))/Tk(2),' (rel)'
    Write(*,'(2(A,ES10.3))') '          Convergence Criteria:      ',0._dp,               ' (abs), ',rTol_tier2,                ' (rel)'
    ERROR STOP
End Function Romberg_Quad_nO1_O2

Function Romberg_Quad_nAr_He(f,a,b,p) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Interface
        Function f(x,k)    !the function to be integrated
            Use Kinds,Only: dp
            Implicit None
            Real(dp) :: f(1:2)
            Real(dp), Intent(In) :: x
            Integer, Intent(In) :: k
        End Function f
    End Interface
    Real(dp), Intent(In) :: a,b    !limits of integration
    Integer, Intent(In) :: p
    Integer, Parameter :: Tmax = 16  !maximum number of extrapolations in the table
    Real(dp) :: Ti(1:2,0:Tmax)  !Extrapolation table previous row
    Real(dp) :: Tk0(1:2),Tk(1:2)  !Extrapolation table current row values
    Integer :: i,j,k  !counters: i for table row, j for quadrature ordinates, k for table column
    Integer :: n      !number of intervals
    Real(dp) :: h0,h  !spacing between quadrature ordinates
    Real(dp) :: fk    !multiplier for extrapolation steps
    Real(dp) :: s(1:2)     !sum of function values at quadrature ordinates

    !Initial trapezoid estimate: T0(0)
    n = 1
    s = 0.5_dp * (f(a,p) + f(b,p))
    h0 = b - a
    Ti(:,0) = h0 * s
    Do i = 1,Tmax !up to Tmax rows in the table
        !Trapezoid estimate i-th row of table: Ti(0)
        n = n * 2
        h = h0 / Real(n,dp)
        Do j = 1,n-1,2  !Odd values of j are NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h,p)
        End Do
        Tk0 = h * s
        !Fill i-th row, columns k = 1:i, with extrapolated estimates
        fk = 1._dp
        Do k = 1,i
            fk = fk * 4._dp
            Tk = (fk * Tk0 - Ti(:,k-1)) / (fk - 1._dp)
            If (k .EQ. i) Then
                Exit !skip storage steps if working final column
            Else
                Ti(:,k-1) = Tk0  !store Tk0 for next i
                Tk0 = Tk  !store Tk for next k
            End If
        End Do
        !Check for convergence
        If ( Any(Abs(Ti(:,i-1) - Tk) .LE. rTol_tier3 * Abs(Tk)) ) Then
            q = Tk
            Return  !Normal exit
        Else !store Tk0 and Tk for next i
            Ti(:,i-1) = Tk0
            Ti(:,i) = Tk
        End If
    End Do
    !If we get this far, we did not converge
    Write(*,'(A,I0,A)')      'ERROR:  US_Std_Atm_1976: Romberg_Quad_nAr_He:  Failed to converge in ',Tmax,' extrapolations.'
    Write(*,'(A,F0.16)')     '        Final estimated value: ',Tk(1)
    Write(*,'(A,F0.16)')     '                               ',Tk(2)
    Write(*,'(2(A,ES10.3))') '        Final Extrapolation Error: ',Abs(Tk(1)-Ti(1,Tmax-1)),' (abs), ',Abs(Tk(1)-Ti(1,Tmax-1))/Tk(1),' (rel)'
    Write(*,'(2(A,ES10.3))') '                                   ',Abs(Tk(2)-Ti(2,Tmax-1)),' (abs), ',Abs(Tk(2)-Ti(2,Tmax-1))/Tk(2),' (rel)'
    Write(*,'(2(A,ES10.3))') '        Convergence Criteria:      ',0._dp,               ' (abs), ',rTol_tier3,                ' (rel)'
    ERROR STOP
End Function Romberg_Quad_nAr_He

Function nH(Z) Result(N)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: N
    Real(dp), Intent(In) :: Z
    
    If (Z .LT. 150._dp) Then
        N = 0._dp
    Else If (Z .LT. 500._dp) Then
        N = (nH500 - phiH * Romberg_Quad_nH(500._dp,Z)) / p6(Z)  !US Standard Atmosphere 1976 equation 39
    Else If (Z .GT. 500._dp) Then
        N = nH500 / p6(Z)  !US Standard Atmosphere 1976 equation 39
    Else
        N = nH500
    End If
End Function nH

Function p6(Z) Result(p)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: Z
    
    !p = (T(Z,11) / T500)**(1._dp + alphaiH1) * Exp(Romberg_Quad_p6(500._dp,Z))
    p = Sqrt(Sqrt((T(Z,11) / T500)**3)) * Exp(Romberg_Quad_p6(500._dp,Z))
End Function p6

Function nH_integrand(Z) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: Z
    Real(dp) :: Tz
    Real(dp) :: D
    
    Tz = T(Z,11)
    D = ai(6) * Sqrt(Tz / 273.15_dp) / & 
      & ((      N7(1)   * Tb(7) * Exp(-nN2_power(Z,10)) + & 
      &     Sum(N7(2:3) * Tb(7) * Exp(-nO1_O2_powers(Z,10))) + & 
      &     Sum(N7(4:5) * Tb(7) * Exp(-nAr_He_powers(Z,10)))     ) / Tz)
    f = p6(Z) / D
End Function nH_integrand

Function Romberg_Quad_nH(a,b) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: a,b    !limits of integration
    Integer, Parameter :: Tmax = 16
    Real(dp) :: T0(0:Tmax)  !Extrapolation table, previous row
    Real(dp) :: Ti(0:Tmax)  !Extrapolation table, current row
    Integer :: i,j,k  !counters: i for table row, j for quadrature ordinates, k for table column
    Integer :: n      !number of intervals
    Real(dp) :: fk    !multiplier for extrapolation steps
    Real(dp) :: h     !spacing between quadrature ordinates
    Real(dp) :: s     !sum of function values at quadrature ordinates

    !Initial trapezoid estimate: T0(0)
    n = 1
    s = 0.5_dp * (nH_integrand(a) + nH_integrand(b))
    T0(0) = (b - a) * s
    Do i = 1,Tmax !up to Tmax rows in the table
        !Trapezoid estimate i-th row of table: Ti(0)
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !Odd values of j are NEW points at which to evaluate f
            s = s + nH_integrand(a + Real(j,dp)*h)
        End Do
        Ti(0) = h * s
        !Fill i-th row with extrapolated estimates
        fk = 1._dp
        Do k = 1,i
            fk = fk * 4._dp
            Ti(k) = (fk * Ti(k-1) - T0(k-1)) / (fk - 1._dp)
        End Do
        !Check for convergence compared to the final extrapolated value in the previous table row
        If ( Abs(T0(i-1) - Ti(i)) .LE. rTol_tier4b * Abs(Ti(i)) ) Then
            q = Ti(i) !Ti(i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
        !switch the current row to the previous row
        T0 = Ti  !i-th row becomes new previous row
    End Do
    !If we get this far, we did not converge
    Print *,"ERROR:  US_Std_Atm_1976: Romberg_Quad_nH:  Failed to converge in 20 extrapolations."
    ERROR STOP
End Function Romberg_Quad_nH

Function p6_integrand(Z) Result(p)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: Z
    Real(dp) :: Nb(1:5)
    
    Nb(1) =   N7(1)   * Tb(7) * Exp(-nN2_power(Z,10))
    Nb(2:3) = N7(2:3) * Tb(7) * Exp(-nO1_O2_powers(Z,10))
    Nb(4:5) = N7(4:5) * Tb(7) * Exp(-nAr_He_powers(Z,10))
    p = (Sum(Nb*Mi(1:5))/Sum(Nb)) * g(Z) / (R_star * T(Z,11))  !US Standard Atmosphere 1976 equation 40
End Function p6_integrand

Function Romberg_Quad_p6(a,b) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: a,b    !limits of integration
    Integer, Parameter :: Tmax = 16
    Real(dp) :: T0(0:Tmax)  !Extrapolation table, previous row
    Real(dp) :: Ti(0:Tmax)  !Extrapolation table, current row
    Integer :: i,j,k  !counters: i for table row, j for quadrature ordinates, k for table column
    Integer :: n      !number of intervals
    Real(dp) :: fk    !multiplier for extrapolation steps
    Real(dp) :: h     !spacing between quadrature ordinates
    Real(dp) :: s     !sum of function values at quadrature ordinates

    !Initial trapezoid estimate: T0(0)
    n = 1
    s = 0.5_dp * (p6_integrand(a) + p6_integrand(b))
    T0(0) = (b - a) * s
    Do i = 1,Tmax !up to Tmax rows in the table
        !Trapezoid estimate i-th row of table: Ti(0)
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !Odd values of j are NEW points at which to evaluate f
            s = s + p6_integrand(a + Real(j,dp)*h)
        End Do
        Ti(0) = h * s
        !Fill i-th row with extrapolated estimates
        fk = 1._dp
        Do k = 1,i
            fk = fk * 4._dp
            Ti(k) = (fk * Ti(k-1) - T0(k-1)) / (fk - 1._dp)
        End Do
        !Check for convergence compared to the final extrapolated value in the previous table row
        If ( Abs(T0(i-1) - Ti(i)) .LE. rTol_tier4a * Abs(Ti(i)) ) Then
            q = Ti(i) !Ti(i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
        !switch the current row to the previous row
        T0 = Ti  !i-th row becomes new previous row
    End Do
    !If we get this far, we did not converge
    Print *,"ERROR:  US_Std_Atm_1976: Romberg_Quad_p6:  Failed to converge in 20 extrapolations."
    ERROR STOP
End Function Romberg_Quad_p6

Function P(Z,layer,layer_range)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: P  ![Pa]
    Real(dp), Intent(In) :: Z ![km]
    Integer, Intent(In), Optional :: layer
    Integer, Intent(In), Optional :: layer_range(1:3)
    Real(dp) :: Tz,N
    Integer :: b
    
    !find atmospheric base layer
    If (Present(layer)) Then
        b = layer - 1
    Else If (Present(layer_range)) Then
        b = Find_Base_Layer(Z,layer_range)
    Else
        b = Find_Base_Layer(Z)
    End If
    If (Lb_nonzero(b)) Then
        If (T_linear_by_H(b)) Then
            Tz = Tb_minus_LbHb(b) + Lb(b) * Z_to_H(Z)  !US Standard Atmosphere 1976 equation 23
            !NOTE: Tz is molecular temperature for altitudes below 86km
        Else
            Tz = Tb(b) + Lb(b) * (Z - Zb(b))  !US Standard Atmosphere 1976 equation 29
        End If
        If (P_rho_not_by_N(b)) Then
            P = Pb_Tb_L_star_Lb(b) * Tz**(-L_star_Lb(b))  !US Standard Atmosphere 1976 equation 33a
        Else
            Call rho_N(Z,Tz,b,N)
            P = N * Tz * N_star  !US Standard Atmosphere 1976 equation 33c
        End If
    Else If (T_exponential(b)) Then
        Tz = T_inf - (T_inf - Tb(b)) * Exp(-lambda * (Z - Zb(b)) * R_Z10 / (R_Earth + Z))  !US Standard Atmosphere 1976 equation 31
        Call rho_N(Z,Tz,b,N)
        P = N * Tz * N_star  !US Standard Atmosphere 1976 equation 33c
    Else If (T_elliptical(b)) Then
        Tz = Tc + big_A * Sqrt(1._dp - ((Z - Zb(b)) / little_A)**2)  !US Standard Atmosphere 1976 equation 27
        Call rho_N(Z,Tz,b,N)
        P = N * Tz * N_star  !US Standard Atmosphere 1976 equation 33c
    Else !zero lapse rate
        Tz = Tb(b)
        If (P_rho_not_by_N(b)) Then
            P = Pb(b) * Exp( L_star_Tb(b) * (Z_to_H(Z) - Hb(b)) )  !US Standard Atmosphere 1976 equation 33b
        Else
            Call rho_N(Z,Tz,b,N)
            P = N * Tb(b) * N_star  !US Standard Atmosphere 1976 equation 33c
        End If
    End If
End Function P

Function rho(Z,layer,layer_range)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: rho  ![g/m^3]
    Real(dp), Intent(In) :: Z ![km]
    Integer, Intent(In), Optional :: layer
    Integer, Intent(In), Optional :: layer_range(1:3)
    Real(dp) :: Tz,Pz,N(1:5)!(1:6)
    Integer :: b
    Real(dp), Parameter :: kg2g = 1000._dp  !conversion for kg to g
    
    !find atmospheric base layer
    If (Present(layer)) Then
        b = layer - 1
    Else If (Present(layer_range)) Then
        b = Find_Base_Layer(Z,layer_range)
    Else
        b = Find_Base_Layer(Z)
    End If
    If (Lb_nonzero(b)) Then
        If (T_linear_by_H(b)) Then
            Tz = Tb_minus_LbHb(b) + Lb(b) * Z_to_H(Z)  !US Standard Atmosphere 1976 equation 23
            !NOTE: Tz is molecular temperature for altitudes below 86km
        Else
            Tz = Tb(b) + Lb(b) * (Z - Zb(b))  !US Standard Atmosphere 1976 equation 29
        End If
        If (P_rho_not_by_N(b)) Then
            Pz = Pb_Tb_L_star_Lb(b) * Tz**(-L_star_Lb(b))  !US Standard Atmosphere 1976 equation 33a
            rho = Pz * rho_star /  Tz  !US Standard Atmosphere 1976 equation 42-1
        Else
            Call rho_N(Z,Tz,b,N)
            rho = Sum(N * Mi(1:5)) * inv_Na * kg2g  !US Standard Atmosphere 1976 equation 42-3
        End If
    Else If (T_exponential(b)) Then
        Tz = T_inf - (T_inf - Tb(b)) * Exp(-lambda * (Z - Zb(b)) * R_Z10 / (R_Earth + Z))  !US Standard Atmosphere 1976 equation 31
        Call rho_N(Z,Tz,b,N)
        rho = Sum(N * Mi(1:5)) * inv_Na * kg2g  !US Standard Atmosphere 1976 equation 42-3
    Else If (T_elliptical(b)) Then
        Tz = Tc + big_A * Sqrt(1._dp - ((Z - Zb(b)) / little_A)**2)  !US Standard Atmosphere 1976 equation 27
        Call rho_N(Z,Tz,b,N)
        rho = Sum(N * Mi(1:5)) * inv_Na * kg2g  !US Standard Atmosphere 1976 equation 42-3
    Else !zero lapse rate
        Tz = Tb(b)
        If (P_rho_not_by_N(b)) Then
            Pz = Pb(b) * Exp( L_star_Tb(b) * (Z_to_H(Z) - Hb(b)) )  !US Standard Atmosphere 1976 equation 33b
            rho = Pz * rho_star /  Tz  !US Standard Atmosphere 1976 equation 42-1
        Else
            Call rho_N(Z,Tz,b,N)
            rho = Sum(N * Mi(1:5)) * inv_Na * kg2g  !US Standard Atmosphere 1976 equation 42-3
        End If
    End If
End Function rho

Elemental Function Z_to_H(Z) Result(H)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: H
    Real(dp), Intent(In) :: Z
    
    H = Z * R_Earth / (Z + R_Earth)
End Function Z_to_H

Elemental Function H_to_Z(H) Result(Z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Z
    Real(dp), Intent(In) :: H
    
    Z = H * R_Earth / (R_Earth - H)
End Function H_to_Z

Function nH_low(Z) Result(nH)
    !Returns number density of hydrogen due to water vapor as a function of altitude
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nH
    Real(dp), Intent(In) :: Z
    Real(dp), Parameter :: H2O_star = 2._dp * Na * rho_star / 18.01528_dp
    
    nH = H2O_star * 4686.E-6_dp  !Temporary value, midlat mean at the surface
End Function nH_low


# if INTEGRAND_STOPS
!---------------------------------------------------------------------------------
!  The following routines are used only for computing the 'stop' values for the 
!  number density integrals:  Used to compute necessary constants which are then 
!  hard-coded in the source.  They are included in compilation by defining 
!  'INTEGRAND_STOPS' as a conditional compiler directive for the preprocessor
!---------------------------------------------------------------------------------
Function nN2_power_stops() Result(xb)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: xb(1:9,1:2)
    Real(dp), Parameter :: Zs(1:9) = (/  86._dp, &
                                      &  91._dp, & 
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 120._dp, & 
                                      & 185._dp, & 
                                      & 250._dp, & 
                                      & 500._dp, & 
                                      & 1000._dp /)
    Real(dp), Parameter :: M_over_R(1:9) = (/ rho_star, &
                                            & rho_star, &
                                            & Mi(1) / R_star, &
                                            & Mi(1) / R_star, &
                                            & Mi(1) / R_star, &
                                            & Mi(1) / R_star, &
                                            & Mi(1) / R_star, &
                                            & Mi(1) / R_star, &
                                            & Mi(1) / R_star  /)
    Integer, Parameter :: bs(1:9) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     & 10, & 
                                     & 10, & 
                                     & 10, & 
                                     & 10, & 
                                     & 10  /)
    Integer :: i

    xb(:,1) = Zs
    xb(:,2) = 0._dp
    Do i = 2,9
        xb(i,2) = xb(i-1,2) + M_over_R(i-1) * Romberg_Quad_nN2(Zs(i-1),Zs(i),bs(i-1))
    End Do
End Function nN2_power_stops

Function nO1_O2_power_stops() Result(xb)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: xb(1:12,1:3)
    Real(dp), Parameter :: Zs(1:12) = (/  86._dp, &
                                      &  91._dp, & 
                                      &  95._dp, & 
                                      &  97._dp, &
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 115._dp, & 
                                      & 120._dp, & 
                                      & 185._dp, & 
                                      & 250._dp, & 
                                      & 500._dp, & 
                                      & 1000._dp /)
    Integer, Parameter :: bs(1:12) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     &  9, & 
                                     & 10, & 
                                     & 10, & 
                                     & 10, & 
                                     & 10, & 
                                     & 10  /)

    xb(:,1) = Zs
    xb(:,2:3) = 0._dp
    xb(2,2:3) = xb(1,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand1,Zs(1),Zs(2),bs(1)) !up to 91km
    xb(3,2:3) = xb(2,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand1,Zs(2),Zs(3),bs(2)) !up to 95km
    xb(4,2:3) = xb(3,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand2,Zs(3),Zs(4),bs(3)) !up to 97km
    xb(5,2:3) = xb(4,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand3,Zs(4),Zs(5),bs(4)) !up to 100km
    xb(6,2:3) = xb(5,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand4,Zs(5),Zs(6),bs(5)) !up to 110km
    xb(7,2:3) = xb(6,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand4,Zs(6),Zs(7),bs(6)) !up to 115km
    xb(8,2:3) = xb(7,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand5,Zs(7),Zs(8),bs(7)) !up to 120km
    xb(9,2:3) = xb(8,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand5,Zs(8),Zs(9),bs(8)) !up to 185km
    xb(10,2:3) = xb(9,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand5,Zs(9),Zs(10),bs(9)) !up to 250km
    xb(11,2:3) = xb(10,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand5,Zs(10),Zs(11),bs(10)) !up to 500km
    xb(12,2:3) = xb(11,2:3) + Romberg_Quad_nO1_O2(nO1_O2_integrand5,Zs(11),Zs(12),bs(11)) !up to 1000km
End Function nO1_O2_power_stops

Function nAr_He_power_stops() Result(xb)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: xb(1:12,1:3)
    Real(dp), Parameter :: Zs(1:12) = (/  86._dp, &
                                      &  91._dp, & 
                                      &  95._dp, & 
                                      &  97._dp, &
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 115._dp, & 
                                      & 120._dp, & 
                                      & 185._dp, & 
                                      & 250._dp, & 
                                      & 500._dp, & 
                                      & 1000._dp /)
    Integer, Parameter :: bs(1:12) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     &  9, & 
                                     & 10, & 
                                     & 10, & 
                                     & 10, & 
                                     & 10, & 
                                     & 10  /)

    xb(:,1) = Zs
    xb(:,2:3) = 0._dp
    xb(2,2:3) = xb(1,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand1,Zs(1),Zs(2),bs(1)) !up to 91km
    xb(3,2:3) = xb(2,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand1,Zs(2),Zs(3),bs(2)) !up to 95km
    xb(4,2:3) = xb(3,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand2,Zs(3),Zs(4),bs(3)) !up to 97km
    xb(5,2:3) = xb(4,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand2,Zs(4),Zs(5),bs(4)) !up to 100km
    xb(6,2:3) = xb(5,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand4,Zs(5),Zs(6),bs(5)) !up to 110km
    xb(7,2:3) = xb(6,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand4,Zs(6),Zs(7),bs(6)) !up to 115km
    xb(8,2:3) = xb(7,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand5,Zs(7),Zs(8),bs(7)) !up to 120km
    xb(9,2:3) = xb(8,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand5,Zs(8),Zs(9),bs(8)) !up to 185km
    xb(10,2:3) = xb(9,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand5,Zs(9),Zs(10),bs(9)) !up to 250km
    xb(11,2:3) = xb(10,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand5,Zs(10),Zs(11),bs(10)) !up to 500km
    xb(12,2:3) = xb(11,2:3) + Romberg_Quad_nAr_He(nAr_He_integrand5,Zs(11),Zs(12),bs(11)) !up to 1000km
End Function nAr_He_power_stops
# endif

# if GL_POINTS
!---------------------------------------------------------------------------------
!  The following routines are used only for computing the required number of 
!  Gauss-Legendre quadrature points to quickly evaluate the number density 
!  integrals:  Used to compute necessary constants which are then  hard-coded in 
!  the source.  They are included in compilation by defining 'GL_POINTS' as a 
!  conditional compiler directive for the preprocessor
!---------------------------------------------------------------------------------
Subroutine nN2_GLpoints(zbs,nb,xb)
    Use Kinds, Only: dp
    Use Utilities, Only: Prec
    Use Quadratures, Only: Romberg_Quad
    Use Quadratures, Only: GaussLegendreN,GaussLegendre96
    Use Quadratures, Only: Progressive_GaussLegendre
    Implicit None
    Real(dp), Intent(Out) :: zbs(1:9)
    Integer, Intent(Out) :: nb(1:9,1:3)
    Real(dp), Intent(Out) :: xb(1:9,1:3)
    Real(dp), Parameter :: Zs(1:9) = (/  86._dp, &
                                      &  91._dp, & 
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 120._dp, &
                                      & 185._dp, &
                                      & 250._dp, &
                                      & 500._dp, &
                                      & 1000._dp  /)
    Integer, Parameter :: bs(1:9) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     & 10, &
                                     & 10, &
                                     & 10, &
                                     & 10, &
                                     & 10  /)
    Integer :: i,j
    Real(dp), Parameter :: rTol = 1.E-15_dp

    zbs = Zs
    nb = 0
    xb = 0._dp
    Do i = 2,9
        xb(i,1) = Romberg_Quad(nN2_integrand_1,Zs(i-1),Zs(i),0._dp,rTol,n_ord=nb(i,1))
        xb(i,2) = Progressive_GaussLegendre(nN2_integrand_1,Zs(i-1),Zs(i),rTol,0._dp,n_done=nb(i,2))
        Do j = 3,100
            xb(i,3) = GaussLegendreN(j,nN2_integrand_1,Zs(i-1),Zs(i))
            If (Floor(Prec(xb(i,3),xb(i,1))) .GE. NINT(-Log10(rTol))) Then
                If (Floor(Prec(GaussLegendreN(j+1,nN2_integrand_1,Zs(i-1),Zs(i)),xb(i,1))).GE.Floor(Prec(xb(i,3),xb(i,1)))) Then
                    nb(i,3) = j
                    Exit
                End If
            End If
        End Do
    End Do
Contains
    Function nN2_integrand_1(z) Result(y)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: y
        Real(dp), Intent(In) :: z
        
        y = g(z) / T(z)
    End Function nN2_integrand_1
End Subroutine nN2_GLpoints

Subroutine nO1_O2_GLpoints(zbs,nb,xb)
    Use Kinds, Only: dp
    Use Utilities, Only: Prec
    Use Quadratures, Only: Romberg_Quad
    Use Quadratures, Only: GaussLegendreN
    Use Quadratures, Only: Progressive_GaussLegendre
    Implicit None
    Real(dp), Intent(Out) :: zbs(1:14)
    Integer, Intent(Out) :: nb(1:14,1:3,1:2)
    Real(dp), Intent(Out) :: xb(1:14,1:3,1:2)
    Real(dp), Parameter :: Zs(1:14) = (/  86._dp, &
                                      &  91._dp, & 
                                      &  95._dp, & 
                                      &  97._dp, &
                                      & 100._dp, & 
                                      & 105._dp, & 
                                      & 110._dp, & 
                                      & 114._dp, & 
                                      & 115._dp, & 
                                      & 120._dp, &
                                      & 185._dp, &
                                      & 250._dp, &
                                      & 500._dp, &
                                      & 1000._dp  /)
    Integer, Parameter :: bs(1:14) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     &  9, & 
                                     &  9, & 
                                     & 10, &
                                     & 10, &
                                     & 10, &
                                     & 10, &
                                     & 10  /)
    Integer :: i,j,b
    Real(dp), Parameter :: rTol = 1.E-14_dp

    zbs = Zs
    nb = 0
    xb = 0._dp
    i = 1  !up to 91km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand1_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    Print*,nN2_power(91._dp,7)
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand1_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand1_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand1_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 2  !up to 95km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand1_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand1_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand1_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand1_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 3  !up to 97km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand2_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand2_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand2_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand2_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand2_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand2_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand2_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand2_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 4  !up to 100km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand3_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand3_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand3_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand3_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand3_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand3_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 5  !up to 105km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand3_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand3_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand3_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand3_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand3_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand3_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 6  !up to 110km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand4_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand4_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand4_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand4_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 7  !up to 113km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand4_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand4_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand4_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand4_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 8  !up to 115km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand4_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand4_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand4_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand4_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 9  !up to 120km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand5_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand5_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand5_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand5_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand5_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand5_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand5_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand5_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do i = 10,13  !up to 1000km
        b = bs(i)
        xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand5_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
        xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand5_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
        xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand5_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
        xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand5_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
        Do j = 3,nb(i+1,2,1)
            xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand5_1,Zs(i),Zs(i+1))
            If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol))) Then
                If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand5_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                    nb(i+1,3,1) = j
                    Exit
                End If
            End If
        End Do
        Do j = 3,100
            xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand5_2,Zs(i),Zs(i+1))
            If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol))) Then
                If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand5_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                    nb(i+1,3,2) = j
                    Exit
                End If
            End If
        End Do
    End Do

Contains
    Function nO1_O2_integrand1_1(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand1_1
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand1(z,b)
        nO1_O2_integrand1_1 = x(1)
    End Function nO1_O2_integrand1_1
    Function nO1_O2_integrand1_2(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand1_2
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand1(z,b)
        nO1_O2_integrand1_2 = x(2)
    End Function nO1_O2_integrand1_2
    Function nO1_O2_integrand2_1(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand2_1
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand2(z,b)
        nO1_O2_integrand2_1 = x(1)
    End Function nO1_O2_integrand2_1
    Function nO1_O2_integrand2_2(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand2_2
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand2(z,b)
        nO1_O2_integrand2_2 = x(2)
    End Function nO1_O2_integrand2_2
    Function nO1_O2_integrand3_1(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand3_1
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand3(z,b)
        nO1_O2_integrand3_1 = x(1)
    End Function nO1_O2_integrand3_1
    Function nO1_O2_integrand3_2(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand3_2
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand3(z,b)
        nO1_O2_integrand3_2 = x(2)
    End Function nO1_O2_integrand3_2
    Function nO1_O2_integrand4_1(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand4_1
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand4(z,b)
        nO1_O2_integrand4_1 = x(1)
    End Function nO1_O2_integrand4_1
    Function nO1_O2_integrand4_2(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand4_2
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand4(z,b)
        nO1_O2_integrand4_2 = x(2)
    End Function nO1_O2_integrand4_2
    Function nO1_O2_integrand5_1(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand5_1
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand5(z,b)
        nO1_O2_integrand5_1 = x(1)
    End Function nO1_O2_integrand5_1
    Function nO1_O2_integrand5_2(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nO1_O2_integrand5_2
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nO1_O2_integrand5(z,b)
        nO1_O2_integrand5_2 = x(2)
    End Function nO1_O2_integrand5_2
End Subroutine nO1_O2_GLpoints

Subroutine nAr_He_GLpoints(zbs,nb,xb)
    Use Kinds, Only: dp
    Use Utilities, Only: Prec
    Use Quadratures, Only: Romberg_Quad
    Use Quadratures, Only: GaussLegendreN
    Use Quadratures, Only: Progressive_GaussLegendre
    Implicit None
    Real(dp), Intent(Out) :: zbs(1:9)
    Integer, Intent(Out) :: nb(1:9,1:3,1:2)
    Real(dp), Intent(Out) :: xb(1:9,1:3,1:2)
    Real(dp), Parameter :: Zs(1:9) = (/  86._dp, &
                                      &  91._dp, & 
                                      &  95._dp, & 
                                      &  97._dp, &
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 115._dp, & 
                                      & 120._dp, &
                                      & 1000._dp  /)
    Integer, Parameter :: bs(1:9) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     &  9, & 
                                     & 10, &
                                     & 10  /)
    Integer :: i,j,b

    zbs = Zs
    nb = 0
    xb = 0._dp
    i = 1  !up to 91km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand1_1,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand1_2,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand1_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand1_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 2  !up to 95km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand1_1,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand1_2,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand1_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand1_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 3  !up to 97km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand2_1,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand2_2,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand2_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand2_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand2_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand2_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 4  !up to 100km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand2_1,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand2_2,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand2_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand2_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand2_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand2_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 5  !up to 110km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand4_1,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand4_2,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand4_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand4_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 6  !up to 115km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand4_1,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand4_2,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand4_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand4_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 7  !up to 120km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand5_1,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand5_2,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand5_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand5_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand5_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand5_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand5_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand5_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    i = 8  !up to 1000km
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand5_1,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand5_2,Zs(i),Zs(i+1),0._dp,rTol_tier3,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand5_1,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand5_2,Zs(i),Zs(i+1),1.E-13_dp,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,nb(i+1,2,1)
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand5_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand5_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand5_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. NINT(-Log10(rTol_tier3))) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand5_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do

Contains
    Function nAr_He_integrand1_1(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nAr_He_integrand1_1
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nAr_He_integrand1(z,b)
        nAr_He_integrand1_1 = x(1)
    End Function nAr_He_integrand1_1
    Function nAr_He_integrand1_2(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nAr_He_integrand1_2
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nAr_He_integrand1(z,b)
        nAr_He_integrand1_2 = x(2)
    End Function nAr_He_integrand1_2
    Function nAr_He_integrand2_1(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nAr_He_integrand2_1
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nAr_He_integrand2(z,b)
        nAr_He_integrand2_1 = x(1)
    End Function nAr_He_integrand2_1
    Function nAr_He_integrand2_2(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nAr_He_integrand2_2
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nAr_He_integrand2(z,b)
        nAr_He_integrand2_2 = x(2)
    End Function nAr_He_integrand2_2
    Function nAr_He_integrand4_1(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nAr_He_integrand4_1
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nAr_He_integrand4(z,b)
        nAr_He_integrand4_1 = x(1)
    End Function nAr_He_integrand4_1
    Function nAr_He_integrand4_2(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nAr_He_integrand4_2
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nAr_He_integrand4(z,b)
        nAr_He_integrand4_2 = x(2)
    End Function nAr_He_integrand4_2
    Function nAr_He_integrand5_1(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nAr_He_integrand5_1
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nAr_He_integrand5(z,b)
        nAr_He_integrand5_1 = x(1)
    End Function nAr_He_integrand5_1
    Function nAr_He_integrand5_2(z)
        Use Kinds, Only: dp
        Implicit None
        Real(dp) :: nAr_He_integrand5_2
        Real(dp), Intent(In) :: z
        Real(dp) :: x(1:2)

        x = nAr_He_integrand5(z,b)
        nAr_He_integrand5_2 = x(2)
    End Function nAr_He_integrand5_2
End Subroutine nAr_He_GLpoints
# endif

End Module US_Std_Atm_1976

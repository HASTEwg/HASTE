Module US_Std_Atm_1976
    
    Use Kinds, Only: dp
    Implicit None    
    Private
    Public :: find_base_layer
    Public :: T
    Public :: P
    Public :: rho    
    Public :: Zb
#   if TEST
        Public :: nN2_power_stops
        Public :: nO1_O2_power_stops
        Public :: nAr_He_power_stops
#   endif

    !US Standard Atmosphere 1976 parameters
    !The following constants are defined here to ensure consistency with 1976 atmosphere model definition.
    !There may be more modern values for these constants, but these values ensure agreement with the 1976 US Std Atmosphere model
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
                                       & 216.65_dp, & 
                                       & 216.65_dp, & 
                                       & 228.65_dp, & 
                                       & 270.65_dp, & 
                                       & 270.65_dp, & 
                                       & 214.65_dp, &
                                       !& 186.9459083101885122_dp, & !<--value is for when M0 correction is NOT accounted for
                                       !& 186.9459083101885122_dp, & !<--value is for when M0 correction is NOT accounted for
                                       & 186.8671666936082608_dp, & !<--value is for when M0 correction is accounted for
                                       & 186.8671666936082608_dp, & !<--value is for when M0 correction is accounted for
                                       & 240._dp, &
                                       & 360._dp, &
                                       & 1000._dp /)
    Real(dp), Parameter :: Pb(0:7) = (/ 101325._dp, &   ![Pa] Computed pressure at layer boundaries
                                      & 22632.0336238972840275_dp, & 
                                      & 5474.87437675730708586_dp, & 
                                      & 868.014988510785148131_dp, & 
                                      & 110.905629143702212828_dp, & 
                                      & 66.9384346263881217465_dp, & 
                                      & 3.95638449983647254755_dp, &
                                      !& 0.37337628269333201966_dp  /) !<--value is for when M0 correction is NOT accounted for
                                      & 0.37069900314554960416_dp  /)  !<--value is for when M0 correction is accounted for
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
    Real(dp), Parameter :: R_Z10 = R_Earth + Zb(10)
    Real(dp), Parameter :: Na = 6.022169E26_dp  ![1/kmol] Avagadro's Number
    Real(dp), Parameter :: inv_Na = 1._dp / Na
    Real(dp), Parameter :: N_star = R_star / Na
    Real(dp), Parameter :: K0 = 1.2E2_dp
    Real(dp), Parameter :: Mi(1:5) = (/ 28.0134_dp, &  !N2
                                      & 15.9994_dp, &  !O1
                                      & 31.9988_dp, &  !O2
                                      & 39.948_dp,  &  !Ar
                                      &  4.0026_dp  /) !He  !US Standard Atmosphere 1976 table 3
    Real(dp), Parameter :: alphai(5:6) = (/ -0.40_dp, &  !He
                                          & -0.25_dp  /) !H  !US Standard Atmosphere 1976 table 6
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
    Real(dp), Parameter :: phiH = 7.2E11_dp
    !Precomuted parameters for Romberg Quadrature routines
    Real(dp), Parameter :: Romb1(1:10) = (/ 4._dp,    &
                                          & 4._dp**2, &
                                          & 4._dp**3, &
                                          & 4._dp**4, &
                                          & 4._dp**5, &
                                          & 4._dp**6, &
                                          & 4._dp**7, &
                                          & 4._dp**8, &
                                          & 4._dp**9, &
                                          & 4._dp**10 /)
    Real(dp), Parameter :: Romb2(1:10) = 1._dp / (Romb1 - 1._dp)
    !Convergence criteria for Romberg Quadrature routines
    Real(dp), Parameter :: rTol_tier1 = 1.E-9_dp  !N2
    Real(dp), Parameter :: rTol_tier2 = 1.E-8_dp  !O1 and O2
    Real(dp), Parameter :: rTol_tier3 = 1.E-7_dp  !Ar and He
    Real(dp), Parameter :: rTol_tier4a = 1.E-6_dp  !H
    Real(dp), Parameter :: rTol_tier4b = 1.E-5_dp  !H
    
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
    Real(dp) :: T  ![K]
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
    Else If (T_exponential(b)) Then
        dT_dZ = lambda * (T_inf - Tb(b)) * ((R_Earth + Zb(b)) / (R_Earth + Z))**2 * Exp(-lambda * (Z - Zb(b)) * R_Z10 / (R_Earth + Z))  !US Standard Atmosphere 1976 equation 32
    Else If (T_elliptical(b)) Then
        dT_dZ = -big_A * (Z - Zb(b)) / (little_A * Sqrt(1._dp - ((Z - Zb(b)) / little_A)**2))  !US Standard Atmosphere 1976 equation 28
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
                                            & 0.9995788_dp /)
    
    i = Ceiling(2._dp * (Z - 80._dp))
    c = Linear_Interp(Z,Zm_corr(i-1),Zm_corr(i),M0_corr(i-1),M0_corr(i))
End Function T_M0_correction

Function nN2_power(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: M_over_R
    Logical :: Z_below_100
    Real(dp), Parameter :: xb(7:10) = (/ 0._dp,                 &  !Z = 86km
                                       & 0.8891738712368936_dp, &  !Z = 91km
                                       & 3.9815997728018476_dp, &  !Z = 110km
                                       & 5.0588195691573031_dp  /) !Z = 120km
    Real(dp), Parameter :: xb_100 = 2.4639390409132486_dp  !Z = 100km
    Logical, Parameter :: no_sublayers(7:10) = (/ .TRUE.,  &
                                                & .FALSE., &
                                                & .TRUE.,  &
                                                & .TRUE.   /)
    Real(dp), Parameter :: rho_star_N2 = Mi(1) / R_star
    
    If (Z .LE. 100._dp) Then
        M_over_R = rho_star
        Z_below_100 = .TRUE.
    Else
        M_over_R = rho_star_N2
        Z_below_100 = .FALSE.
    End If
    If (no_sublayers(b)) Then !b=7, 9, or 10
        x = xb(b) + M_over_R * Romberg_Quad_nN2(Zb(b),Z,b)
    Else !b=8
        If (Z_below_100) Then
            x = xb(8) + M_over_R * Romberg_Quad_nN2(Zb(8),Z,8)
        Else
            x = xb_100 + M_over_R * Romberg_Quad_nN2(100._dp,Z,8)
        End If
    End If
End Function nN2_power

Function g(Z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: g
    Real(dp), Intent(In) :: Z
    
    g = g0 * (R_Earth / (R_Earth + Z))**2  !US Standard Atmosphere 1976 equation 17
End Function g

Function nO1_O2_powers(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Logical :: Z_below_97
    Real(dp), Parameter :: xb(1:2,7:10) = Reshape( (/  0._dp,                 &  !O1, Z = 86km
                                                    &  0._dp,                 &  !O2, Z = 86km
                                                    & -1.2335158785532025_dp, &  !O1, Z = 91km
                                                    &  0.8987089660301271_dp, &  !O2, Z = 91km
                                                    & -1.2350403922105528_dp, &  !O1, Z = 110km
                                                    &  4.5003526937771803_dp, &  !O2, Z = 110km
                                                    & -0.7312338738839638_dp, &  !O1, Z = 120km
                                                    &  5.8804681169361679_dp  /), &  !O2, Z = 120km
                                                    & (/2,4/) )
    Real(dp), Parameter :: xb_95(1:2) =  (/ -1.6326227572400966_dp, &  !O1, Z = 95km
                                          &  1.6401385731339726_dp  /) !O2, Z = 95km
    Real(dp), Parameter :: xb_97(1:2) =  (/ -1.6736396506936295_dp, &  !O1, Z = 97km
                                          &  2.0206764985042182_dp  /) !O2, Z = 97km
    Real(dp), Parameter :: xb_100(1:2) = (/ -1.6519505201748717_dp, &  !O1, Z = 100km
                                          &  2.6026369578525224_dp  /) !O2, Z = 100km
    Real(dp), Parameter :: xb_115(1:2) = (/ -0.9801497240119973_dp, &  !O1, Z = 115km
                                          &  5.2767050379951361_dp  /) !O2, Z = 115km
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
            x = Romberg_Quad_nO1_O2(nO1_O2_integrand1,Zb(7),Z,7)
        Else !b=10
            x = xb(:,10) + Romberg_Quad_nO1_O2(nO1_O2_integrand5,Zb(10),Z,10)
        End If
    Else
        If (Z_below_97) Then !b=8
            If (Z .LT. 95._dp) Then
                x = xb(:,8) + Romberg_Quad_nO1_O2(nO1_O2_integrand1,Zb(8),Z,8)
            Else
                x = xb_95 + Romberg_Quad_nO1_O2(nO1_O2_integrand2,95._dp,Z,8)
            End If
        Else !b=8 OR b=9
            If (Z .LT. 100._dp) Then !b=8
                x = xb_97 + Romberg_Quad_nO1_O2(nO1_O2_integrand2,97._dp,Z,8)
            Else If (b .EQ. 8) Then !b=8
                x = xb_100 + Romberg_Quad_nO1_O2(nO1_O2_integrand3,100._dp,Z,8)
            Else !b=9
                If (Z .LT. 115._dp) Then
                    x = xb(:,9) + Romberg_Quad_nO1_O2(nO1_O2_integrand4,Zb(9),Z,9)
                Else
                    x = xb_115 + Romberg_Quad_nO1_O2(nO1_O2_integrand5,115._dp,Z,9)
                End If
            End If
        End If
    End If
End Function nO1_O2_powers

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
    K = K0 * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
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
    K = 1.2E2_dp * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
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
    K = K0 * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
    f = g(Z) * D * (Mi(2:3) + Mi(1)*K/D) / (R_star * Tz * (D + K)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
End Function nO1_O2_integrand4

Function nO1_O2_integrand5(Z,b) Result(f)  !for 115 to 1000 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b

    f = g(Z) * Mi(2:3) / (R_star * T(Z,b+1)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
End Function nO1_O2_integrand5

Function nAr_He_powers(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp), Parameter :: xb(1:2,7:10) = Reshape( (/  0._dp, &  !Ar, Z = 86km
                                                    &  0._dp, &  !He, Z = 86km
                                                    &  0.9029433887519961_dp, &  !Ar, Z = 91km
                                                    &  0.7963213747812477_dp, &  !He, Z = 91km
                                                    &  4.6108383812528880_dp, &  !Ar, Z = 110km
                                                    &  2.3167238396434371_dp, &  !He, Z = 110km
                                                    &  6.2412684806214495_dp, &  !Ar, Z = 120km
                                                    &  2.3147895408503995_dp  /), &  !He, Z = 120km
                                                    & (/2,4/) )
    Real(dp), Parameter :: xb_95(1:2) =  (/ 1.6463776323299731_dp, &  !Ar, Z = 95km
                                          & 1.3378385484034475_dp  /) !He, Z = 95km
    Real(dp), Parameter :: xb_100(1:2) = (/ 2.6119420164281658_dp, &  !Ar, Z = 100km
                                          & 1.8579919647339275_dp  /) !He, Z = 100km
    Real(dp), Parameter :: xb_115(1:2) = (/ 5.5159366790272590_dp, &  !Ar, Z = 115km
                                          & 2.3185678009807535_dp  /) !He, Z = 115km
    Logical, Parameter :: no_sublayers(7:10) = (/ .TRUE.,  &
                                                & .FALSE., &
                                                & .FALSE., &
                                                & .TRUE.   /)
    
    If (no_sublayers(b)) Then
        If (b .EQ. 7) Then !b=7
            x = Romberg_Quad_nAr_He(nAr_He_integrand1,Zb(7),Z,7)
        Else !b=10
            x = xb(:,10) + Romberg_Quad_nAr_He(nAr_He_integrand5,Zb(10),Z,10)
        End If
    Else
        If (b .EQ. 8) Then !b=8
            If (Z .LT. 95._dp) Then
                x = xb(:,8) + Romberg_Quad_nAr_He(nAr_He_integrand1,Zb(8),Z,8)
            Else If (Z .LT. 100._dp) Then
                x = xb_95 + Romberg_Quad_nAr_He(nAr_He_integrand2,95._dp,Z,8)
            Else !Z > 100
                x = xb_100 + Romberg_Quad_nAr_He(nAr_He_integrand4,100._dp,Z,8)
            End If
        Else !b=9
            If (Z .LT. 115._dp) Then
                x = xb(:,9) + Romberg_Quad_nAr_He(nAr_He_integrand4,Zb(9),Z,9)
            Else
                x = xb_115 + Romberg_Quad_nAr_He(nAr_He_integrand4,115._dp,Z,9)
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
    f(2) = f(2) + y(2) * alphai(5) * dT_dZ(Z,b+1)
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
    K = K0 * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
    y = D / (R_star * Tz * (D + K))
    f = g(Z) * y * (Mi(4:5) + M0*K/D) + & 
      & bigQi(4:5) * (Z - bigUi(4:5))**2 * Exp(-bigWi(4:5)*(Z - bigUi(4:5))**3)
    f(2) = f(2) + y(2) * alphai(5) * dT_dZ(Z,b+1)
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
    K = K0 * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
    y = D / (R_star * Tz * (D + K))
    f = g(Z) * y * (Mi(4:5) + (Sum(Nb*Mi(1:3))/Sum(Nb))*K/D) + & 
      & bigQi(4:5) * (Z - bigUi(4:5))**2 * Exp(-bigWi(4:5)*(Z - bigUi(4:5))**3)
    f(2) = f(2) + y(2) * alphai(5) * dT_dZ(Z,b+1)
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
    f(2) = f(2) + y * alphai(5) * dT_dZ(Z,b+1)
End Function nAr_He_integrand5

Subroutine N_densities(Z,Tz,b,N)
    !returns number density of each atmospheric constituent above 86km geometric altitude
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: Z
    Real(dp), Intent(In) :: Tz
    Integer, Intent(In) :: b
    Real(dp), Intent(Out) :: N(1:5)
    Real(dp) :: x(1:5)
    !UNDONE Extend N_density (and other functionality in this module) to compute N for Ar, He, and H
    
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
    Real(dp) :: Ns(1:5)
    
    Call N_densities(Z,Tz,b,Ns)
    N = Sum(Ns)
End Subroutine N_density

Function Romberg_Quad_nN2(a,b,p) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: a,b    !limits of integration
    Integer, Intent(In) :: p
    Real(dp) :: R(0:10,0:10)  !Romberg table
    Integer :: n,i,j
    Real(dp) :: h,s,as

    n = 1
    h = b - a
    s = 0.5_dp * (g(a) / T(a,p+1) + g(b) / T(b,p+1))
    R(0,0) = h * s
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            as = a + Real(j,dp)*h
            s = s + g(as) / T(as,p+1)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        Do j = 1,i
            R(j,i) = Romb2(j) * (Romb1(j) * R(j-1,i) - R(j-1,i-1))
        End Do
        !check for convergence
        If ( Abs(R(i-1,i-1) - R(i,i)) .LE. rTol_tier1 * Abs(R(i,i)) ) Then
            q = R(i,i)  !R(i,i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
    End Do
    !If we get this far, we did not converge
    Call Continue_Romberg_nN2(a,b,p,s,10,R(:,10),2,q)
End Function Romberg_Quad_nN2
Recursive Subroutine Continue_Romberg_nN2(a,b,p,s,d,R0,level,q)  !adds 10 more rows to the previous Romberg_Quad table
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: a,b    !limits of integration
    Integer, Intent(In) :: p
    Real(dp), Intent(InOut) :: s  !previous sum of ordinates
    Integer, Intent(In) :: d  !length of final row in OLD Romberg Table
    Real(dp), Intent(In) :: R0(0:d)  !final row of OLD romberg table
    Integer, Intent(In) :: level
    Real(dp), Intent(Out) :: q    !the result of the integration, if convergence attained
    Real(dp) :: R(0:d+10,0:10)  !Romberg table extension
    Integer :: n,i,j
    Real(dp) :: h,as
    Integer :: fours
    
    R(0:d,0) = R0
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = 2**(d+i)
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            as = a + Real(j,dp)*h
            s = s + g(as) / T(as,p+1)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        fours = 1
        Do j = 1,d+i
            fours = fours * 4
            R(j,i) = (Real(fours,dp) * R(j-1,i) - R(j-1,i-1)) / Real(fours - 1,dp)
        End Do
        !check for convergence
        If ( Abs(R(i-1,i-1) - R(i,i)) .LE. rTol_tier1 * Abs(R(i,i)) ) Then
            q = R(d+i,i)
            Return  !Normal exit
        End If
    End Do
    If (level .GT. 10) Then !max allowed recursion depth, interval has been split 100 times...
        Print *,"ERROR:  US_Std_Atm_1976: Continue_Romberg_nN2:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !If we get this far, we did not converge, recurse to add 10 more rows
    Call Continue_Romberg_nN2(a,b,p,s,d+10,R(:,10),level+1,q)
End Subroutine Continue_Romberg_nN2

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
    Real(dp) :: R(1:2,0:10,0:10)  !Romberg table
    Integer :: n,i,j
    Real(dp) :: h,s(1:2)

    n = 1
    h = b - a
    s = 0.5_dp * (f(a,p) + f(b,p))
    R(:,0,0) = h * s
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h,p)
        End Do
        R(:,0,i) = h * s
        !fill out Romberg table row
        Do j = 1,i
            R(:,j,i) = Romb2(j) * (Romb1(j) * R(:,j-1,i) - R(:,j-1,i-1))
        End Do
        !check for convergence
        If ( All( Abs(R(:,i-1,i-1) - R(:,i,i)) .LE. rTol_tier2 * Abs(R(:,i,i)) ) ) Then
            q = R(:,i,i)  !R(i,i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
    End Do
    !If we get this far, we did not converge
    Call Continue_Romberg_nO1_O2(f,a,b,p,s,10,R(:,:,10),2,q)
End Function Romberg_Quad_nO1_O2
Recursive Subroutine Continue_Romberg_nO1_O2(f,a,b,p,s,d,R0,level,q)  !adds 10 more rows to the previous Romberg_Quad table
    Use Kinds, Only: dp
    Implicit None
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
    Real(dp), Intent(InOut) :: s(1:2)  !previous sum of ordinates
    Integer, Intent(In) :: d  !length of final row in OLD Romberg Table
    Real(dp), Intent(In) :: R0(1:2,0:d)  !final row of OLD romberg table
    Integer, Intent(In) :: level
    Real(dp), Intent(Out) :: q(1:2)    !the result of the integration, if convergence attained
    Real(dp) :: R(1:2,0:d+10,0:10)  !Romberg table extension
    Integer :: n,i,j
    Real(dp) :: h
    Integer :: fours
    
    R(:,0:d,0) = R0
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = 2**(d+i)
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h,p)
        End Do
        R(:,0,i) = h * s
        !fill out Romberg table row
        fours = 1
        Do j = 1,d+i
            fours = fours * 4
            R(:,j,i) = (Real(fours,dp) * R(:,j-1,i) - R(:,j-1,i-1)) / Real(fours - 1,dp)
        End Do
        !check for convergence
        If ( All( Abs(R(:,i-1,i-1) - R(:,i,i)) .LE. rTol_tier2 * Abs(R(:,i,i)) ) ) Then
            q = R(:,d+i,i)
            Return  !Normal exit
        End If
    End Do
    If (level .GT. 10) Then !max allowed recursion depth, interval has been split 100 times...
        Print *,"ERROR:  US_Std_Atm_1976: Continue_Romberg_nO1_O2:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !If we get this far, we did not converge, recurse to add 10 more rows
    Call Continue_Romberg_nO1_O2(f,a,b,p,s,d+10,R(:,:,10),level+1,q)
End Subroutine Continue_Romberg_nO1_O2

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
    Real(dp) :: R(1:2,0:10,0:10)  !Romberg table
    Integer :: n,i,j
    Real(dp) :: h,s(1:2)

    n = 1
    h = b - a
    s = 0.5_dp * (f(a,p) + f(b,p))
    R(:,0,0) = h * s
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h,p)
        End Do
        R(:,0,i) = h * s
        !fill out Romberg table row
        Do j = 1,i
            R(:,j,i) = Romb2(j) * (Romb1(j) * R(:,j-1,i) - R(:,j-1,i-1))
        End Do
        !check for convergence
        If ( All( Abs(R(:,i-1,i-1) - R(:,i,i)) .LE. rTol_tier3 * Abs(R(:,i,i)) ) ) Then
            q = R(:,i,i)  !R(i,i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
    End Do
    !If we get this far, we did not converge
    Call Continue_Romberg_nAr_He(f,a,b,p,s,10,R(:,:,10),2,q)
End Function Romberg_Quad_nAr_He
Recursive Subroutine Continue_Romberg_nAr_He(f,a,b,p,s,d,R0,level,q)  !adds 10 more rows to the previous Romberg_Quad table
    Use Kinds, Only: dp
    Implicit None
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
    Real(dp), Intent(InOut) :: s(1:2)  !previous sum of ordinates
    Integer, Intent(In) :: d  !length of final row in OLD Romberg Table
    Real(dp), Intent(In) :: R0(1:2,0:d)  !final row of OLD romberg table
    Integer, Intent(In) :: level
    Real(dp), Intent(Out) :: q(1:2)    !the result of the integration, if convergence attained
    Real(dp) :: R(1:2,0:d+10,0:10)  !Romberg table extension
    Integer :: n,i,j
    Real(dp) :: h
    Integer :: fours
    
    R(:,0:d,0) = R0
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = 2**(d+i)
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h,p)
        End Do
        R(:,0,i) = h * s
        !fill out Romberg table row
        fours = 1
        Do j = 1,d+i
            fours = fours * 4
            R(:,j,i) = (Real(fours,dp) * R(:,j-1,i) - R(:,j-1,i-1)) / Real(fours - 1,dp)
        End Do
        !check for convergence
        If ( All( Abs(R(:,i-1,i-1) - R(:,i,i)) .LE. rTol_tier3 * Abs(R(:,i,i)) ) ) Then
            q = R(:,d+i,i)
            Return  !Normal exit
        End If
    End Do
    If (level .GT. 10) Then !max allowed recursion depth, interval has been split 100 times...
        Print *,"ERROR:  US_Std_Atm_1976: Continue_Romberg_Ar_He:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !If we get this far, we did not converge, recurse to add 10 more rows
    Call Continue_Romberg_nAr_He(f,a,b,p,s,d+10,R(:,:,10),level+1,q)
End Subroutine Continue_Romberg_nAr_He

Function nH(Z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nH
    Real(dp), Intent(In) :: Z
    
    If (Z .LT. 150._dp) Then
        nH = 0._dp
        RETURN
    Else If (Z .GT. 500._dp) Then
        nH = nH500
        RETURN
    End If
    nH = (nH500 + phiH * Romberg_Quad_nH(Z,500._dp)) / p6(Z)
End Function nH

Function p6(Z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: p6
    Real(dp), Intent(In) :: Z
    !TODO Get a higher precision value for T500
    Real(dp), Parameter :: T500 = 999.235602_dp
    
    p6 = (T(Z,11) / T500)**(1._dp + alphai(6)) * Exp(Romberg_Quad_p6(Z,500._dp))
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
      & (      N7(1) *   Tb(7) * Exp(-nN2_power(Z,10)) + & 
      &    Sum(N7(2:3) * Tb(7) * Exp(-nO1_O2_powers(Z,10))) + & 
      &    Sum(N7(4:5) * Tb(7) * Exp(-nAr_He_powers(Z,10)))     ) / Tz
    f = p6(Z) / D
End Function nH_integrand

Function Romberg_Quad_nH(a,b) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp) :: R(0:10,0:10)  !Romberg table
    Integer :: n,i,j
    Real(dp) :: h,s

    n = 1
    h = b - a
    s = 0.5_dp * (nH_integrand(a) + nH_integrand(b))
    R(0,0) = h * s
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + nH_integrand(a + Real(j,dp)*h)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        Do j = 1,i
            R(j,i) = Romb2(j) * (Romb1(j) * R(j-1,i) - R(j-1,i-1))
        End Do
        !check for convergence
        If ( Abs(R(i-1,i-1) - R(i,i)) .LE. rTol_tier4b * Abs(R(i,i)) ) Then
            q = R(i,i)  !R(i,i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
    End Do
    !If we get this far, we did not converge
    Call Continue_Romberg_nH(a,b,s,10,R(:,10),2,q)
End Function Romberg_Quad_nH
Recursive Subroutine Continue_Romberg_nH(a,b,s,d,R0,level,q)  !adds 10 more rows to the previous Romberg_Quad table
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(InOut) :: s  !previous sum of ordinates
    Integer, Intent(In) :: d  !length of final row in OLD Romberg Table
    Real(dp), Intent(In) :: R0(0:d)  !final row of OLD romberg table
    Integer, Intent(In) :: level
    Real(dp), Intent(Out) :: q    !the result of the integration, if convergence attained
    Real(dp) :: R(0:d+10,0:10)  !Romberg table extension
    Integer :: n,i,j
    Real(dp) :: h
    Integer :: fours
    
    R(0:d,0) = R0
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = 2**(d+i)
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + nH_integrand(a + Real(j,dp)*h)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        fours = 1
        Do j = 1,d+i
            fours = fours * 4
            R(j,i) = (Real(fours,dp) * R(j-1,i) - R(j-1,i-1)) / Real(fours - 1,dp)
        End Do
        !check for convergence
        If ( Abs(R(i-1,i-1) - R(i,i)) .LE. rTol_tier4b * Abs(R(i,i)) ) Then
            q = R(d+i,i)
            Return  !Normal exit
        End If
    End Do
    If (level .GT. 10) Then !max allowed recursion depth, interval has been split 100 times...
        Print *,"ERROR:  US_Std_Atm_1976: Continue_Romberg_nH:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !If we get this far, we did not converge, recurse to add 10 more rows
    Call Continue_Romberg_nH(a,b,s,d+10,R(:,10),level+1,q)
End Subroutine Continue_Romberg_nH

Function p6_integrand(Z) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: Z
    Real(dp) :: Tz
    Real(dp) :: Nb(1:5)
    
    Tz = T(Z,11)
    Nb(1) = N7(1) * Tb(7) * Exp(-nN2_power(Z,10))
    Nb(2:3) = N7(2:3) * Tb(7) * Exp(-nO1_O2_powers(Z,10))
    Nb(4:5) = N7(4:5) * Tb(7) * Exp(-nAr_He_powers(Z,10))
    Nb = Nb / Tz
    f = (Sum(Nb*Mi(1:5))/Sum(Nb)) * g(Z) / (R_star * Tz)
End Function p6_integrand

Function Romberg_Quad_p6(a,b) Result(q)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp) :: R(0:10,0:10)  !Romberg table
    Integer :: n,i,j
    Real(dp) :: h,s

    n = 1
    h = b - a
    s = 0.5_dp * (p6_integrand(a) + p6_integrand(b))
    R(0,0) = h * s
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + p6_integrand(a + Real(j,dp)*h)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        Do j = 1,i
            R(j,i) = Romb2(j) * (Romb1(j) * R(j-1,i) - R(j-1,i-1))
        End Do
        !check for convergence
        If ( Abs(R(i-1,i-1) - R(i,i)) .LE. rTol_tier4a * Abs(R(i,i)) ) Then
            q = R(i,i)  !R(i,i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
    End Do
    !If we get this far, we did not converge
    Call Continue_Romberg_p6(a,b,s,10,R(:,10),2,q)
End Function Romberg_Quad_p6
Recursive Subroutine Continue_Romberg_p6(a,b,s,d,R0,level,q)  !adds 10 more rows to the previous Romberg_Quad table
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: a,b    !limits of integration
    Real(dp), Intent(InOut) :: s  !previous sum of ordinates
    Integer, Intent(In) :: d  !length of final row in OLD Romberg Table
    Real(dp), Intent(In) :: R0(0:d)  !final row of OLD romberg table
    Integer, Intent(In) :: level
    Real(dp), Intent(Out) :: q    !the result of the integration, if convergence attained
    Real(dp) :: R(0:d+10,0:10)  !Romberg table extension
    Integer :: n,i,j
    Real(dp) :: h
    Integer :: fours
    
    R(0:d,0) = R0
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = 2**(d+i)
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + p6_integrand(a + Real(j,dp)*h)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        fours = 1
        Do j = 1,d+i
            fours = fours * 4
            R(j,i) = (Real(fours,dp) * R(j-1,i) - R(j-1,i-1)) / Real(fours - 1,dp)
        End Do
        !check for convergence
        If ( Abs(R(i-1,i-1) - R(i,i)) .LE. rTol_tier4a * Abs(R(i,i)) ) Then
            q = R(d+i,i)
            Return  !Normal exit
        End If
    End Do
    If (level .GT. 10) Then !max allowed recursion depth, interval has been split 100 times...
        Print *,"ERROR:  US_Std_Atm_1976: Continue_Romberg_p6:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !If we get this far, we did not converge, recurse to add 10 more rows
    Call Continue_Romberg_p6(a,b,s,d+10,R(:,10),level+1,q)
End Subroutine Continue_Romberg_p6

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
            If (b.EQ.6 .AND. Z.GT.80._dp) Tz = Tz * T_M0_correction(Z)  !US Standard Atmosphere 1976 equation 22
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
    Real(dp) :: Tz,Pz,N(1:5)
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
            If (b.EQ.6 .AND. Z.GT.80._dp) Tz = Tz * T_M0_correction(Z)  !US Standard Atmosphere 1976 equation 22
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

# if TEST
!---------------------------------------------------------------------------------
!  The following routines are used only for computing the 'stop' values for the 
!  number density integrals:  Used to compute necessary constants which are then 
!  hard-coded in the source.  They are included in compilation by defining 'TEST' 
!  as a conditional compiler directive for the preprocessor
!---------------------------------------------------------------------------------
Function nN2_power_stops() Result(xb)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: xb(1:5,1:2)
    Real(dp), Parameter :: Zs(1:5) = (/  86._dp, &
                                      &  91._dp, & 
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 120._dp  /)
    Integer, Parameter :: bs(1:5) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     & 10  /)

    xb(:,1) = Zs
    xb(:,2) = 0._dp
    Do i = 2,5
        xb(i,2) = Sum(xb(1:i-1,2)) + Romberg_Quad_nN2_hi(Zs(i-1),Zs(i),b(i-1))
    End Do
End Function nN2_power_stops

Function Romberg_Quad_hi(a,b,p) Result(q)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: a,b    !limits of integration
    Integer, Intent(In) :: p
    Real(dp) :: R(0:10,0:10)  !Romberg table
    Integer :: n,i,j
    Real(dp) :: h,s,as
    Real(dp), Parameter :: rtol = 1.E-16_dp

    n = 1
    h = b - a
    s = 0.5_dp * (g(a) / T(a,p+1) + g(b) / T(b,p+1))
    R(0,0) = h * s
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            as = a + Real(j,dp)*h
            s = s + g(as) / T(as,p+1)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        Do j = 1,i
            R(j,i) = Romb2(j) * (Romb1(j) * R(j-1,i) - R(j-1,i-1))
        End Do
        !check for convergence
        If ( Converged(R(i-1,i-1),R(i,i),rtol=1.E-18_dp,atol=1.E-21_dp) ) Then
            q = R(i,i)  !R(i,i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
    End Do
    !If we get this far, we did not converge
    Call Continue_Romberg_nN2_hi(a,b,p,s,10,R(:,10),2,q)
End Function Romberg_Quad_nN2
Recursive Subroutine Continue_Romberg_nN2_hi(a,b,p,s,d,R0,level,q)  !adds 10 more rows to the previous Romberg_Quad table
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
    Real(dp), Intent(In) :: a,b    !limits of integration
    Integer, Intent(In) :: p
    Real(dp), Intent(InOut) :: s  !previous sum of ordinates
    Integer, Intent(In) :: d  !length of final row in OLD Romberg Table
    Real(dp), Intent(In) :: R0(0:d)  !final row of OLD romberg table
    Integer, Intent(In) :: level
    Real(dp), Intent(Out) :: q    !the result of the integration, if convergence attained
    Real(dp) :: R(0:d+10,0:10)  !Romberg table extension
    Integer :: n,i,j
    Real(dp) :: h,as
    Integer :: fours
        
    R(0:d,0) = R0
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = 2**(d+i)
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            as = a + Real(j,dp)*h
            s = s + g(as) / T(as,p+1)
        End Do
        R(0,i) = h * s
        !fill out Romberg table row
        fours = 1
        Do j = 1,d+i
            fours = fours * 4
            R(j,i) = (Real(fours,dp) * R(j-1,i) - R(j-1,i-1)) / Real(fours - 1,dp)
        End Do
        !check for convergence
        If ( Converged(R(i-1,i-1),R(i,i),rtol=1.E-18_dp,atol=1.E-21_dp) ) Then
            q = R(d+i,i)
            Return  !Normal exit
        End If
    End Do
    If (level .GT. 10) Then !max allowed recursion depth, interval has been split 100 times...
        Print *,"ERROR:  US_Std_Atm_1976: Continue_Romberg_nN2_hi:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !If we get this far, we did not converge, recurse to add 10 more rows
    Call Continue_Romberg_nN2_hi(a,b,p,s,d+10,R(:,10),level+1,q)
End Subroutine Continue_Romberg_nN2_hi

Function nO1_O2_power_stops() Result(xb)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: xb(1:8,1:3)
    Real(dp), Parameter :: Zs(1:8) = (/  86._dp, &
                                      &  91._dp, & 
                                      &  95._dp, & 
                                      &  97._dp, &
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 115._dp, & 
                                      & 120._dp  /)
    Integer, Parameter :: bs(1:8) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     &  9, & 
                                     & 10  /)

    xb(:,1) = Zs
    xb(:,2:3) = 0._dp
    xb(2,2:3) = Sum(xb(1:1,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nO1_O2_integrand1_hi,Zs(1),Zs(2),b(1)) !up to 91km
    xb(3,2:3) = Sum(xb(1:2,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nO1_O2_integrand1_hi,Zs(2),Zs(3),b(2)) !up to 95km
    xb(4,2:3) = Sum(xb(1:3,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nO1_O2_integrand2_hi,Zs(3),Zs(4),b(3)) !up to 97km
    xb(5,2:3) = Sum(xb(1:4,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nO1_O2_integrand3_hi,Zs(4),Zs(5),b(4)) !up to 100km
    xb(6,2:3) = Sum(xb(1:5,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nO1_O2_integrand4_hi,Zs(5),Zs(6),b(5)) !up to 110km
    xb(7,2:3) = Sum(xb(1:6,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nO1_O2_integrand4_hi,Zs(6),Zs(7),b(6)) !up to 115km
    xb(8,2:3) = Sum(xb(1:7,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nO1_O2_integrand5,Zs(7),Zs(8),b(7)) !up to 120km
End Function nO1_O2_power_stops

Function nO1_O2_integrand1_hi(Z,b) Result(f)  !for 86 to 95 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    
    Tz = T(Z,b+1)
    D = ai(2:3) * (Tz / 273.15_dp)**bi(2:3) / (N7(1) * Tb(7) * Exp(-nN2_power_hi(Z,b)) / Tz)
    f = g(Z) * D * (Mi(2:3) + M0*K0/D) / (R_star * Tz * (D + K0)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
    f(1) = f(1) + littleQi * (littleUi - Z)**2 * Exp(-littleWi*(littleUi - Z)**3)
End Function nO1_O2_integrand1_hi

Function nO1_O2_integrand2_hi(Z,b) Result(f)  !for 95 to 97 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    Real(dp) :: K
    
    Tz = T(Z,b+1)
    D = ai(2:3) * (Tz / 273.15_dp)**bi(2:3) / (N7(1) * Tb(7) * Exp(-nN2_power_hi(Z,b)) / Tz)
    K = K0 * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
    f = g(Z) * D * (Mi(2:3) + M0*K/D) / (R_star * Tz * (D + K)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
    f(1) = f(1) + littleQi * (littleUi - Z)**2 * Exp(-littleWi*(littleUi - Z)**3)
End Function nO1_O2_integrand2_hi

Function nO1_O2_integrand3_hi(Z,b) Result(f)  !for 97 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    Real(dp) :: K

    Tz = T(Z,b+1)
    D = ai(2:3) * (Tz / 273.15_dp)**bi(2:3) / (N7(1) * Tb(7) * Exp(-nN2_power_hi(Z,b)) / Tz)
    K = 1.2E2_dp * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
    f = g(Z) * D * (Mi(2:3) + M0*K/D) / (R_star * Tz * (D + K)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
End Function nO1_O2_integrand3_hi

Function nO1_O2_integrand4_hi(Z,b) Result(f)  !for 100 to 115 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    Real(dp) :: K
    
    Tz = T(Z,b+1)
    D = ai(2:3) * (Tz / 273.15_dp)**bi(2:3) / (N7(1) * Tb(7) * Exp(-nN2_power_hi(Z,b)) / Tz)
    K = K0 * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
    f = g(Z) * D * (Mi(2:3) + Mi(1)*K/D) / (R_star * Tz * (D + K)) + & 
      & bigQi(2:3) * (Z - bigUi(2:3))**2 * Exp(-bigWi(2:3)*(Z - bigUi(2:3))**3)
End Function nO1_O2_integrand4_hi

Function nN2_power_hi(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: M_over_R
    Logical :: Z_below_100
    Real(dp), Parameter :: xb(7:10) = (/ 0._dp,                 &  !Z = 86km
                                       & 0.8891738712368936_dp, &  !Z = 91km
                                       & 3.9815997728018476_dp, &  !Z = 110km
                                       & 5.0588195691573031_dp  /) !Z = 120km
    Real(dp), Parameter :: xb_100 = 2.4639390409132486_dp  !Z = 100km
    Logical, Parameter :: no_sublayers(7:10) = (/ .TRUE.,  &
                                                & .FALSE., &
                                                & .TRUE.,  &
                                                & .TRUE.   /)
    Real(dp), Parameter :: rho_star_N2 = Mi(1) / R_star
    
    If (Z .LE. 100._dp) Then
        M_over_R = rho_star
        Z_below_100 = .TRUE.
    Else
        M_over_R = rho_star_N2
        Z_below_100 = .FALSE.
    End If
    If (no_sublayers(b)) Then !b=7, 9, or 10
        x = xb(b) + M_over_R * Romberg_Quad_nN2_hi(Zb(b),Z,b)
    Else !b=8
        If (Z_below_100) Then
            x = xb(8) + M_over_R * Romberg_Quad_nN2_hi(Zb(8),Z,8)
        Else
            x = xb_100 + M_over_R * Romberg_Quad_nN2_hi(100._dp,Z,8)
        End If
    End If
End Function nN2_power_hi

Function Romberg_Quad_nO1_O2Ar_he_hi(f,a,b,p) Result(q)
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
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
    Real(dp) :: R(1:2,0:10,0:10)  !Romberg table
    Integer :: n,i,j
    Real(dp) :: h,s(1:2)

    n = 1
    h = b - a
    s = 0.5_dp * (f(a,p) + f(b,p))
    R(:,0,0) = h * s
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = n * 2
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h,p)
        End Do
        R(:,0,i) = h * s
        !fill out Romberg table row
        Do j = 1,i
            R(:,j,i) = Romb2(j) * (Romb1(j) * R(:,j-1,i) - R(:,j-1,i-1))
        End Do
        !check for convergence
        If ( Converged(R(1,i-1,i-1),R(1,i,i)rtol=1.E-16_dp,atol=1.E-20_dp) .AND. & 
           & Converged(R(2,i-1,i-1),R(2,i,i)rtol=1.E-16_dp,atol=1.E-20_dp)       ) Then
            q = R(:,i,i)  !R(i,i) is the position of the highest precision converged value
            Return  !Normal exit
        End If
    End Do
    !If we get this far, we did not converge
    Call Continue_Romberg_nO1_O2_Ar_he_hi(f,a,b,p,s,10,R(:,:,10),2,q)
End Function Romberg_Quad_nO1_O2Ar_he_hi
Recursive Subroutine Continue_Romberg_nO1_O2_Ar_he_hi(f,a,b,p,s,d,R0,level,q)  !adds 10 more rows to the previous Romberg_Quad table
    Use Kinds, Only: dp
    Use Utilities, Only: Converged
    Implicit None
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
    Real(dp), Intent(InOut) :: s(1:2)  !previous sum of ordinates
    Integer, Intent(In) :: d  !length of final row in OLD Romberg Table
    Real(dp), Intent(In) :: R0(1:2,0:d)  !final row of OLD romberg table
    Integer, Intent(In) :: level
    Real(dp), Intent(Out) :: q(1:2)    !the result of the integration, if convergence attained
    Real(dp) :: R(1:2,0:d+10,0:10)  !Romberg table extension
    Integer :: n,i,j
    Real(dp) :: h
    Integer :: fours
    
    R(:,0:d,0) = R0
    Do i = 1,10
        !compute trapezoid estimate for next row of table
        n = 2**(d+i)
        h = (b - a) / Real(n,dp)
        Do j = 1,n-1,2  !only odd values of j, these are the NEW points at which to evaluate f
            s = s + f(a + Real(j,dp)*h,p)
        End Do
        R(:,0,i) = h * s
        !fill out Romberg table row
        fours = 1
        Do j = 1,d+i
            fours = fours * 4
            R(:,j,i) = (Real(fours,dp) * R(:,j-1,i) - R(:,j-1,i-1)) / Real(fours - 1,dp)
        End Do
        !check for convergence
        If ( Converged(R(1,i-1,i-1),R(1,i,i)rtol=1.E-16_dp,atol=1.E-20_dp) .AND. & 
           & Converged(R(2,i-1,i-1),R(2,i,i)rtol=1.E-16_dp,atol=1.E-20_dp)       ) Then
            q = R(:,d+i,i)
            Return  !Normal exit
        End If
    End Do
    If (level .GT. 10) Then !max allowed recursion depth, interval has been split 100 times...
        Print *,"ERROR:  US_Std_Atm_1976: Continue_Romberg_nO1_O2_Ar_he_hi:  Failed to converge before reaching max recursion depth."
        ERROR STOP
    End If
    !If we get this far, we did not converge, recurse to add 10 more rows
    Call Continue_Romberg_nO1_O2_Ar_he_hi(f,a,b,p,s,d+10,R(:,:,10),level+1,q)
End Subroutine Continue_Romberg_nO1_O2_Ar_he_hi

Function nAr_He_power_stops() Result(xb)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: xb(1:7,1:3)
    Real(dp), Parameter :: Zs(1:7) = (/  86._dp, &
                                      &  91._dp, & 
                                      &  95._dp, & 
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 115._dp, & 
                                      & 120._dp  /)
    Integer, Parameter :: bs(1:7) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     &  9, & 
                                     & 10  /)

    xb(:,1) = Zs
    xb(:,2:3) = 0._dp
    xb(2,2:3) = Sum(xb(1:1,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nAr_He_integrand1_hi,Zs(1),Zs(2),b(1)) !up to 91km
    xb(3,2:3) = Sum(xb(1:2,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nAr_He_integrand1_hi,Zs(2),Zs(3),b(2)) !up to 95km
    xb(4,2:3) = Sum(xb(1:3,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nAr_He_integrand2_hi,Zs(3),Zs(4),b(3)) !up to 100km
    xb(5,2:3) = Sum(xb(1:4,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nAr_He_integrand4_hi,Zs(4),Zs(5),b(4)) !up to 110km
    xb(6,2:3) = Sum(xb(1:5,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nAr_He_integrand4_hi,Zs(5),Zs(6),b(5)) !up to 115km
    xb(7,2:3) = Sum(xb(1:6,2:3),DIM=1) + Romberg_Quad_nO1_O2Ar_he_hi(nAr_He_integrand5,Zs(6),Zs(7),b(6)) !up to 120km
End Function nAr_He_power_stops

Function nAr_He_integrand1_hi(Z,b) Result(f)  !for 86 to 95 km
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
    Nb(1) = N7(1) * Tb(7) * Exp(-nN2_power_hi(Z,b))
    Nb(2:3) = N7(2:3) * Tb(7) * Exp(-nO1_O2_powers_hi(Z,b))
    Nb = Nb / Tz
    D = ai(4:5) * (Tz / 273.15_dp)**bi(4:5) / Sum(Nb)
    y = D / (R_star * Tz * (D + K0))
    f = g(Z) * y * (Mi(4:5) + M0*K0/D) + & 
      & bigQi(4:5) * (Z - bigUi(4:5))**2 * Exp(-bigWi(4:5)*(Z - bigUi(4:5))**3)
    f(2) = f(2) + y(2) * alphai(5) * dT_dZ(Z,b+1)
End Function nAr_He_integrand1_hi

Function nAr_He_integrand2_hi(Z,b) Result(f)  !for 95 to 100 km
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
    Nb(1) = N7(1) * Tb(7) * Exp(-nN2_power_hi(Z,b))
    Nb(2:3) = N7(2:3) * Tb(7) * Exp(-nO1_O2_powers_hi(Z,b))
    Nb = Nb / Tz
    D = ai(4:5) * (Tz / 273.15_dp)**bi(4:5) / Sum(Nb)
    K = K0 * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
    y = D / (R_star * Tz * (D + K))
    f = g(Z) * y * (Mi(4:5) + M0*K/D) + & 
      & bigQi(4:5) * (Z - bigUi(4:5))**2 * Exp(-bigWi(4:5)*(Z - bigUi(4:5))**3)
    f(2) = f(2) + y(2) * alphai(5) * dT_dZ(Z,b+1)
End Function nAr_He_integrand2_hi

Function nAr_He_integrand4_hi(Z,b) Result(f)  !for 100 to 115 km
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
    Nb(1) = N7(1) * Tb(7) * Exp(-nN2_power_hi(Z,b))
    Nb(2:3) = N7(2:3) * Tb(7) * Exp(-nO1_O2_powers_hi(Z,b))
    Nb = Nb / Tz
    D = ai(4:5) * (Tz / 273.15_dp)**bi(4:5) / Sum(Nb)
    K = K0 * Exp(1._dp - 400._dp / (400._dp - (Z - 95._dp)**2))
    y = D / (R_star * Tz * (D + K))
    f = g(Z) * y * (Mi(4:5) + (Sum(Nb*Mi(1:3))/Sum(Nb))*K/D) + & 
      & bigQi(4:5) * (Z - bigUi(4:5))**2 * Exp(-bigWi(4:5)*(Z - bigUi(4:5))**3)
    f(2) = f(2) + y(2) * alphai(5) * dT_dZ(Z,b+1)
End Function nAr_He_integrand4_hi

Function nO1_O2_powers_hi(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Logical :: Z_below_97
    Real(dp), Parameter :: xb(1:2,7:10) = Reshape( (/  0._dp,                 &  !O1, Z = 86km
                                                    &  0._dp,                 &  !O2, Z = 86km
                                                    & -1.2335158785532025_dp, &  !O1, Z = 91km
                                                    &  0.8987089660301271_dp, &  !O2, Z = 91km
                                                    & -1.2350403922105528_dp, &  !O1, Z = 110km
                                                    &  4.5003526937771803_dp, &  !O2, Z = 110km
                                                    & -0.7312338738839638_dp, &  !O1, Z = 120km
                                                    &  5.8804681169361679_dp  /), &  !O2, Z = 120km
                                                    & (/2,4/) )
    Real(dp), Parameter :: xb_95(1:2) =  (/ -1.6326227572400966_dp, &  !O1, Z = 95km
                                          &  1.6401385731339726_dp  /) !O2, Z = 95km
    Real(dp), Parameter :: xb_97(1:2) =  (/ -1.6736396506936295_dp, &  !O1, Z = 97km
                                          &  2.0206764985042182_dp  /) !O2, Z = 97km
    Real(dp), Parameter :: xb_100(1:2) = (/ -1.6519505201748717_dp, &  !O1, Z = 100km
                                          &  2.6026369578525224_dp  /) !O2, Z = 100km
    Real(dp), Parameter :: xb_115(1:2) = (/ -0.9801497240119973_dp, &  !O1, Z = 115km
                                          &  5.2767050379951361_dp  /) !O2, Z = 115km
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
            x = Romberg_Quad_nO1_O2_hi(nO1_O2_integrand1_hi,Zb(7),Z,7)
        Else !b=10
            x = xb(:,10) + Romberg_Quad_nO1_O2_hi(nO1_O2_integrand5,Zb(10),Z,10)
        End If
    Else
        If (Z_below_97) Then !b=8
            If (Z .LT. 95._dp) Then
                x = xb(:,8) + Romberg_Quad_nO1_O2_hi(nO1_O2_integrand1_hi,Zb(8),Z,8)
            Else
                x = xb_95 + Romberg_Quad_nO1_O2_hi(nO1_O2_integrand2_hi,95._dp,Z,8)
            End If
        Else !b=8 OR b=9
            If (Z .LT. 100._dp) Then !b=8
                x = xb_97 + Romberg_Quad_nO1_O2_hi(nO1_O2_integrand2_hi,97._dp,Z,8)
            Else If (b .EQ. 8) Then !b=8
                x = xb_100 + Romberg_Quad_nO1_O2_hi(nO1_O2_integrand3_hi,100._dp,Z,8)
            Else !b=9
                If (Z .LT. 115._dp) Then
                    x = xb(:,9) + Romberg_Quad_nO1_O2_hi(nO1_O2_integrand4_hi,Zb(9),Z,9)
                Else
                    x = xb_115 + Romberg_Quad_nO1_O2_hi(nO1_O2_integrand5,115._dp,Z,9)
                End If
            End If
        End If
    End If
End Function nO1_O2_powers_hi
# endif

End Module US_Std_Atm_1976

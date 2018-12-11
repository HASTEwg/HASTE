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
    Real(dp), Parameter :: rho_star = M0 / R_star  !precomputed quantity for 1976 density calculations
    Real(dp), Parameter :: Tc = (Lb(9) * (Zb(9)-Zb(8)) * Tb(9) + Tb(8)**2 - Tb(9)**2) / &
                              & (Lb(9) * (Zb(9)-Zb(8)) + 2._dp * Tb(8) - 2._dp * Tb(9))  !US Standard Atmosphere 1976 equation B-8
    Real(dp), Parameter :: big_A = Tb(8) - Tc  !US Standard Atmosphere 1976 equation B-5
    Real(dp), Parameter :: little_A = (Zb(9)-Zb(8)) * big_A / Sqrt(big_A**2 - (Tb(9)-Tc)**2)  !US Standard Atmosphere 1976 equation B-9
    Real(dp), Parameter :: T_inf = 1000._dp
    Real(dp), Parameter :: lambda = Lb(9) / (T_inf - Tb(10))  !precomputed quantity for 1976 temperature calculations
    Real(dp), Parameter :: R_Z7 = R_Earth + Zb(7)
    Real(dp), Parameter :: R_Z9 = R_Earth + Zb(9)
    Real(dp), Parameter :: R_Z10 = R_Earth + Zb(10)
    Real(dp), Parameter :: Na = 6.022169E26_dp  ![1/kmol] Avagadro's Number
    Real(dp), Parameter :: K0 = 1.2E2_dp
    Real(dp), Parameter :: Mi(1:6) = (/ 28.0134_dp, &  !N2
                                      & 15.9994_dp, &  !O1
                                      & 31.9988_dp, &  !O2
                                      & 39.948_dp,  &  !Ar
                                      &  4.0026_dp, &  !He
                                      &  0.5_dp * 2.01594_dp  /) !H1  !US Standard Atmosphere 1976 table 3
    Real(dp), Parameter :: alphaHe = -0.40_dp  !He  !US Standard Atmosphere 1976 table 6
    Real(dp), Parameter :: alphaHe_star = alphaHe * R_star  !precomputed quantity for 1976 He number density calculations
    ! Real(dp), Parameter :: alphaH1 = -0.25_dp  !H1  !US Standard Atmosphere 1976 table 6
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
    Real(dp), Parameter :: N7_T7(1:5) = N7 * Tb(7)  !precomputed quantity for 1976 diffusion coeff calculations
    Real(dp), Parameter :: nH500 = 8.E10_dp
    Real(dp), Parameter :: phiH = 7.2E11_dp
    !Convergence criteria for quadrature routines
#   if (INTEGRAND_STOPS || GL_POINTS)
        Real(dp), Parameter :: rTol_tier1 = 1.E-15_dp  !N2
        Real(dp), Parameter :: rTol_tier2 = 1.E-14_dp  !O1 and O2
        Real(dp), Parameter :: rTol_tier3 = 1.E-13_dp  !Ar and He
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
    If (Lb_nonzero(b)) Then !b=0,2,3,5,6,9
        If (T_linear_by_H(b)) Then !b=0,2,3,5,6
            T = Teq23(Z,b)  !US Standard Atmosphere 1976 equation 23
            If (b.EQ.6 .AND. Z.GT.80._dp) T = T * T_M0_correction(Z)  !US Standard Atmosphere 1976 equation 22
        Else !b=9
            T = Teq29(Z)  !US Standard Atmosphere 1976 equation 29
        End If
    Else If (T_exponential(b)) Then !b=10
        T = Teq31(Z)  !US Standard Atmosphere 1976 equation 31
    Else If (T_elliptical(b)) Then !b=8
        T = Teq27(Z)  !US Standard Atmosphere 1976 equation 27
    Else !zero lapse rate, b = 1,4,7
        T = Tb(b)
    End If
End Function T

Function Teq23(Z,b) !b=0,2,3,5,6
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Teq23
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp), Parameter :: Tb_minus_LbHb(0:7) = Tb(0:7) - Lb(0:7)*Hb(0:7)  !precomputed quantity for 1976 temperature calculations

    Teq23 = Tb_minus_LbHb(b) + Lb(b) * Z_to_H(Z)  !US Standard Atmosphere 1976 equation 23
End Function Teq23

Function Teq27(Z) !b=8
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Teq27
    Real(dp), Intent(In) :: Z

    Teq27 = Tc + big_A * Sqrt(1._dp - ((Z - Zb(8)) / little_A)**2)  !US Standard Atmosphere 1976 equation 27
End Function Teq27

Function Teq29(Z) !b=9
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Teq29
    Real(dp), Intent(In) :: Z

    Teq29 = Tb(9) + Lb(9) * (Z - Zb(9))  !US Standard Atmosphere 1976 equation 29
End Function Teq29

Function Teq31(Z) !b=10
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Teq31
    Real(dp), Intent(In) :: Z

    Teq31 = T_inf - (T_inf - Tb(10)) * Exp(-lambda * (Z - Zb(10)) * R_Z10 / (R_Earth + Z))  !US Standard Atmosphere 1976 equation 31
End Function Teq31

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
    If (Lb_nonzero(b)) Then !b=0,2,3,5,6,9
        dT_dZ = Lb(b)
        If (b.EQ.6 .AND. Z.GT.80._dp) dT_dZ = dT_dZ * T_M0_correction(Z)  !US Standard Atmosphere 1976 equation 22
    Else If (T_exponential(b)) Then  !b=10
        dT_dZ = lambda * (T_inf - Tb(10)) * (R_Z10 / (R_Earth + Z))**2 * Exp(-lambda * (Z - Zb(10)) * R_Z10 / (R_Earth + Z))  !US Standard Atmosphere 1976 equation 32
    Else If (T_elliptical(b)) Then  !b=8
        dT_dZ = -big_A * (Z - Zb(8)) / ((little_A**2) * Sqrt(1._dp - ((Z - Zb(8)) / little_A)**2))  !US Standard Atmosphere 1976 equation 28
    Else !b=1,4,7
        dT_dZ = 0._dp
    End If
End Function dT_dZ

Function T_M0_correction(Z) Result(c)
    !Computes correction factor to convert molecular temperature to kinetic temperature for geometric altitudes 80-86km
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
    Real(dp), Parameter :: xb(8:10) = (/ 0.8891738712368936_dp, &  !Z = 91km
                                       & 3.9815997728018484_dp, &  !Z = 110km
                                       & 5.0588195691573041_dp  /) !Z = 120km
    Real(dp), Parameter :: xb_100 = 2.4639390409132491_dp  !Z = 100km
    Logical, Parameter :: no_sublayers(7:10) = (/ .TRUE.,  &
                                                & .FALSE., &
                                                & .TRUE.,  &
                                                & .TRUE.   /)
    Real(dp), Parameter :: rho_star_N2 = Mi(1) / R_star
    !Precomputed parameters for b = 7
    Real(dp), Parameter :: c7 = rho_star * g0 * R_Earth * (R_Earth/R_Z7) / Tb(7)
    !precomputed parameters for b=9
    Real(dp), Parameter :: c9a = rho_star_N2 * g0 * (R_Earth / (Tb(9) - Lb(9)*R_Z9))**2
    Real(dp), Parameter :: c9b = -Log(Tb(9)/R_Z9)
    Real(dp), Parameter :: c9c = (Lb(9)*R_Z9 - Tb(9)) / R_Z9
    !precomputed parameters for b=10
    Real(dp), Parameter :: c10a = rho_star_N2 * g0 * (R_Earth/R_Z10)**2 / (T_inf * lambda)
    Real(dp), Parameter :: c10b = -lambda * R_Z10**2
    Real(dp), Parameter :: c10c = lambda * R_Z10 - Log(Tb(10))

    If (no_sublayers(b)) Then !b=7, 9, or 10
        If (b .EQ. 7) Then !b=7
            x = c7 * (Z - Zb(7)) / (R_Earth + Z)
        Else If (b .EQ. 9) Then !b=9
            x = xb(9) + c9a * ( Lb(9) * (Log(T(Z,10)/(R_Earth+Z)) + c9b) - c9c * (Z-Zb(9)) / (R_Earth+Z) )
        Else !b=10
            x = xb(10) + c10a * (Log(T(Z,11)) + c10b / (R_Earth + Z) + c10c)
        End If
    Else !b=8
        If (Z .LT. 100._dp) Then
            x = xb(8) + rho_star * GL_Quad_nN2_8a(Z)
        Else
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

Function GL_Quad_nN2_8a(z) Result(q)  !for 91 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 6  !GL-points for 12 digits of precision
    Integer, Parameter :: n0 = 1
    Integer, Parameter :: n1 = 3
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.4679139345726910473898703439895509948116556057692105353116253200_dp, &
                                        & 0.3607615730481386075698335138377161116615218927467454822897392402_dp, &
                                        & 0.1713244923791703450402961421727328935268225014840439823986354398_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.2386191860831969086305017216807119354186106301400213501813951646_dp, &
                                        & 0.6612093864662645136613995950199053470064485643951700708145267059_dp, &
                                        & 0.9324695142031520278123015544939946091347657377122898248725496165_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(8))
    c2 = 0.5_dp * (z+Zb(8))
    q = 0._dp
    Do i = 1,n1
        q = q + wi(i) * (nN2_integrand(xi(i)*c1 + c2,8) + nN2_integrand(-xi(i)*c1 + c2,8))
    End Do
    q = q * c1
End Function GL_Quad_nN2_8a

Function GL_Quad_nN2_8b(z) Result(q)  !for 100 to 110 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 17  !GL-points for 12 digits of precision
    Integer, Parameter :: n0 = 0
    Integer, Parameter :: n1 = 8
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.1794464703562065254582656442618856214487803198976685236676686980_dp, &
                                        & 0.1765627053669926463252709901131972391509244180007481180431444069_dp, &
                                        & 0.1680041021564500445099706637883231550211981289650740142699558513_dp, &
                                        & 0.1540457610768102880814315948019586119404830584710179343852647114_dp, &
                                        & 0.1351363684685254732863199817023501973721258532344890203779946107_dp, &
                                        & 0.1118838471934039710947883856263559267358434242630770500184864824_dp, &
                                        & 0.0850361483171791808835353701910620738504913892185054757664103607_dp, &
                                        & 0.0554595293739872011294401653582446605128462519532288469937240787_dp, &
                                        & 0.0241483028685479319601100262875653246916973159450252783111851488_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                        & 0.1784841814958478558506774936540655574754193326915256435629518143_dp, &
                                        & 0.3512317634538763152971855170953460050405397515756750233191610195_dp, &
                                        & 0.5126905370864769678862465686295518745829237224111729059127314990_dp, &
                                        & 0.6576711592166907658503022166430023351478058914759732438052316955_dp, &
                                        & 0.7815140038968014069252300555204760502239724727405685125133145355_dp, &
                                        & 0.8802391537269859021229556944881556926234168179344279003519101593_dp, &
                                        & 0.9506755217687677612227169578958030214433850465591087076699692124_dp, &
                                        & 0.9905754753144173356754340199406652765077898504595643027839087867_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-100._dp)
    c2 = 0.5_dp * (z+100._dp)
    q = wi(0) * nN2_integrand(c2,8)
    Do i = 1,n1
        q = q + wi(i) * (nN2_integrand(xi(i)*c1 + c2,8) + nN2_integrand(-xi(i)*c1 + c2,8))
    End Do
    q = q * c1
End Function GL_Quad_nN2_8b

Function nO1_O2_powers(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp), Parameter :: xb(1:2,8:10) = Reshape( (/ -1.2335158785532041_dp, &     !O1, Z = 91km
                                                    &  0.8987089660301270_dp, &     !O2, Z = 91km
                                                    & -1.2350403922105540_dp, &     !O1, Z = 110km
                                                    &  4.5003526937771818_dp, &     !O2, Z = 110km
                                                    & -0.7312338738839658_dp, &     !O1, Z = 120km
                                                    &  5.8804681169361679_dp  /), & !O2, Z = 120km
                                                  & (/2,3/) )
    Real(dp), Parameter :: xb_95(1:2) =  (/ -1.6326227572400986_dp, &  !O1, Z = 95km
                                          &  1.6401385731339722_dp  /) !O2, Z = 95km
    Real(dp), Parameter :: xb_97(1:2) =  (/ -1.6736396506936311_dp, &  !O1, Z = 97km
                                          &  2.0206764985042174_dp  /) !O2, Z = 97km
    Real(dp), Parameter :: xb_100(1:2) = (/ -1.6519505201748732_dp, &  !O1, Z = 100km
                                          &  2.6026369578525217_dp  /) !O2, Z = 100km
    Real(dp), Parameter :: xb_115(1:2) = (/ -0.9801497240119992_dp, &  !O1, Z = 115km
                                          &  5.2767050379951366_dp  /) !O2, Z = 115km
    Logical, Parameter :: no_sublayers(7:10) = (/ .TRUE.,  &
                                                & .FALSE., &
                                                & .FALSE., &
                                                & .TRUE.   /)
    Real(dp), Parameter :: bigQ_3W(1:2) = -bigQi(2:3) / (3._dp * bigWi(2:3))
    Real(dp), Parameter :: eWUZb3(1:2,7:10) = Reshape( (/ bigQ_3W(1) * Exp(bigWi(2) * (bigUi(2) - Zb(7))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(3) * (bigUi(3) - Zb(7))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(2) * (bigUi(2) - Zb(8))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(3) * (bigUi(3) - Zb(8))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(2) * (bigUi(2) - Zb(9))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(3) * (bigUi(3) - Zb(9))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(2) * (bigUi(2) - Zb(10))**3), &
                                                        & bigQ_3W(2) * Exp(bigWi(3) * (bigUi(3) - Zb(10))**3)  /), &
                                                     & (/2,4/) )
    Real(dp), Parameter :: littleQ_3W = littleQi / (3._dp * littleWi)
    Real(dp), Parameter :: enWZbU3(7:8) = (/ littleQ_3W * Exp(-littleWi * (littleUi - Zb(7))**3), &
                                             littleQ_3W * Exp(-littleWi * (littleUi - Zb(8))**3)  /)
    Real(dp), Parameter :: rho_star_O1_O2(1:2) = Mi(2:3) / R_star
    !precomputed parameters for b=7
    Real(dp), Parameter :: c7a      = rho_star * g0 * R_Earth * (R_Earth / R_Z7) / Tb(7)
    Real(dp), Parameter :: c7b(1:2) = (M0 - Mi(2:3)) / M0
    Real(dp), Parameter :: c7c      = K0 * N7(1)
    Real(dp), Parameter :: c7d(1:2) = ai(2:3) * (Tb(7)/273.15_dp)**bi(2:3)
    Real(dp), Parameter :: c7e(1:2) = Log(c7c + c7d)
    !precomputed parameters for b=8b
    Real(dp), Parameter :: eWUZb3_95(1:2) = bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - 95._dp)**3)
    Real(dp), Parameter :: enWZbU3_95     = littleQ_3W * Exp(-littleWi * (littleUi - 95._dp)**3)
    !precomputed parameters for b=8c
    Real(dp), Parameter :: eWUZb3_97(1:2) = bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - 97._dp)**3)
    !precomputed parameters for b=8d
    Real(dp), Parameter :: eWUZb3_100(1:2) = bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - 100._dp)**3)
    !precomputed parameters for b=9b
    Real(dp), Parameter :: R_Z115    = R_Earth + 115._dp    
    Real(dp), Parameter :: T115      = Tb(9) + Lb(9)*(115._dp - Zb(9))
    Real(dp), Parameter :: Tb9_Lb9R9 = Tb(9) - Lb(9)*R_Z9
    Real(dp), Parameter :: c9ba(1:2) = rho_star_O1_O2 * g0 * (R_Earth / Tb9_Lb9R9)**2
    Real(dp), Parameter :: c9bb      = Tb9_Lb9R9 / R_Z115
    Real(dp), Parameter :: c9bc      = Log(R_Z115 / T115)
    Real(dp), Parameter :: eWUZb3_115(1:2) = bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - 115._dp)**3)
    !precomputed parameters for b=10
    Real(dp), Parameter :: c10a(1:2) = rho_star_O1_O2 * g0 * (R_Earth/R_Z10)**2 / (T_inf * lambda)
    Real(dp), Parameter :: c10b      = -lambda * R_Z10**2
    Real(dp), Parameter :: c10c      = lambda * R_Z10 - Log(Tb(10))

    If (no_sublayers(b)) Then !b=7,10
        If (b .EQ. 7) Then !b=7
            x = c7a * (Z - Zb(7)) / (R_Earth + Z) + c7b * (c7e - Log(c7c + c7d*Exp(nN2_power(Z,7))))
            x = x + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Z)**3) - eWUZb3(:,7)
            x(1) = x(1) + littleQ_3W * Exp(-littleWi * (littleUi - Z)**3) - enWZbU3(7)
        Else !b=10
            x = xb(:,10) + c10a * (Log(T(Z,11)) + c10b / (R_Earth + Z) + c10c)
            x = x + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Z)**3) - eWUZb3(:,10)
        End If
    Else !b=8,9
        If (b .EQ. 8) Then !b=8
            If (Z .LT. 95._dp) Then !91-95km
                x = xb(:,8) + GL_Quad_nO1_O2_8a(Z)
                x = x + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Z)**3) - eWUZb3(:,8)
                x(1) = x(1) + littleQ_3W * Exp(-littleWi * (littleUi - Z)**3) - enWZbU3(8)
            Else If (Z .LT. 97._dp) Then !95-97km
                x = xb_95 + GL_Quad_nO1_O2_8b(Z)
                x = x + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Z)**3) - eWUZb3_95
                x(1) = x(1) + littleQ_3W * Exp(-littleWi * (littleUi - Z)**3) - enWZbU3_95
            Else If (Z .LT. 100._dp) Then !97-100km
                x = xb_97 + GL_Quad_nO1_O2_8c(Z)
                x = x + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Z)**3) - eWUZb3_97
            Else !100-110km
                x = xb_100 + GL_Quad_nO1_O2_8d(Z)
                x = x + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Z)**3) - eWUZb3_100
            End If
        Else !b=9
            If (Z .LT. 115._dp) Then !110-115km
                x = xb(:,9) + GL_Quad_nO1_O2_9a(Z)
                x = x + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Z)**3) - eWUZb3(:,9)
            Else !115-120km
                x = xb_115 + c9ba * (c9bb * (Z-115._dp) / (R_Earth+Z) + Lb(9) * (Log(T(Z,10)/(R_Earth+Z)) + c9bc))
                x = x + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Z)**3) - eWUZb3_115
            End If
        End If
    End If
End Function nO1_O2_powers

Function K95to115(Z) Result(K)
    !computes eddy-diffusion coefficent according to US Standard Atmosphere 1976 equation 7a-c
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: K
    Real(dp), Intent(In) :: Z
    Real(dp) :: x
    
    If (Z .LT. 95._dp) Then
        K = K0  !US Standard Atmosphere 1976 equation 7a
    Else If (Z .LT. 115._dp) Then
        x = (Z - 95._dp)**2
        K = K0 * Exp(-x / (400._dp - x))  !US Standard Atmosphere 1976 equation 7b
    Else
        K = 0._dp  !US Standard Atmosphere 1976 equation 7c
    End If
End Function K95to115

Function Dcoeff_O1_O2(Tz,Z,b) Result(D)
    !computes molecular-diffusion coefficent according to US Standard Atmosphere 1976 equation 8 for O1 and O2
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: D(1:2)
    Real(dp), Intent(In) :: Tz,Z
    Integer, Intent(In) :: b
    
    D = ai(2:3) * (Tz / 273.15_dp)**bi(2:3) / (N7_T7(1) * Exp(-nN2_power(Z,b)) / Tz)  !US Standard Atmosphere 1976 equation 8
End Function Dcoeff_O1_O2

Function nO1_O2_integrand1(Z,b) Result(f)  !for 86 to 95 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    
    Tz = T(Z,b+1)
    D = Dcoeff_O1_O2(Tz,Z,b)
    f = g(Z) * D * (Mi(2:3) + M0*K0/D) / (R_star * Tz * (D + K0))
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
    
    Tz = Teq27(Z)
    D = Dcoeff_O1_O2(Tz,Z,b)
    K = K95to115(Z)
    f = g(Z) * D * (Mi(2:3) + M0*K/D) / (R_star * Tz * (D + K))
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

    Tz = Teq27(Z)
    D = Dcoeff_O1_O2(Tz,Z,b)
    K = K95to115(Z)
    f = g(Z) * D * (Mi(2:3) + M0*K/D) / (R_star * Tz * (D + K))
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
    D = Dcoeff_O1_O2(Tz,Z,b)
    K = K95to115(Z)
    f = g(Z) * D * (Mi(2:3) + Mi(1)*K/D) / (R_star * Tz * (D + K))
End Function nO1_O2_integrand4

Function nO1_O2_integrand5(Z,b) Result(f)  !for 115 to 1000 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b

    f = g(Z) * Mi(2:3) / (R_star * T(Z,b+1))
End Function nO1_O2_integrand5

Function GL_Quad_nO1_O2_8a(z) Result(q)  !for 91 to 95 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 5  !GL-points for 11 digits of precision
    Integer, Parameter :: n0 = 0
    Integer, Parameter :: n1 = 2
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.5688888888888888888888888888888888888888888888888888888888888889_dp, &
                                        & 0.4786286704993664680412915148356381929122955533431415399727276673_dp, &
                                        & 0.2369268850561890875142640407199173626432600022124140155828278882_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                        & 0.5384693101056830910363144207002088049672866069055599562022316271_dp, &
                                        & 0.9061798459386639927976268782993929651256519107625308628737622865_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(8))
    c2 = 0.5_dp * (z+Zb(8))
    q = wi(0) * nO1_O2_integrand1(c2,8)
    Do i = 1,n1
        q = q + wi(i) * (nO1_O2_integrand1(xi(i)*c1 + c2,8) + nO1_O2_integrand1(-xi(i)*c1 + c2,8))
    End Do
    q = q * c1
End Function GL_Quad_nO1_O2_8a

Function GL_Quad_nO1_O2_8b(z) Result(q)  !for 95 to 97 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 4  !GL-points for 11 digits of precision
    Integer, Parameter :: n0 = 1
    Integer, Parameter :: n1 = 2
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.6521451548625461426269360507780005927646513041661064595074706805_dp, &
                                        & 0.3478548451374538573730639492219994072353486958338935404925293195_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.3399810435848562648026657591032446872005758697709143525929539768_dp, &
                                        & 0.8611363115940525752239464888928095050957253796297176376157219209_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-95._dp)
    c2 = 0.5_dp * (z+95._dp)
    q = 0._dp
    Do i = 1,n1
        q = q + wi(i) * (nO1_O2_integrand2(xi(i)*c1 + c2,8) + nO1_O2_integrand2(-xi(i)*c1 + c2,8))
    End Do
    q = q * c1
End Function GL_Quad_nO1_O2_8b

Function GL_Quad_nO1_O2_8c(z) Result(q)  !for 97 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 5  !GL-points for 11 digits of precision
    Integer, Parameter :: n0 = 0
    Integer, Parameter :: n1 = 2
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.5688888888888888888888888888888888888888888888888888888888888889_dp, &
                                        & 0.4786286704993664680412915148356381929122955533431415399727276673_dp, &
                                        & 0.2369268850561890875142640407199173626432600022124140155828278882_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                        & 0.5384693101056830910363144207002088049672866069055599562022316271_dp, &
                                        & 0.9061798459386639927976268782993929651256519107625308628737622865_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-97._dp)
    c2 = 0.5_dp * (z+97._dp)
    q = wi(0) * nO1_O2_integrand3(c2,8)
    Do i = 1,n1
        q = q + wi(i) * (nO1_O2_integrand3(xi(i)*c1 + c2,8) + nO1_O2_integrand3(-xi(i)*c1 + c2,8))
    End Do
    q = q * c1
End Function GL_Quad_nO1_O2_8c

Function GL_Quad_nO1_O2_8d(z) Result(q)  !for 100 to 110 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 15  !GL-points for 11 digits of precision
    Integer, Parameter :: n0 = 0
    Integer, Parameter :: n1 = 7
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.2025782419255612728806201999675193148386621580094773567967041161_dp, &
                                        & 0.1984314853271115764561183264438393248186925599575419934847379279_dp, &
                                        & 0.1861610000155622110268005618664228245062260122779284028154957273_dp, &
                                        & 0.1662692058169939335532008604812088111309001800984129073218651906_dp, &
                                        & 0.1395706779261543144478047945110283225208502753155112432023911286_dp, &
                                        & 0.1071592204671719350118695466858693034155437157581019806870223891_dp, &
                                        & 0.0703660474881081247092674164506673384667080327543307198259072929_dp, &
                                        & 0.0307532419961172683546283935772044177217481448334340742642282855_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                        & 0.2011940939974345223006283033945962078128364544626376796159497246_dp, &
                                        & 0.3941513470775633698972073709810454683627527761586982550311653440_dp, &
                                        & 0.5709721726085388475372267372539106412383863962827496048532654171_dp, &
                                        & 0.7244177313601700474161860546139380096308992945841025635514234207_dp, &
                                        & 0.8482065834104272162006483207742168513662561747369926340957275588_dp, &
                                        & 0.9372733924007059043077589477102094712439962735153044579013630764_dp, &
                                        & 0.9879925180204854284895657185866125811469728171237614899999975156_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-100._dp)
    c2 = 0.5_dp * (z+100._dp)
    q = wi(0) * nO1_O2_integrand4(c2,8)
    Do i = 1,n1
        q = q + wi(i) * (nO1_O2_integrand4(xi(i)*c1 + c2,8) + nO1_O2_integrand4(-xi(i)*c1 + c2,8))
    End Do
    q = q * c1
End Function GL_Quad_nO1_O2_8d

Function GL_Quad_nO1_O2_9a(z) Result(q)  !for 110 to 115 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 18  !GL-points for 11 digits of precision
    Integer, Parameter :: n0 = 1
    Integer, Parameter :: n1 = 9
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.1691423829631435918406564701349866103341058193703438802698751915_dp, &
                                        & 0.1642764837458327229860537764659275904123389539973529532444969709_dp, &
                                        & 0.1546846751262652449254180038363747721932183962673541726666419147_dp, &
                                        & 0.1406429146706506512047313037519472280955024103309725598834561953_dp, &
                                        & 0.1225552067114784601845191268002015552281638973334390971672535137_dp, &
                                        & 0.1009420441062871655628139849248346070628011388876789016103745367_dp, &
                                        & 0.0764257302548890565291296776166365256053179062083582874495213792_dp, &
                                        & 0.0497145488949697964533349462026386416808662461289102022463043079_dp, &
                                        & 0.0216160135264833103133427102664524693876852314755899454620759901_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.0847750130417353012422618529357838117333173869060899200433645176_dp, &
                                        & 0.2518862256915055095889728548779112301628617656596404580202710317_dp, &
                                        & 0.4117511614628426460359317938330516370789896821200255112811488678_dp, &
                                        & 0.5597708310739475346078715485253291369276264857707094166399869441_dp, &
                                        & 0.6916870430603532078748910812888483894522705728175077589021626568_dp, &
                                        & 0.8037049589725231156824174550145907971032989216119224817504280642_dp, &
                                        & 0.8926024664975557392060605911271455154078952713522982141874663149_dp, &
                                        & 0.9558239495713977551811958929297763099728441348113064788453876297_dp, &
                                        & 0.9915651684209309467300160047061507702525789368454396929196756302_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(9))
    c2 = 0.5_dp * (z+Zb(9))
    q = 0._dp
    Do i = 1,n1
        q = q + wi(i) * (nO1_O2_integrand4(xi(i)*c1 + c2,9) + nO1_O2_integrand4(-xi(i)*c1 + c2,9))
    End Do
    q = q * c1
End Function GL_Quad_nO1_O2_9a

Function nAr_He_powers(Z,b) Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp), Parameter :: xb(1:2,8:10) = Reshape( (/  0.9029433887519960_dp, &     !Ar, Z = 91km
                                                    &  0.7963213747812479_dp, &     !He, Z = 91km
                                                    &  4.6108383812648741_dp, &     !Ar, Z = 110km
                                                    &  2.3167238395822887_dp, &     !He, Z = 110km
                                                    &  6.2412684806342682_dp, &     !Ar, Z = 120km
                                                    &  2.3147895407888002_dp  /), & !He, Z = 120km
                                                 & (/2,3/) )
    Real(dp), Parameter :: xb_95(1:2) =  (/ 1.6463776323299726_dp, &  !Ar, Z = 95km
                                          & 1.3378385484034478_dp  /) !He, Z = 95km
    Real(dp), Parameter :: xb_97(1:2) =  (/ 2.0274784607584775_dp, &  !Ar, Z = 97km
                                          & 1.5670572447696414_dp  /) !He, Z = 97km
    Real(dp), Parameter :: xb_100(1:2) = (/ 2.6119420164401124_dp, &  !Ar, Z = 100km
                                          & 1.8579919646718013_dp  /) !He, Z = 100km
    Real(dp), Parameter :: xb_115(1:2) = (/ 5.5159366790400775_dp, &  !Ar, Z = 115km
                                          & 2.3185678009191539_dp  /) !He, Z = 115km
    Logical, Parameter :: no_sublayers(7:10) = (/ .TRUE.,  &
                                                & .FALSE., &
                                                & .FALSE., &
                                                & .TRUE.   /)
    Real(dp), Parameter :: bigQ_3W(1:2) = -bigQi(4:5) / (3._dp * bigWi(4:5))
    Real(dp), Parameter :: eWUZb3(1:2,7:10) = Reshape( (/ bigQ_3W(1) * Exp(bigWi(4) * (bigUi(4) - Zb(7))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(5) * (bigUi(5) - Zb(7))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(4) * (bigUi(4) - Zb(8))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(5) * (bigUi(5) - Zb(8))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(4) * (bigUi(4) - Zb(9))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(5) * (bigUi(5) - Zb(9))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(4) * (bigUi(4) - Zb(10))**3), &
                                                        & bigQ_3W(2) * Exp(bigWi(5) * (bigUi(5) - Zb(10))**3)  /), &
                                                     & (/2,4/) )
    Real(dp), Parameter :: rho_star_Ar_He(1:2) = Mi(4:5) / R_star
    !precomputed parameters for b=8b
    Real(dp), Parameter :: eWUZb3_95(1:2) = bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - 95._dp)**3)
    !precomputed parameters for b=8c
    Real(dp), Parameter :: eWUZb3_97(1:2) = bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - 97._dp)**3)
    !precomputed parameters for b=8d
    Real(dp), Parameter :: eWUZb3_100(1:2) = bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - 100._dp)**3)
    !precomputed parameters for b=9b
    Real(dp), Parameter :: R_Z115    = R_Earth + 115._dp    
    Real(dp), Parameter :: T115      = Tb(9) + Lb(9)*(115._dp - Zb(9))
    Real(dp), Parameter :: Tb9_Lb9R9 = Tb(9) - Lb(9)*R_Z9
    Real(dp), Parameter :: c9ba(1:2) = rho_star_Ar_He * g0 * (R_Earth / Tb9_Lb9R9)**2
    Real(dp), Parameter :: c9bb      = Tb9_Lb9R9 / R_Z115
    Real(dp), Parameter :: c9bc      = Log(R_Z115 / T115)
    Real(dp), Parameter :: eWUZb3_115(1:2) = bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - 115._dp)**3)
    Real(dp), Parameter :: c9bd      = alphaHe * Log(T115)
    !precomputed parameters for b=10
    Real(dp), Parameter :: c10a(1:2) = rho_star_Ar_He * g0 * (R_Earth/R_Z10)**2 / (T_inf * lambda)
    Real(dp), Parameter :: c10b      = -lambda * R_Z10**2
    Real(dp), Parameter :: c10c      = lambda * R_Z10 - Log(Tb(10))
    Real(dp), Parameter :: c10d      = alphaHe * Log(Tb(10))

    If (no_sublayers(b)) Then !b=7,10
        If (b .EQ. 7) Then !b=7
            x = GL_Quad_nAr_He_7_8a(Z,7)
            x = x + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Z)**3) - eWUZb3(:,7)
        Else !b=10
            x = xb(:,10) + c10a * (Log(T(Z,11)) + c10b / (R_Earth + Z) + c10c)
            x = x + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Z)**3) - eWUZb3(:,10)
            x(2) = x(2) + alphaHe * Log(T(Z,11)) - c10d
        End If
    Else !b=8,9
        If (b .EQ. 8) Then !b=8
            If (Z .LT. 95._dp) Then !91-95km
                x = xb(:,8) + GL_Quad_nAr_He_7_8a(Z,8)
                x = x + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Z)**3) - eWUZb3(:,8)
            Else If (Z .LT. 97._dp) Then !95-97km
                x = xb_95 + GL_Quad_nAr_He_8b(Z)
                x = x + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Z)**3) - eWUZb3_95
            Else If (Z .LT. 100._dp) Then !97-100km
                x = xb_97 + GL_Quad_nAr_He_8c(Z)
                x = x + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Z)**3) - eWUZb3_97
            Else !100-110km
                x = xb_100 + GL_Quad_nAr_He_8d(Z)
                x = x + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Z)**3) - eWUZb3_100
            End If
        Else !b=9
            If (Z .LT. 115._dp) Then !110-115km
                x = xb(:,9) + GL_Quad_nAr_He_9a(Z)
                x = x + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Z)**3) - eWUZb3(:,9)
            Else !115-120km
                x = xb_115 + c9ba * (c9bb * (Z-115._dp) / (R_Earth+Z) + Lb(9) * (Log(T(Z,10)/(R_Earth+Z)) + c9bc))
                x = x + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Z)**3) - eWUZb3_115
                x(2) = x(2) + alphaHe * Log(T(Z,10)) - c9bd
            End If
        End If
    End If
End Function nAr_He_powers

Function Dcoeff_Ar_He(Tz,Z,b,Nb_out) Result(D)
    !computes molecular-diffusion coefficent according to US Standard Atmosphere 1976 equation 8 for Ar and He
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: D(1:2)
    Real(dp), Intent(In) :: Tz,Z
    Integer, Intent(In) :: b
    Real(dp), Intent(Out), Optional :: Nb_out(1:3)
    Real(dp) :: Nb(1:3)
    
    Nb(1) = nN2_power(Z,b)
    Nb(2:3) = nO1_O2_powers(Z,b)
    Nb = N7_T7(1:3) * Exp(-Nb) / Tz
    D = ai(4:5) * (Tz / 273.15_dp)**bi(4:5) / Sum(Nb)  !US Standard Atmosphere 1976 equation 8
    If (Present(Nb_out)) Nb_out = Nb
End Function Dcoeff_Ar_He

Function nAr_He_integrand1(Z,b) Result(f)  !for 86 to 95 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    Real(dp) :: y(1:2)
    
    Tz = T(Z,b+1)
    D = Dcoeff_Ar_He(Tz,Z,b)
    y = D / (R_star * Tz * (D + K0))
    f = g(Z) * y * (Mi(4:5) + M0*K0/D)
    f(2) = f(2) + y(2) * alphaHe_star * dT_dZ(Z,b+1)
End Function nAr_He_integrand1

Function nAr_He_integrand2(Z,b) Result(f)  !for 95 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: Tz
    Real(dp) :: D(1:2)
    Real(dp) :: y(1:2)
    Real(dp) :: K
    
    Tz = Teq27(Z)
    D = Dcoeff_Ar_He(Tz,Z,b)
    K = K95to115(Z)
    y = D / (R_star * Tz * (D + K))
    f = g(Z) * y * (Mi(4:5) + M0*K/D)
    f(2) = f(2) + y(2) * alphaHe_star * dT_dZ(Z,b+1)
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
    D = Dcoeff_Ar_He(Tz,Z,b,Nb_out=Nb)
    K = K95to115(Z)
    y = D / (R_star * Tz * (D + K))
    f = g(Z) * y * (Mi(4:5) + (Sum(Nb*Mi(1:3))/Sum(Nb))*K/D)
    f(2) = f(2) + y(2) * alphaHe_star * dT_dZ(Z,b+1)
End Function nAr_He_integrand4

Function nAr_He_integrand5(Z,b) Result(f)  !for 115 to 1000 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f(1:2)
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp) :: y

    y = 1._dp / (R_star * T(Z,b+1))
    f = g(Z) * y * Mi(4:5)
End Function nAr_He_integrand5

Function GL_Quad_nAr_He_7_8a(z,b) Result(q)  !for 86 to 91 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    Integer, Intent(In) :: b
    !Integer, Parameter :: n = 5  !GL-points for 10 digits of precision
    Integer, Parameter :: n0 = 0
    Integer, Parameter :: n1 = 2
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.5688888888888888888888888888888888888888888888888888888888888889_dp, &
                                        & 0.4786286704993664680412915148356381929122955533431415399727276673_dp, &
                                        & 0.2369268850561890875142640407199173626432600022124140155828278882_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                        & 0.5384693101056830910363144207002088049672866069055599562022316271_dp, &
                                        & 0.9061798459386639927976268782993929651256519107625308628737622865_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(b))
    c2 = 0.5_dp * (z+Zb(b))
    q = wi(0) * nAr_He_integrand1(c2,b)
    Do i = 1,n1
        q = q + wi(i) * (nAr_He_integrand1(xi(i)*c1 + c2,b) + nAr_He_integrand1(-xi(i)*c1 + c2,b))
    End Do
    q = q * c1
End Function GL_Quad_nAr_He_7_8a

Function GL_Quad_nAr_He_8b(z) Result(q)  !for 95 to 97 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 4  !GL-points for 10 digits of precision
    Integer, Parameter :: n0 = 1
    Integer, Parameter :: n1 = 2
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.6521451548625461426269360507780005927646513041661064595074706805_dp, &
                                        & 0.3478548451374538573730639492219994072353486958338935404925293195_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.3399810435848562648026657591032446872005758697709143525929539768_dp, &
                                        & 0.8611363115940525752239464888928095050957253796297176376157219209_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-95._dp)
    c2 = 0.5_dp * (z+95._dp)
    q = 0._dp
    Do i = 1,n1
        q = q + wi(i) * (nAr_He_integrand2(xi(i)*c1 + c2,8) + nAr_He_integrand2(-xi(i)*c1 + c2,8))
    End Do
    q = q * c1
End Function GL_Quad_nAr_He_8b

Function GL_Quad_nAr_He_8c(z) Result(q)  !for 97 to 100 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 4  !GL-points for 10 digits of precision
    Integer, Parameter :: n0 = 1
    Integer, Parameter :: n1 = 2
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.6521451548625461426269360507780005927646513041661064595074706805_dp, &
                                        & 0.3478548451374538573730639492219994072353486958338935404925293195_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.3399810435848562648026657591032446872005758697709143525929539768_dp, &
                                        & 0.8611363115940525752239464888928095050957253796297176376157219209_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-97._dp)
    c2 = 0.5_dp * (z+97._dp)
    q = 0._dp
    Do i = 1,n1
        q = q + wi(i) * (nAr_He_integrand2(xi(i)*c1 + c2,8) + nAr_He_integrand2(-xi(i)*c1 + c2,8))
    End Do
    q = q * c1
End Function GL_Quad_nAr_He_8c

Function GL_Quad_nAr_He_8d(z) Result(q)  !for 100 to 110 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 17  !GL-points for 10 digits of precision
    Integer, Parameter :: n0 = 0
    Integer, Parameter :: n1 = 8
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.1794464703562065254582656442618856214487803198976685236676686980_dp, &
                                        & 0.1765627053669926463252709901131972391509244180007481180431444069_dp, &
                                        & 0.1680041021564500445099706637883231550211981289650740142699558513_dp, &
                                        & 0.1540457610768102880814315948019586119404830584710179343852647114_dp, &
                                        & 0.1351363684685254732863199817023501973721258532344890203779946107_dp, &
                                        & 0.1118838471934039710947883856263559267358434242630770500184864824_dp, &
                                        & 0.0850361483171791808835353701910620738504913892185054757664103607_dp, &
                                        & 0.0554595293739872011294401653582446605128462519532288469937240787_dp, &
                                        & 0.0241483028685479319601100262875653246916973159450252783111851488_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                        & 0.1784841814958478558506774936540655574754193326915256435629518143_dp, &
                                        & 0.3512317634538763152971855170953460050405397515756750233191610195_dp, &
                                        & 0.5126905370864769678862465686295518745829237224111729059127314990_dp, &
                                        & 0.6576711592166907658503022166430023351478058914759732438052316955_dp, &
                                        & 0.7815140038968014069252300555204760502239724727405685125133145355_dp, &
                                        & 0.8802391537269859021229556944881556926234168179344279003519101593_dp, &
                                        & 0.9506755217687677612227169578958030214433850465591087076699692124_dp, &
                                        & 0.9905754753144173356754340199406652765077898504595643027839087867_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-100._dp)
    c2 = 0.5_dp * (z+100._dp)
    q = wi(0) * nAr_He_integrand4(c2,8)
    Do i = 1,n1
        q = q + wi(i) * (nAr_He_integrand4(xi(i)*c1 + c2,8) + nAr_He_integrand4(-xi(i)*c1 + c2,8))
    End Do
    q = q * c1
End Function GL_Quad_nAr_He_8d

Function GL_Quad_nAr_He_9a(z) Result(q)  !for 110 to 115 km
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: q(1:2)    !the result of the integration
    Real(dp), Intent(In) :: z    !limit of integration
    !Integer, Parameter :: n = 23  !GL-points for 10 digits of precision
    Integer, Parameter :: n0 = 0
    Integer, Parameter :: n1 = 11
    Real(dp), Parameter :: wi(n0:n1) = (/ 0.1336545721861061753514571105458443385831528076368833174819359780_dp, &
                                        & 0.1324620394046966173716424647033169258050356694742235243225122680_dp, &
                                        & 0.1289057221880821499785953393997936532597154971917834924443404317_dp, &
                                        & 0.1230490843067295304675784006720096548158528125464705744356703329_dp, &
                                        & 0.1149966402224113649416435129339613014914105229295856066090757034_dp, &
                                        & 0.1048920914645414100740861850147438548584715831939750055028251923_dp, &
                                        & 0.0929157660600351474770186173697646486034600717431298937326595046_dp, &
                                        & 0.0792814117767189549228925247420432269137119919384065025556345403_dp, &
                                        & 0.0642324214085258521271696151589109980391582757548068304953466821_dp, &
                                        & 0.0480376717310846685716410716320339965612163083035116113336836669_dp, &
                                        & 0.0309880058569794443106942196418845053837725289999280664348878066_dp, &
                                        & 0.0134118594871417720813094934586150649766183341057372333923958821_dp /)
    Real(dp), Parameter :: xi(n0:n1) = (/ 0.0000000000000000000000000000000000000000000000000000000000000000_dp, &
                                        & 0.1332568242984661109317426822417661370104052762533821565109453772_dp, &
                                        & 0.2641356809703449305338695382833096029790132501941396048690788771_dp, &
                                        & 0.3903010380302908314214888728806054585780508506925034812192052633_dp, &
                                        & 0.5095014778460075496897930478668464305448427691848576232271893808_dp, &
                                        & 0.6196098757636461563850973116495956533871806588070922957008655856_dp, &
                                        & 0.7186613631319501944616244837486188483299297451312927957547812882_dp, &
                                        & 0.8048884016188398921511184069967785579414301397303080225854324357_dp, &
                                        & 0.8767523582704416673781568859341456716389290299606506349094011158_dp, &
                                        & 0.9329710868260161023491969890384229782357018201513907706929012546_dp, &
                                        & 0.9725424712181152319560240768207773751816137953539739424912561587_dp, &
                                        & 0.9947693349975521235239257154455743605736273724588704209267938842_dp /)
    Real(dp) :: c1,c2  !changes limits of integration from (a,b) to (-1,1)
    Integer :: i

    c1 = 0.5_dp * (z-Zb(9))
    c2 = 0.5_dp * (z+Zb(9))
    q = wi(0) * nAr_He_integrand4(c2,9)
    Do i = 1,n1
        q = q + wi(i) * (nAr_He_integrand4(xi(i)*c1 + c2,9) + nAr_He_integrand4(-xi(i)*c1 + c2,9))
    End Do
    q = q * c1
End Function GL_Quad_nAr_He_9a

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

 Function nH(Z) Result(N)
    Use Kinds, Only: dp
    Use Quadratures, Only: Romberg_Quad
    Implicit None
    Real(dp) :: N
    Real(dp), Intent(In) :: Z
    
    If (Z .LT. 150._dp) Then
        N = 0._dp
    Else If (Z .LT. 500._dp) Then
        N = (nH500 - phiH * Romberg_Quad(nH_integrand,500._dp,Z,0._dp,rTol_tier4b)) / p6(Z)  !US Standard Atmosphere 1976 equation 39
    Else !z .GE. 500
        N = nH500 / p6(Z)  !US Standard Atmosphere 1976 equation 39
    End If
End Function nH

Function p6(Z) Result(p)
    Use Kinds, Only: dp
    Use Quadratures, Only: Romberg_Quad
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: Z
    Real(dp), Parameter :: T500 = 999.2356017626150686_dp
    
    p = Sqrt(Sqrt((T(Z,11) / T500)**3)) * Exp(Romberg_Quad(p6_integrand,500._dp,Z,0._dp,rTol_tier4a))
End Function p6

Function nH_integrand(Z) Result(f)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: Z
    Real(dp) :: Tz
    Real(dp) :: Nb(1:5)
    Real(dp) :: D_inv
    
    Tz = T(Z,11)
    Nb(1) =   nN2_power(Z,10)
    Nb(2:3) = nO1_O2_powers(Z,10)
    Nb(4:5) = nAr_He_powers(Z,10)
    Nb = N7_T7 * Exp(-Nb) / Tz
    D_inv = Sum(Nb) / (ai(6) * Sqrt(Tz / 273.15_dp))
    f = p6(Z) * D_inv
End Function nH_integrand

Function p6_integrand(Z) Result(p)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: Z
    Real(dp) :: Tz
    Real(dp) :: Nb(1:5)
    
    Tz = T(Z,11)
    Nb(1) =   nN2_power(Z,10)
    Nb(2:3) = nO1_O2_powers(Z,10)
    Nb(4:5) = nAr_He_powers(Z,10)
    Nb = N7_T7 * Exp(-Nb) / Tz
    p = ( Sum(Nb*Mi(1:5)) / Sum(Nb) ) * g(Z) / (R_star * Tz)  !US Standard Atmosphere 1976 equation 40
End Function p6_integrand

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
    If (Lb_nonzero(b)) Then !b=0,2,3,5,6,9
        If (T_linear_by_H(b)) Then !b=0,2,3,5,6
            Tz = Teq23(Z,b)  !US Standard Atmosphere 1976 equation 23
            !NOTE: Tz is molecular temperature for altitudes below 86km
            P = Peq33a(Tz,b)  !US Standard Atmosphere 1976 equation 33a
        Else !b=9
            Tz = Teq29(Z)  !US Standard Atmosphere 1976 equation 29
            Call rho_N(Z,Tz,b,N)
            P = Peq33c(N,Tz)  !US Standard Atmosphere 1976 equation 33c
        End If
    Else If (T_exponential(b)) Then !b=10
        Tz = Teq31(Z)  !US Standard Atmosphere 1976 equation 31
        Call rho_N(Z,Tz,b,N)
        P = Peq33c(N,Tz)  !US Standard Atmosphere 1976 equation 33c
    Else If (T_elliptical(b)) Then !b=8
        Tz = Teq27(Z)  !US Standard Atmosphere 1976 equation 27
        Call rho_N(Z,Tz,b,N)
        P = Peq33c(N,Tz)  !US Standard Atmosphere 1976 equation 33c
    Else !b=1,4,7  zero lapse rate
        Tz = Tb(b)
        If (P_rho_not_by_N(b)) Then !b=1,4
            P = Peq33b(Z,b)  !US Standard Atmosphere 1976 equation 33b
        Else  !b=7
            Call rho_N(Z,Tz,b,N)
            P = Peq33c(N,Tz)  !US Standard Atmosphere 1976 equation 33c
        End If
    End If
End Function P

Function Peq33a(Tz,b)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Peq33a
    Real(dp), Intent(In) :: Tz
    Integer, Intent(In) :: b
    Real(dp), Parameter :: L_star = g0 * M0 / R_star  !precomputed quantity for 1976 pressure calculations
    Real(dp), Parameter :: L_star_Lb(0:7) = (/  L_star / Lb(0), &  !precomputed quantity for 1976 pressure calculations
                                             & -1._dp,          & 
                                             &  L_star / Lb(2), & 
                                             &  L_star / Lb(3), & 
                                             & -1._dp,          & 
                                             &  L_star / Lb(5), & 
                                             &  L_star / Lb(6), &
                                             & -1._dp           /)
    Real(dp), Parameter :: Pb_Tb_L_star_Lb(0:7) = (/  Pb(0) * Tb(0)**L_star_Lb(0), &  !precomputed quantity for 1976 pressure calculations
                                                   & -1._dp,                       & 
                                                   &  Pb(2) * Tb(2)**L_star_Lb(2), & 
                                                   &  Pb(3) * Tb(3)**L_star_Lb(3), & 
                                                   & -1._dp,                       & 
                                                   &  Pb(5) * Tb(5)**L_star_Lb(5), & 
                                                   &  Pb(6) * Tb(6)**L_star_Lb(6), &
                                                   & -1._dp                        /)

    Peq33a = Pb_Tb_L_star_Lb(b) * Tz**(-L_star_Lb(b))  !US Standard Atmosphere 1976 equation 33a
End Function Peq33a

Function Peq33b(Z,b)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Peq33b
    Real(dp), Intent(In) :: Z
    Integer, Intent(In) :: b
    Real(dp), Parameter :: L_star = g0 * M0 / R_star  !precomputed quantity for 1976 pressure calculations
    Real(dp), Parameter :: L_star_Tb(0:7) = (/ -1._dp,          &  !precomputed quantity for 1976 pressure calculations
                                             & -L_star / Tb(1), & 
                                             & -1._dp,          & 
                                             & -1._dp,          & 
                                             & -L_star / Tb(4), & 
                                             & -1._dp,          & 
                                             & -1._dp,          & 
                                             & -1._dp           /)
    
    Peq33b = Pb(b) * Exp( L_star_Tb(b) * (Z_to_H(Z) - Hb(b)) )  !US Standard Atmosphere 1976 equation 33b
End Function Peq33b

Function Peq33c(N,Tz)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Peq33c
    Real(dp), Intent(In) :: N,Tz
    Real(dp), Parameter :: N_star = R_star / Na
    
    Peq33c = N * Tz * N_star  !US Standard Atmosphere 1976 equation 33c
End Function Peq33c

Function rho(Z,layer,layer_range)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: rho  ![g/m^3]
    Real(dp), Intent(In) :: Z ![km]
    Integer, Intent(In), Optional :: layer
    Integer, Intent(In), Optional :: layer_range(1:3)
    Real(dp) :: Tz
    Real(dp) :: Pz
    Real(dp) :: N(1:5)!(1:6)
    Integer :: b
    
    !find atmospheric base layer
    If (Present(layer)) Then
        b = layer - 1
    Else If (Present(layer_range)) Then
        b = Find_Base_Layer(Z,layer_range)
    Else
        b = Find_Base_Layer(Z)
    End If
    If (Lb_nonzero(b)) Then !b=0,2,3,5,6,9
        If (T_linear_by_H(b)) Then !b=0,2,3,5,6
            Tz = Teq23(Z,b)  !US Standard Atmosphere 1976 equation 23
            !NOTE: Tz is molecular temperature for altitudes below 86km
            Pz = Peq33a(Tz,b)  !US Standard Atmosphere 1976 equation 33a
            rho = rhoeq42_1(Tz,Pz)  !US Standard Atmosphere 1976 equation 42-1
        Else !b=9
            Tz = Teq29(Z)  !US Standard Atmosphere 1976 equation 29
            Call rho_N(Z,Tz,b,N)
            rho = rhoeq42_3(N)  !US Standard Atmosphere 1976 equation 42-3
        End If
    Else If (T_exponential(b)) Then !b=10
        Tz = Teq31(Z)  !US Standard Atmosphere 1976 equation 31
        Call rho_N(Z,Tz,b,N)
        rho = rhoeq42_3(N)  !US Standard Atmosphere 1976 equation 42-3
    Else If (T_elliptical(b)) Then !b=8
        Tz = Teq27(Z)  !US Standard Atmosphere 1976 equation 27
        Call rho_N(Z,Tz,b,N)
        rho = rhoeq42_3(N)  !US Standard Atmosphere 1976 equation 42-3
    Else !b=1,4,7  zero lapse rate
        Tz = Tb(b)
        If (P_rho_not_by_N(b)) Then !b=1,4
            Pz = Peq33b(Z,b)  !US Standard Atmosphere 1976 equation 33b
            rho = rhoeq42_1(Tz,Pz)  !US Standard Atmosphere 1976 equation 42-1
        Else !b=7
            Call rho_N(Z,Tz,b,N)
            rho = rhoeq42_3(N)  !US Standard Atmosphere 1976 equation 42-3
        End If
    End If
End Function rho

Function rhoeq42_1(Tz,Pz)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: rhoeq42_1
    Real(dp), Intent(In) :: Tz,Pz
    
    rhoeq42_1 = Pz * rho_star /  Tz  !US Standard Atmosphere 1976 equation 42-1
End Function rhoeq42_1

Function rhoeq42_3(N)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: rhoeq42_3
    Real(dp), Intent(In) :: N(1:5)
    Real(dp), Parameter :: inv_Na_kg2g = 1000._dp / Na  !1/Na multiplied by conversion for kg to g
    
    rhoeq42_3 = Sum(N * Mi(1:5)) * inv_Na_kg2g  !US Standard Atmosphere 1976 equation 42-3
End Function rhoeq42_3

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
    Use Quadratures, Only: Romberg_Quad
    Implicit None
    Real(dp) :: xb(1:5,1:2)
    Real(dp), Parameter :: Zs(1:5) = (/  86._dp, &
                                      &  91._dp, & 
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 120._dp  /)
    Real(dp), Parameter :: M_over_R(1:5) = (/ rho_star, &
                                            & rho_star, &
                                            & Mi(1) / R_star, &
                                            & Mi(1) / R_star, &
                                            & Mi(1) / R_star  /)
    Integer, Parameter :: bs(1:5) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  9, & 
                                     & 10  /)
    Integer :: i
    Real(dp), Parameter :: rho_star_N2 = Mi(1) / R_star
    !Precomputed parameters for b = 7
    Real(dp), Parameter :: c7 = rho_star * g0 * R_Earth * (R_Earth/R_Z7) / Tb(7)
    !precomputed parameters for b=9
    Real(dp), Parameter :: c9a = rho_star_N2 * g0 * (R_Earth / (Tb(9) - Lb(9)*R_Z9))**2
    Real(dp), Parameter :: c9b = -Log(Tb(9)/R_Z9)
    Real(dp), Parameter :: c9c = (Lb(9)*R_Z9 - Tb(9)) / R_Z9

    xb(:,1) = Zs
    xb(:,2) = 0._dp
    i = 2
    xb(i,2) = xb(i-1,2) + c7 * (Zb(8) - Zb(7)) / (R_Earth + Zb(8)) !up to 91km
    i = 3
    xb(i,2) = xb(i-1,2) + M_over_R(i-1) * Romberg_Quad(nN2_integrand_no_b,Zs(i-1),Zs(i),0._dp,rTol_tier1) !up to 100 km
    i = 4
    xb(i,2) = xb(i-1,2) + M_over_R(i-1) * Romberg_Quad(nN2_integrand_no_b,Zs(i-1),Zs(i),0._dp,rTol_tier1) !up to 110 km
    i = 5
    xb(i,2) = xb(i-1,2) + c9a * ( Lb(9) * (Log(T(Zb(10),10)/(R_Earth+Zb(10))) + c9b) - c9c * (Zb(10)-Zb(9)) / (R_Earth+Zb(10)) ) !up to 120 km
End Function nN2_power_stops

Function nO1_O2_power_stops() Result(xb)
    Use Kinds, Only: dp
    Use Quadratures, Only: Romberg_Quad
    Implicit None
    Real(dp) :: xb(1:8,1:3)
    Integer :: i
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
    Real(dp), Parameter :: bigQ_3W(1:2) = -bigQi(2:3) / (3._dp * bigWi(2:3))
    Real(dp), Parameter :: eWUZb3(1:2,7:10) = Reshape( (/ bigQ_3W(1) * Exp(bigWi(2) * (bigUi(2) - Zb(7))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(3) * (bigUi(3) - Zb(7))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(2) * (bigUi(2) - Zb(8))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(3) * (bigUi(3) - Zb(8))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(2) * (bigUi(2) - Zb(9))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(3) * (bigUi(3) - Zb(9))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(2) * (bigUi(2) - Zb(10))**3), &
                                                        & bigQ_3W(2) * Exp(bigWi(3) * (bigUi(3) - Zb(10))**3)  /), &
                                                      & (/2,4/) )
    Real(dp), Parameter :: littleQ_3W = littleQi / (3._dp * littleWi)
    Real(dp), Parameter :: enWZbU3(7:8) = (/ littleQ_3W * Exp(-littleWi * (littleUi - Zb(7))**3), &
                                             littleQ_3W * Exp(-littleWi * (littleUi - Zb(8))**3)  /)
    Real(dp), Parameter :: rho_star_O1_O2(1:2) = Mi(2:3) / R_star
    !precomputed parameters for b=7
    Real(dp), Parameter :: c7a      = rho_star * g0 * R_Earth * (R_Earth / R_Z7) / Tb(7)
    Real(dp), Parameter :: c7b(1:2) = (M0 - Mi(2:3)) / M0
    Real(dp), Parameter :: c7c      = K0 * N7(1)
    Real(dp), Parameter :: c7d(1:2) = ai(2:3) * (Tb(7)/273.15_dp)**bi(2:3)
    Real(dp), Parameter :: c7e(1:2) = Log(c7c + c7d)
    !precomputed parameters for b=8b
    Real(dp), Parameter :: eWUZb3_95(1:2) = bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - 95._dp)**3)
    Real(dp), Parameter :: enWZbU3_95     = littleQ_3W * Exp(-littleWi * (littleUi - 95._dp)**3)
    !precomputed parameters for b=8c
    Real(dp), Parameter :: eWUZb3_97(1:2) = bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - 97._dp)**3)
    !precomputed parameters for b=8d
    Real(dp), Parameter :: eWUZb3_100(1:2) = bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - 100._dp)**3)
    !precomputed parameters for b=9b
    Real(dp), Parameter :: R_Z115    = R_Earth + 115._dp    
    Real(dp), Parameter :: T115      = Tb(9) + Lb(9)*(115._dp - Zb(9))
    Real(dp), Parameter :: Tb9_Lb9R9 = Tb(9) - Lb(9)*R_Z9
    Real(dp), Parameter :: c9ba(1:2) = rho_star_O1_O2 * g0 * (R_Earth / Tb9_Lb9R9)**2
    Real(dp), Parameter :: c9bb      = Tb9_Lb9R9 / R_Z115
    Real(dp), Parameter :: c9bc      = Log(R_Z115 / T115)
    Real(dp), Parameter :: eWUZb3_115(1:2) = bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - 115._dp)**3)
    !precomputed parameters for b=10
    Real(dp), Parameter :: c10a(1:2) = rho_star_O1_O2 * g0 * (R_Earth/R_Z10)**2 / (T_inf * lambda)
    Real(dp), Parameter :: c10b      = -lambda * R_Z10**2
    Real(dp), Parameter :: c10c      = lambda * R_Z10 - Log(Tb(10))

    xb(:,1) = Zs
    xb(:,2:3) = 0._dp
    i = 2  !up to 91km
    xb(i,2:3) = xb(i-1,2:3) +  c7a * (Zs(i) - Zb(7)) / (R_Earth + Zs(i)) + c7b * (c7e - Log(c7c + c7d*Exp(nN2_power(Zs(i),7))))
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Zs(i))**3) - eWUZb3(:,7)
    xb(i,2) = xb(i,2) + littleQ_3W * Exp(-littleWi * (littleUi - Zs(i))**3) - enWZbU3(7)
    i = 3  !up to 95km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nO1_O2_integrand1_1,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nO1_O2_integrand1_2,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Zs(i))**3) - eWUZb3(:,8)
    xb(i,2) = xb(i,2) + littleQ_3W * Exp(-littleWi * (littleUi - Zs(i))**3) - enWZbU3(8)
    i = 4  !up to 97km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nO1_O2_integrand2_1,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nO1_O2_integrand2_2,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Zs(i))**3) - eWUZb3_95
    xb(i,2) = xb(i,2) + littleQ_3W * Exp(-littleWi * (littleUi - Zs(i))**3) - enWZbU3_95
    i = 5  !up to 100km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nO1_O2_integrand3_1,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nO1_O2_integrand3_2,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Zs(i))**3) - eWUZb3_97
    i = 6  !up to 110km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nO1_O2_integrand4_1,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nO1_O2_integrand4_2,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Zs(i))**3) - eWUZb3_100
    i = 7  !up to 115km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nO1_O2_integrand4_1,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nO1_O2_integrand4_2,Zs(i-1),Zs(i),0._dp,rTol_tier2)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Zs(i))**3) - eWUZb3(:,9)
    i = 8  !up to 120km
    xb(i,2:3) = xb(i-1,2:3) +  c9ba * (c9bb * (Zs(i)-115._dp) / (R_Earth+Zs(i)) + Lb(9) * (Log(T(Zs(i),10)/(R_Earth+Zs(i))) + c9bc))
    xb(i,2:3) = xb(i,2:3) +  bigQ_3W * Exp(bigWi(2:3) * (bigUi(2:3) - Zs(i))**3) - eWUZb3_115
End Function nO1_O2_power_stops

Function nAr_He_power_stops() Result(xb)
    Use Kinds, Only: dp
    Use Quadratures, Only: Romberg_Quad
    Implicit None
    Real(dp) :: xb(1:8,1:3)
    Integer :: i
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
    Real(dp), Parameter :: bigQ_3W(1:2) = -bigQi(4:5) / (3._dp * bigWi(4:5))
    Real(dp), Parameter :: eWUZb3(1:2,7:10) = Reshape( (/ bigQ_3W(1) * Exp(bigWi(4) * (bigUi(4) - Zb(7))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(5) * (bigUi(5) - Zb(7))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(4) * (bigUi(4) - Zb(8))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(5) * (bigUi(5) - Zb(8))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(4) * (bigUi(4) - Zb(9))**3),  &
                                                        & bigQ_3W(2) * Exp(bigWi(5) * (bigUi(5) - Zb(9))**3),  &
                                                        & bigQ_3W(1) * Exp(bigWi(4) * (bigUi(4) - Zb(10))**3), &
                                                        & bigQ_3W(2) * Exp(bigWi(5) * (bigUi(5) - Zb(10))**3)  /), &
                                                      & (/2,4/) )
    Real(dp), Parameter :: rho_star_Ar_He(1:2) = Mi(4:5) / R_star
    !precomputed parameters for b=8b
    Real(dp), Parameter :: eWUZb3_95(1:2) = bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - 95._dp)**3)
    !precomputed parameters for b=8c
    Real(dp), Parameter :: eWUZb3_97(1:2) = bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - 97._dp)**3)
    !precomputed parameters for b=8d
    Real(dp), Parameter :: eWUZb3_100(1:2) = bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - 100._dp)**3)
    !precomputed parameters for b=9b
    Real(dp), Parameter :: R_Z115    = R_Earth + 115._dp    
    Real(dp), Parameter :: T115      = Tb(9) + Lb(9)*(115._dp - Zb(9))
    Real(dp), Parameter :: Tb9_Lb9R9 = Tb(9) - Lb(9)*R_Z9
    Real(dp), Parameter :: c9ba(1:2) = rho_star_Ar_He * g0 * (R_Earth / Tb9_Lb9R9)**2
    Real(dp), Parameter :: c9bb      = Tb9_Lb9R9 / R_Z115
    Real(dp), Parameter :: c9bc      = Log(R_Z115 / T115)
    Real(dp), Parameter :: eWUZb3_115(1:2) = bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - 115._dp)**3)
    Real(dp), Parameter :: c9bd      = alphaHe * Log(T115)
    
    xb(:,1) = Zs
    xb(:,2:3) = 0._dp
    i = 2  !up to 91km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nAr_He_integrand1_1,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nAr_He_integrand1_2,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Zs(i))**3) - eWUZb3(:,7)
    i = 3  !up to 95km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nAr_He_integrand1_1,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nAr_He_integrand1_2,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Zs(i))**3) - eWUZb3(:,8)
    i = 4  !up to 97km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nAr_He_integrand2_1,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nAr_He_integrand2_2,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Zs(i))**3) - eWUZb3_95
    i = 5  !up to 100km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nAr_He_integrand2_1,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nAr_He_integrand2_2,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Zs(i))**3) - eWUZb3_97
    i = 6  !up to 110km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nAr_He_integrand4_1,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nAr_He_integrand4_2,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Zs(i))**3) - eWUZb3_100
    i = 7  !up to 115km
    xb(i,2) = xb(i-1,2) + Romberg_Quad(nAr_He_integrand4_1,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,3) = xb(i-1,3) + Romberg_Quad(nAr_He_integrand4_2,Zs(i-1),Zs(i),0._dp,rTol_tier3)
    xb(i,2:3) = xb(i,2:3) + bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Zs(i))**3) - eWUZb3(:,9)
    i = 8  !up to 120km
    xb(i,2:3) = xb(i-1,2:3) +  c9ba * (c9bb * (Zs(i)-115._dp) / (R_Earth+Zs(i)) + Lb(9) * (Log(T(Zs(i),10)/(R_Earth+Zs(i))) + c9bc))
    xb(i,2:3) = xb(i,2:3) +  bigQ_3W * Exp(bigWi(4:5) * (bigUi(4:5) - Zs(i))**3) - eWUZb3_115
    xb(i,3) = xb(i,3) + alphaHe * Log(T(Zs(i),10)) - c9bd
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
    Real(dp), Intent(Out) :: zbs(1:3)
    Integer, Intent(Out) :: nb(1:3,1:4)
    Real(dp), Intent(Out) :: xb(1:3,1:4)
    Real(dp), Parameter :: Zs(1:3) = (/  91._dp, & 
                                      & 100._dp, & 
                                      & 110._dp  /)
    Integer, Parameter :: bs(1:3) = (/  8, & 
                                     &  8, & 
                                     &  9  /)
    Integer :: i,j
    Integer, Parameter :: Pgoal1 = 15
    Integer, Parameter :: Pgoal2 = 12
    Real(dp), Parameter :: rTol = 1.E-15_dp

    zbs = Zs
    nb = 0
    xb = 0._dp
    Do i = 2,3
        xb(i,1) = Romberg_Quad(nN2_integrand_no_b,Zs(i-1),Zs(i),0._dp,rTol,n_ord=nb(i,1))
        xb(i,2) = Progressive_GaussLegendre(nN2_integrand_no_b,Zs(i-1),Zs(i),rTol,0._dp,n_done=nb(i,2))
        Do j = 3,100
            xb(i,3) = GaussLegendreN(j,nN2_integrand_no_b,Zs(i-1),Zs(i))
            If (Floor(Prec(xb(i,3),xb(i,1))) .GE. Pgoal1) Then
                If (Floor(Prec(GaussLegendreN(j+1,nN2_integrand_no_b,Zs(i-1),Zs(i)),xb(i,1))).GE.Floor(Prec(xb(i,3),xb(i,1)))) Then
                    nb(i,3) = j
                    Exit
                End If
            End If
        End Do
        Do j = 3,100
            xb(i,4) = GaussLegendreN(j,nN2_integrand_no_b,Zs(i-1),Zs(i))
            If (Floor(Prec(xb(i,4),xb(i,1))) .GE. Pgoal2) Then
                If (Floor(Prec(GaussLegendreN(j+1,nN2_integrand_no_b,Zs(i-1),Zs(i)),xb(i,1))).GE.Floor(Prec(xb(i,4),xb(i,1)))) Then
                    nb(i,4) = j
                    Exit
                End If
            End If
        End Do
    End Do
End Subroutine nN2_GLpoints

Subroutine nO1_O2_GLpoints(zbs,nb,xb)
    Use Kinds, Only: dp
    Use Utilities, Only: Prec
    Use Quadratures, Only: Romberg_Quad
    Use Quadratures, Only: GaussLegendreN
    Use Quadratures, Only: Progressive_GaussLegendre
    Implicit None
    Real(dp), Intent(Out) :: zbs(1:6)
    Integer, Intent(Out) :: nb(1:6,1:4,1:2)
    Real(dp), Intent(Out) :: xb(1:6,1:4,1:2)
    Real(dp), Parameter :: Zs(1:6) = (/  91._dp, & 
                                      &  95._dp, & 
                                      &  97._dp, &
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 115._dp  /)
    Integer, Parameter :: bs(1:6) = (/  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  9  /)
    Integer :: i,j,b
    Integer, Parameter :: Pgoal1 = 14
    Integer, Parameter :: Pgoal2 = 11
    Real(dp), Parameter :: rTol = 1.E-14_dp

    zbs = Zs
    nb = 0
    xb = 0._dp
    !up to 95km
    i = 1
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand1_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand1_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand1_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand1_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nO1_O2_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nO1_O2_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
    !up to 97km
    i = 2
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand2_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand2_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand2_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand2_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand2_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand2_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand2_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand2_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nO1_O2_integrand2_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand2_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nO1_O2_integrand2_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand2_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
    !up to 100km
    i = 3
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand3_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand3_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand3_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand3_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand3_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand3_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nO1_O2_integrand3_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nO1_O2_integrand3_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
    !up to 110km
    i = 4
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand3_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand3_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand3_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand3_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand3_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand3_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nO1_O2_integrand3_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nO1_O2_integrand3_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand3_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
    !up to 115km
    i = 5
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nO1_O2_integrand4_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nO1_O2_integrand4_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nO1_O2_integrand4_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nO1_O2_integrand4_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nO1_O2_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nO1_O2_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nO1_O2_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nO1_O2_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nO1_O2_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
End Subroutine nO1_O2_GLpoints

Subroutine nAr_He_GLpoints(zbs,nb,xb)
    Use Kinds, Only: dp
    Use Utilities, Only: Prec
    Use Quadratures, Only: Romberg_Quad
    Use Quadratures, Only: GaussLegendreN
    Use Quadratures, Only: Progressive_GaussLegendre
    Implicit None
    Real(dp), Intent(Out) :: zbs(1:7)
    Integer, Intent(Out) :: nb(1:7,1:4,1:2)
    Real(dp), Intent(Out) :: xb(1:7,1:4,1:2)
    Real(dp), Parameter :: Zs(1:7) = (/  86._dp, &
                                      &  91._dp, & 
                                      &  95._dp, & 
                                      &  97._dp, &
                                      & 100._dp, & 
                                      & 110._dp, & 
                                      & 115._dp  /)
    Integer, Parameter :: bs(1:7) = (/  7, &
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  8, & 
                                     &  9  /)
    Integer :: i,j,b
    Integer, Parameter :: Pgoal1 = 13
    Integer, Parameter :: Pgoal2 = 10
    Real(dp), Parameter :: rTol = 1.E-13_dp

    zbs = Zs
    nb = 0
    xb = 0._dp
    !up to 91km
    i = 1
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand1_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand1_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand1_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand1_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nAr_He_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nAr_He_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
    !up to 95km
    i = 2
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand1_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand1_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand1_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand1_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nAr_He_integrand1_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nAr_He_integrand1_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand1_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
    !up to 97km
    i = 3
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand2_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand2_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand2_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand2_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand2_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand2_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nAr_He_integrand2_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nAr_He_integrand2_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
    !up to 100km
    i = 4
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand2_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand2_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand2_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand2_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand2_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand2_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nAr_He_integrand2_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nAr_He_integrand2_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand2_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
    !up to 110km
    i = 5
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand4_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand4_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand4_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand4_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nAr_He_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nAr_He_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
    !up to 115km
    i = 6
    b = bs(i)
    xb(i+1,1,1) = Romberg_Quad(nAr_He_integrand4_1,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,1))
    xb(i+1,1,2) = Romberg_Quad(nAr_He_integrand4_2,Zs(i),Zs(i+1),0._dp,rTol,n_ord=nb(i+1,1,2))
    xb(i+1,2,1) = Progressive_GaussLegendre(nAr_He_integrand4_1,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,1))
    xb(i+1,2,2) = Progressive_GaussLegendre(nAr_He_integrand4_2,Zs(i),Zs(i+1),rTol,0._dp,n_done=nb(i+1,2,2))
    Do j = 3,100
        xb(i+1,3,1) = GaussLegendreN(j,nAr_He_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,1),xb(i+1,1,1))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,3,1),xb(i+1,1,1)))) Then
                nb(i+1,3,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,3,2) = GaussLegendreN(j,nAr_He_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,3,2),xb(i+1,1,2))) .GE. Pgoal1) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,3,2),xb(i+1,1,2)))) Then
                nb(i+1,3,2) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,1) = GaussLegendreN(j,nAr_He_integrand4_1,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,1),xb(i+1,1,1))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_1,Zs(i),Zs(i+1)),xb(i+1,1,1))).GE.Floor(Prec(xb(i+1,4,1),xb(i+1,1,1)))) Then
                nb(i+1,4,1) = j
                Exit
            End If
        End If
    End Do
    Do j = 3,100
        xb(i+1,4,2) = GaussLegendreN(j,nAr_He_integrand4_2,Zs(i),Zs(i+1))
        If (Floor(Prec(xb(i+1,4,2),xb(i+1,1,2))) .GE. Pgoal2) Then
            If (Floor(Prec(GaussLegendreN(j+1,nAr_He_integrand4_2,Zs(i),Zs(i+1)),xb(i+1,1,2))).GE.Floor(Prec(xb(i+1,4,2),xb(i+1,1,2)))) Then
                nb(i+1,4,2) = j
                Exit
            End If
        End If
    End Do
End Subroutine nAr_He_GLpoints
# endif

# if (INTEGRAND_STOPS || GL_POINTS)
Function nN2_integrand_no_b(z) Result(y)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: y
    Real(dp), Intent(In) :: z
    
    y = g(z) / T(z)
End Function nN2_integrand_no_b

Function nO1_O2_integrand1_1(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand1_1
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand1(z,b)
    nO1_O2_integrand1_1 = x(1)
End Function nO1_O2_integrand1_1
Function nO1_O2_integrand1_2(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand1_2
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand1(z,b)
    nO1_O2_integrand1_2 = x(2)
End Function nO1_O2_integrand1_2
Function nO1_O2_integrand2_1(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand2_1
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand2(z,b)
    nO1_O2_integrand2_1 = x(1)
End Function nO1_O2_integrand2_1
Function nO1_O2_integrand2_2(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand2_2
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand2(z,b)
    nO1_O2_integrand2_2 = x(2)
End Function nO1_O2_integrand2_2
Function nO1_O2_integrand3_1(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand3_1
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand3(z,b)
    nO1_O2_integrand3_1 = x(1)
End Function nO1_O2_integrand3_1
Function nO1_O2_integrand3_2(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand3_2
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand3(z,b)
    nO1_O2_integrand3_2 = x(2)
End Function nO1_O2_integrand3_2
Function nO1_O2_integrand4_1(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand4_1
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand4(z,b)
    nO1_O2_integrand4_1 = x(1)
End Function nO1_O2_integrand4_1
Function nO1_O2_integrand4_2(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand4_2
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand4(z,b)
    nO1_O2_integrand4_2 = x(2)
End Function nO1_O2_integrand4_2
Function nO1_O2_integrand5_1(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand5_1
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand5(z,b)
    nO1_O2_integrand5_1 = x(1)
End Function nO1_O2_integrand5_1
Function nO1_O2_integrand5_2(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nO1_O2_integrand5_2
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nO1_O2_integrand5(z,b)
    nO1_O2_integrand5_2 = x(2)
End Function nO1_O2_integrand5_2

Function nAr_He_integrand1_1(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nAr_He_integrand1_1
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nAr_He_integrand1(z,b)
    nAr_He_integrand1_1 = x(1)
End Function nAr_He_integrand1_1
Function nAr_He_integrand1_2(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nAr_He_integrand1_2
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nAr_He_integrand1(z,b)
    nAr_He_integrand1_2 = x(2)
End Function nAr_He_integrand1_2
Function nAr_He_integrand2_1(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nAr_He_integrand2_1
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nAr_He_integrand2(z,b)
    nAr_He_integrand2_1 = x(1)
End Function nAr_He_integrand2_1
Function nAr_He_integrand2_2(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nAr_He_integrand2_2
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nAr_He_integrand2(z,b)
    nAr_He_integrand2_2 = x(2)
End Function nAr_He_integrand2_2
Function nAr_He_integrand4_1(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nAr_He_integrand4_1
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nAr_He_integrand4(z,b)
    nAr_He_integrand4_1 = x(1)
End Function nAr_He_integrand4_1
Function nAr_He_integrand4_2(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nAr_He_integrand4_2
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nAr_He_integrand4(z,b)
    nAr_He_integrand4_2 = x(2)
End Function nAr_He_integrand4_2
Function nAr_He_integrand5_1(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nAr_He_integrand5_1
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nAr_He_integrand5(z,b)
    nAr_He_integrand5_1 = x(1)
End Function nAr_He_integrand5_1
Function nAr_He_integrand5_2(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: nAr_He_integrand5_2
    Real(dp), Intent(In) :: z
    Real(dp) :: x(1:2)
    Integer :: b

    b = Find_Base_Layer(z)
    x = nAr_He_integrand5(z,b)
    nAr_He_integrand5_2 = x(2)
End Function nAr_He_integrand5_2
# endif

End Module US_Std_Atm_1976

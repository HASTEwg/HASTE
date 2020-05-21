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
Module Random_Directions

    Implicit None
    Private
    Public :: Isotropic_Azimuth
    Public :: Isotropic_mu
    Public :: Isotropic_Omega_hat
    Public :: Neutron_Anisotropic_mu0cm
    Public :: mu_omega_2_OmegaHat
    Public :: mu_from_power_cosine
    Public :: mu_from_power_cosine125
    Public :: PDF_power_cosine
    Public :: PDF_power_cosine125

    Interface Neutron_Anisotropic_mu0cm
        Module Procedure Neutron_Anisotropic_mu0cm_Legendre
        Module Procedure Neutron_Anisotropic_mu0cm_TablePDF
    End Interface Neutron_Anisotropic_mu0cm

Contains

Function Isotropic_Azimuth(RNG) Result(w)
    !Returns w (or omega), an azimuthal scattering angle uniformly distributed [0,2pi)
    Use Kinds, Only: dp
    Use Global, Only: TwoPi
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp) :: w
    Type(RNG_Type), Intent(InOut) :: RNG

    w = TwoPi * RNG%Get_Random()
End Function Isotropic_Azimuth

Function Isotropic_mu(RNG) Result(mu)
    !Returns mu, cosine of the polar scattering angle (theta) uniformly distributed [1,-1) corresponding to angles [0,pi)
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp) :: mu
    Type(RNG_Type), Intent(InOut) :: RNG

    mu = 2._dp * RNG%Get_Random() - 1._dp
End Function Isotropic_mu

Function mu_from_power_cosine(RNG,x) Result(mu)
    !Returns mu, cosine of the polar scattering angle (theta) distributed on angles [0,pi/2) with density function Cos(theta)^x
    Use Kinds, Only: dp
    Use Global, Only: halfPi
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp) :: mu
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp), Intent(In) :: x
    Real(dp) :: theta

    Do
        theta = halfPi * (1._dp - Sqrt(RNG%Get_Random()))
        If ((halfPi - theta) * RNG%Get_Random() .GT. Cos(theta)**x) Cycle
        Exit
    End Do
    mu = Cos(theta)
End Function mu_from_power_cosine

Function mu_from_power_cosine125(RNG) Result(mu)
    !Returns mu, cosine of the polar scattering angle (theta) distributed on angles [0,pi/2) with density function Cos(theta)^1.25
    Use Kinds, Only: dp
    Use Global, Only: halfPi
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp) :: mu
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp) :: theta
    Real(dp), Parameter :: g_max = 0.5_dp * (5._dp**0.375_dp)

    Do
        theta = halfPi * (1._dp - Sqrt(RNG%Get_Random()))
        If (g_max * (halfPi - theta) * RNG%Get_Random() .GT. Cos(theta)**1.25_dp) Cycle
        Exit
    End Do
    mu = Cos(theta)
End Function mu_from_power_cosine125

Function PDF_power_cosine(mu,x) Result(p)
    Use Kinds, Only: dp
    Use Global, Only: sqrtPi
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: mu
    Real(dp), Intent(In) :: x

    p = (mu**x) * 2._dp * GAMMA(1._dp + 0.5_dp*x) / ( sqrtPi * GAMMA(0.5_dp * (x + 1._dp)) )
End Function PDF_power_cosine

Function PDF_power_cosine125(mu) Result(p)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: mu
    Real(dp), Parameter :: invPDFnorm = 1._dp / 0.9308740569746155_dp

    p = (mu**1.25_dp) * invPDFnorm
End Function PDF_power_cosine125

Function mu_omega_2_OmegaHat(xi,w) Result(Omegahat)
    !Converts angles (polar cosine and azimuthal rotation) to a unit vector
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: OmegaHat(1:3)
    Real(dp), Intent(In) :: xi !cosine of the polar scattering angle
    Real(dp), Intent(In) :: w  !azimuthal scattering angle
    Real(dp) :: nu,mu,eta

    nu = Sqrt(1._dp - xi**2)
    mu = nu * Cos(w)
    eta = nu * Sin(w)
    OmegaHat = (/ mu, eta, xi /)
End Function mu_omega_2_OmegaHat

Function Isotropic_Omega_hat(RNG) Result(OmegaHat)
    !Reutrns an isotropic random direction
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp):: OmegaHat(1:3)
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp):: xi  ! z direction cosine = cos(theta), theta is angle from zHat
    Real(dp):: w   ! angle from xHat toward yHat around zHat

    xi = Isotropic_mu(RNG)
    w = Isotropic_Azimuth(RNG)
    OmegaHat = mu_omega_2_OmegaHat(xi,w)
End Function Isotropic_Omega_hat

Function Neutron_Anisotropic_mu0cm_Legendre(n,a,RNG) Result(mu0cm)
    ! Draws random mu0cm from Legendre expansion of anisotropic distribution in CM frame:
    !   pdf is f(mu0cm) = Sum[a[j] LegendreP[j, mu0cm], {j,0,n}]
    !   Root-solves CDF(mu0cm) = xi for mu0cm, where xi is random in [0,1)
    Use Kinds, Only: dp
    Use Legendre_Utilities, Only: Legendre_CDF_Coefficients, Legendre_P
    Use Random_Numbers, Only: RNG_Type
    Use FileIO_Utilities, Only: Output_Message
    Implicit None
    Real(dp) :: mu0cm                    ! [] scattering deflection angle cosine in CM frame
    Integer, Intent(In) :: n             ! order of Legendre expansion of pdf
    Real(dp), Intent(In) :: a(0:n)       ! [] Legendre coefficients of pdf: a(j) = ((2j+1)/2) sigma(j)
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp) :: b(0:n+1)                 ! [] Legendre coefficients of CDF
    Real(dp) :: P(0:n+1)                 ! [] P(j) = LegendreP[j,mu]
    Real(dp) :: xi                       ! [] pseudo-random number in [0,1)
    Real(dp) :: muOld                    ! [] used to test for convergence
    Real(dp) :: muMin, muMax             ! [] bounds for bisection (when Newton jumps out of bounds)
    Real(dp) :: pdf, CDF                 ! [] pdf and cdf of mu0cm
    Integer :: i
    Real(dp) :: x                        ! []
    Real(dp), Parameter :: AbsTol = 1.E-12_dp      ! tolerance for convergence

    xi = RNG%Get_Random()
    mu0cm = 2._dp * xi - 1._dp
    If (n .EQ. 0) Return ! Isotropic scatter
    ! linearly-anisotropic scatter (either as sample or as starting point for rootsolver)
    x = mu0cm + a(1)
    x = 2._dp * x / (1._dp + Sqrt(1._dp + 4._dp * a(1) * x))
    If (n .EQ. 1) Then  !linearly anisotropic scatter
        If (Abs(x) .LE. 1._dp) then
            mu0cm = x
        Else If (Abs(x) .LE. 1._dp + 10._dp * Spacing(1._dp)) Then
            mu0cm = Sign(1._dp, x)
        Else
            Call Output_Message('ERROR:  Random_Directions: Neutron_Anisotropic_mu0cm_Legendre:  & 
                                &Linearly-anisotropic scatter out of range.',kill=.TRUE.)
        End If
        Return
    End If
    b = Legendre_CDF_Coefficients(n,a)
    ! Try linearly-anisotropic scatter, x, as first approximation for mu0cm;
    !   but if higher order terms are required to get mu0cm in range,
    !   start with isotropic value instead (already stored in mu0cm)
    If (Abs(x) .LE. 1._dp) mu0cm = x
    ! Initialize search boundaries
    muMin = -1._dp
    muMax =  1._dp
    i = 0
    Do i = 1,42  ! 41 iterations will meet tolerance by bisection
        muOld = mu0cm
        Call Legendre_P(mu0cm,n+1,P)
        CDF = Dot_Product(b, P)
        If (Abs(CDF-xi) .LE. AbsTol) Return  !Normal exit for guessing correct mu0cm
        If (CDF .LE. xi) Then ! tighten boundaries
            MuMin = mu0cm
        Else
            MuMax = mu0cm
        End If
        ! Try Newtons method
        pdf = Dot_Product(a,P(0:n))
        mu0cm = mu0cm - (CDF - xi) / pdf
        ! Use bisection if Newtons method jumps outside the current bisection interval
        If (mu0cm.LT.muMin .OR. mu0cm.GT.muMax) mu0cm = 0.5_dp * (muMin + muMax)
        If (Abs(mu0cm - muOld) .LE. AbsTol) Return  !Normal exit for convergence on mu0cm
    End Do
    !If we get this far, no normal exit
    Call Output_Message('ERROR:  Random_Directions: Neutron_Anisotropic_mu0cm_Legendre:  Failed to converge in', & 
                       & i,' iterations.',kill=.TRUE.)
End Function Neutron_Anisotropic_mu0cm_Legendre

Function Neutron_Anisotropic_mu0cm_tablePDF(n1,ua1,n2,ua2,Econv,RNG) Result(mu0cm)
    ! Draws random mu0cm from tabular pdf of anisotropic distribution in CM frame:
    !   Uses geometric rejection on the tabulated pdf (interpolated via log-linear in mu, and linear-linear in energy)
    Use Kinds, Only: dp
    Use cs_Utilities, Only: Tabular_Cosine_pdf
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp):: mu0cm                    ! [] scattering deflection angle cosine in CM frame
    Integer, Intent(In) :: n1,n2
    Real(dp), Intent(In) :: ua1(1:n1,1:2),ua2(1:n2,1:2)
    Real(dp), Intent(In) :: Econv
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp) :: maxP

    If (Econv.GE.0._dp .AND. Econv.LE.1._dp) Then !interpolation between E1 and E2
        ! maxP = Max(MaxVal(ua1(:,2)),MaxVal(ua2(:,2)))
        maxP = Exp(Max(MaxVal(ua1(:,2)),MaxVal(ua2(:,2))))
    Else !extrapolating outside range E1 to E2
        maxP = 1._dp  !this is not the most efficent, but this case would not be encountered frequently
    End If
    Do
        mu0cm = 2._dp * RNG%Get_Random() - 1._dp
        If (Tabular_Cosine_pdf(mu0cm,n1,ua1,n2,ua2,Econv) .GE. maxP*RNG%Get_Random()) Exit
    End Do
End Function Neutron_Anisotropic_mu0cm_tablePDF

Function Photon_Aniosotropic_mu_Coherent(RNG) Result(mu)
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp) :: mu
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp) :: xi
    Real(dp) :: a
    Real(dp), Parameter :: one_third = 1._dp / 3._dp
    Real(dp), Parameter :: two_thirds = 2._dp / 3._dp

    xi = RNG%Get_Random()
    a = 2._dp - 4._dp * xi + Sqrt(5._dp + 16._dp*xi*(xi-1._dp))
    mu = (1._dp - a**two_thirds) / (a**one_third)
End Function Photon_Aniosotropic_mu_Coherent

Function Photon_Aniosotropic_mu_Incoherent(RNG,a) Result(mu)
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp) :: mu
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp), Intent(In) :: a
    Real(dp) :: kn
    Real(dp) :: mu2p1 !(mu**2 + 1._dp)
    Real(dp) :: amamup1 !(a - a*mu + 1._dp) OR (1._dp+a(1._dp-mu))

    Do  !sampling by geometric rejection
        !sample mu from coherent scattering distribution
        mu = Photon_Aniosotropic_mu_Coherent(RNG)
        !compute the value of the KN distribution at this mu
        !This is the KN distributon scaled on 0 < f(mu) < TWO  (using this scaled value eliminates a multiplication here and a 
        ! multiplication in the following check for acceptance)
        mu2p1 = 1._dp + mu**2
        amamup1 = 1._dp + a * (1._dp - mu)
        kn = mu2p1 * (1._dp + (a**2 * (mu2p1 - 2._dp*mu) / (mu2p1 * amamup1))) / amamup1**2
        !if the probability of KN at this mu, exceeds the probability of the sampled distribution
        If ( kn .GT. mu2p1*RNG%Get_Random() ) Exit
    End Do
End Function Photon_Aniosotropic_mu_Incoherent

End Module Random_Directions

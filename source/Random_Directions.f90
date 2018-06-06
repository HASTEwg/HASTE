Module Random_Directions
    
    Implicit None
    Private
    Public :: Isotropic_Azimuth
    Public :: Isotropic_mu
    Public :: Isotropic_Omega_hat
    Public :: Neutron_Anisotropic_mu0cm
    Public :: mu_omega_2_OmegaHat
    
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

Function mu_omega_2_OmegaHat(xi,w) Result(Omega_hat)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: OmegaHat
    Real(dp), Intent(In) :: xi,w
    Real(dp) :: nu,nu,eta
    
    nu = Sqrt(1._dp - xi**2)
    mu = nu * Cos(w)
    eta = nu * Sin(w)
    OmegaHat = (/ mu, eta, xi /)    
End Function mu_omega_2_OmegaHat

Function Isotropic_Omega_hat(RNG) Result(OmegaHat)
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp):: OmegaHat(1:3)        ! Result
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp):: xi                   ! z direction cosine = cos(theta), theta is angle from zHat
    !Real(dp):: nu                   ! sin(theta)
    Real(dp):: omega                ! angle from xHat toward yHat around zHat
    !Real(dp):: mu, eta              ! x and y direction cosines
    
    xi = Isotropic_mu(RNG)
    !nu = Sqrt(1._dp - xi**2)
    omega = Isotropic_Azimuth(RNG)
    !mu = nu * Cos(omega)
    !eta = nu * Sin(omega)
    !OmegaHat = (/ mu, eta, xi /)
    OmegaHat = mu_omega_2_OmegaHat(xi,omega)
End Function Isotropic_Omega_hat

Function Neutron_Anisotropic_mu0cm_Legendre(a,n,RNG) Result(mu0cm)
    ! Draws random mu0cm from Legendre expansion of anisotropic distribution in CM frame:
    !   pdf is f(mu0cm) = Sum[a[j] LegendreP[j, mu0cm], {j,0,n}]
    !   Root-solves CDF(mu0cm) = xi for mu0cm, where xi is random in [0,1)
    Use Kinds, Only: dp
    Use Legendre_Utilities, Only: Legendre_CDF_Coefficients, Legendre_P
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp) :: mu0cm                    ! [] scattering deflection angle cosine in CM frame
    Real(dp), Intent(In) :: a(0:n)       ! [] Legendre coefficients of pdf: a(j) = ((2j+1)/2) sigma(j)
    Integer, Intent(In) :: n             ! order of Legendre expansion of pdf
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
            Print *, "ERROR:  Random_Directions: Neutron_Anisotropic_mu0cm_Legendre:  Linearly-anisotropic scatter out of range."
            Stop
        End If
        Return
    End If
    b = Legendre_CDF_Coefficients(a, n)
    ! Try linearly-anisotropic scatter, x, as first approximation for mu0cm;
    !   but if higher order terms are required to get mu0cm in range,
    !   start with isotropic value instead (already stored in mu0cm)
    If (Abs(x) .LE. 1._dp) mu0cm = x
    ! Initialize search boundaries
    muMin = -1._dp
    muMax =  1._dp
    i = 0
    Do !i = 0,50  ! 41 iterations will meet tolerance by bisection
        muOld = mu0cm
        Call Legendre_P(mu0cm, n+1, P)
        CDF = Dot_Product(b, P)
        If (Abs(CDF-xi) .LE. AbsTol) Return  !Normal exit for guessing correct mu0cm
        If (CDF .LE. xi) Then ! tighten boundaries 
            MuMin = mu0cm
        Else
            MuMax = mu0cm
        End If
        ! Try Newton's method
        pdf = Dot_Product(a, P(0:n))
        mu0cm = mu0cm - (CDF - xi) / pdf 
        ! Use bisection if Newton's method jumps outside the current bisection interval
        If (mu0cm.LT.muMin .OR. mu0cm.GT.muMax) mu0cm = 0.5_dp * (muMin + muMax)
        If (Abs(mu0cm - muOld) .LE. AbsTol) Return  !Normal exit for convergence on mu0cm
        If (i .GT. 50) Exit  !unconverged exit
        i = i + 1
    End Do
    !If we get this far, no normal exit
    Print *, "ERROR:  Random_Directions: Neutron_Anisotropic_mu0cm_Legendre:  Failed to converge in",i," iterations."
    Stop
End Function Neutron_Anisotropic_mu0cm_Legendre

Function Neutron_Anisotropic_mu0cm_tablePDF(n1,ua1,n2,ua2,Econv,RNG) Result(mu0cm)
    ! Draws random mu0cm from tabular pdf of anisotropic distribution in CM frame:
    !   Uses geometric rejection on the tabulated pdf (interpolated via log-linear in mu, and linear-linear in energy)
    Use Kinds, Only: dp
    Use Cross_Sections, Only: Tabular_Cosine_pdf
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp):: mu0cm                    ! [] scattering deflection angle cosine in CM frame
    Integer, Intent(In) :: n1,n2
    Real(dp), Intent(In) :: ua1(1:n1,1:2),ua2(1:n2,1:2)
    Real(dp), Intent(In) :: Econv
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp) :: maxP
    
    If (Econv.GE.0._dp .AND. Econv.LE.1._dp) Then !interpolation between E1 and E2
        maxP = Max(MaxVal(ua1(:,2)),MaxVal(ua2(:,2)))
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
        mu = Photon_Aniosotropic_mu_Rayleigh(RNG)
        !compute the value of the KN distribution at this mu 
        !this is the KN distributon scaled on 0 < f(mu) < TWO  (using this scaled value eliminates a multiplication here and a multiplication in the following check for acceptance)
        mu2p1 = 1._dp + mu**2
        amamup1 = 1._dp + a * (1._dp - mu)
        kn = mu2p1 * (1._dp + (a**2 * (mu2p1 - 2._dp*mu) / (mu2p1 * amamup1))) / amamup1**2
        !mu2p1amamup1 = mu2p1 * amamup1
        !kn = mu2p1amamup1**2 * amamup1 * (mu2p1amamup1 + a**2 * (mu2p1 - 2._dp*mu))  !alternate formulation?  Need to check, might be cheaper to compute
        !if the probability of KN at this mu, exceeds the probability of the sampled distribution
        If ( kn .GT. RNG%Get_Random(mu2p1) ) Exit  !If ( kn .GT. RNG%Get_Random(0.5_dp*(1._dp + mu**2)) ) Exit
    End Do
End Function Photon_Aniosotropic_mu_Incoherent

End Module Random_Directions

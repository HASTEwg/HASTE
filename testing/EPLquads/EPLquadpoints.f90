Program EPLquadpoints

Use Kinds, Only: dp
Use Global, Only: R_earth
Use US_Std_Atm_1976, Only: rho
Use US_Std_Atm_1976, Only: Zb
Use Quadratures, Only: GaussLegendreN

Implicit None

Real(dp), SAVE :: z0,r0,zeta0,inv_rho0
Integer :: b,i,n
Integer, Parameter :: n_zeta = 100
Integer, Parameter :: n_max = 20
Real(dp) :: dZ,Smax
Real(dp) :: Ls(1:10,0:n_max,0:n_zeta)
Real(dp) :: Lz(1:10,0:n_max,0:n_zeta)

inv_rho0 = rho(0._dp)

Do b = 0,Size(Zb)-2
    z0 = Zb(b)
    r0 = R_earth + z0
    Do i = 0,n_zeta
        zeta0 = Real(i,dp)*(1._dp / Real(n_zeta,dp))
        dZ = Zb(b+1) - z0
        Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
        Ls(b,0,i) = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,rtol=1.E-12_dp,atol=0._dp)
        Lz(b,0,i) = Romberg_Quad(EPL_Integrand_dZ,0._dp,dZ,rtol=1.E-12_dp,atol=0._dp)
        Do n = 1,n_max
            Ls(b,n,i) = GaussLegendreN(n,EPL_Integrand_dS,0._dp,Smax)
            Lz(b,n,i) = GaussLegendreN(n,EPL_Integrand_dZ,0._dp,dZ)
        End Do
    End Do
End Do

Contains

Function EPL_Integrand_dS(s) Result(f)
    Use Kinds, Only: dp
    Use Utilities, Only: Smaller_Quadratic_Root
    Use US_Std_Atm_1976, Only: rho
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: s
    Real(dp) :: deltaZ
    
    deltaZ = Smaller_Quadratic_root(r0,s*(2._dp*r0*zeta0 + s))
    f = rho(z0 + deltaZ) * inv_rho0
End Function EPL_Integrand_dS

Function EPL_Integrand_dZ(deltaZ) Result(f)
    Use Kinds, Only: dp
    Use US_Std_Atm_1976, Only: rho
    Implicit None
    Real(dp) :: f
    Real(dp), Intent(In) :: deltaZ
    
    f = inv_rho0 * rho(z0 + deltaZ) * ((r0 + deltaZ) / Sqrt(r0*zeta0)**2 + 2._dp*r0*deltaZ + deltaZ**2))
End Function EPL_Integrand_dZ

End Program EPLquadpoints

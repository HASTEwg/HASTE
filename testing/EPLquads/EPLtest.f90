Program EPLtest

Use Kinds, Only: dp
Use Global, Only: R_earth
Use Atmospheres, Only: Atmosphere_Type
Use Atmospheres, Only: Setup_atmosphere
Use Pathlengths, Only: zeta_downward
Use Pathlengths, Only: S_upward
Use Pathlengths, Only: EPL
Use Pathlengths, Only: S_and_L_to_edge
Use Pathlengths, Only: r_close_approach
Use Quadratures, Only: Romberg_Quad

Implicit None

Type(Atmosphere_Type) :: atm
Real(dp) :: z0,z1
Real(dp) :: r0,r1
Real(dp) :: zeta0,zeta1
Real(dp) :: Smax
Integer :: i,nb
Real(dp) :: s,L
Integer, Allocatable :: bb(:)
Real(dp),Allocatable :: Lb(:)
Logical, Allocatable :: db(:)

Allocate(bb(2),Lb(2),db(2))
bb = 0
Lb = 0._dp
db = .FALSE.
atm = Setup_Atmosphere('atm_setup.txt','resources/')
zeta0 = -0.8278009109832294_dp
z0 = 86._dp
r0 = R_earth + z0
z1 = 0._dp
r1 = R_earth + z1
zeta1 = zeta_downward(r0,r1,zeta0)
Smax = S_upward(r1,z0-z1,-zeta1)
Print*,EPL(atm,r0,z0,zeta0,Smax)  !Downward EPL is wrong?
Print*,EPL(atm,r1,z1,-zeta1,Smax)
Print*,Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,0._dp,1.E-6_dp)
Call S_and_L_to_edge(atm,r0,z0,zeta0,s,L,nb,bb,Lb,db)
Do i = 1,nb
    Print*,bb(i),Lb(i),db(i)
End Do
Print*,L

Print*,-zeta1
Print*,-zeta_downward(r1,R_close_approach(r0,zeta0))

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
    f = rho(z0 + deltaZ)
    f = f /rho(0._dp)
End Function EPL_Integrand_dS

End Program EPLtest

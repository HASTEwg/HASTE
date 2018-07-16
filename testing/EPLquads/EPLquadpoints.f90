Program EPLquadpoints

Use Kinds, Only: dp
Use Global, Only: R_earth
Use US_Std_Atm_1976, Only: rho
Use US_Std_Atm_1976, Only: Zb
Use Quadratures, Only: Romberg_Quad
Use Quadratures, Only: GaussLegendreN
Use Utilities, Only: Prec

Implicit None

Real(dp), SAVE :: z0,r0,zeta0,inv_rho0
Integer :: b,i,n,j
Integer, Parameter :: n_zeta = 10
Integer, Parameter :: n_max = 20
Real(dp) :: dZ,Smax
Real(dp) :: Ls(0:10,1:n_max,0:n_zeta)
Real(dp) :: Lz(0:10,1:n_max,1:n_zeta)
Real(dp) :: L0(0:10,0:n_zeta)
Real(dp) :: b6zs(0:13)
Real(dp) :: b6L0(0:12,0:n_zeta)
Real(dp) :: b6Ls(0:12,1:n_max,0:n_zeta)
Real(dp) :: b6Lz(0:12,1:n_max,1:n_zeta)
Integer :: unit
Real(dp), Parameter :: reltol = 1.E-15_dp

inv_rho0 = 1._dp / rho(0._dp)
Write(*,*)
!Compute "exact" values
!Layers 1 thru 6
Do b = 0,5!Size(Zb)-2
    z0 = Zb(b)
    r0 = R_earth + z0
    Do i = 0,n_zeta
        zeta0 = Real(i,dp)*(1._dp / Real(n_zeta,dp))
        Write(*,'(A,A,I0,A,F0.3,A)',ADVANCE="NO") ACHAR(13),'Computing exact EPL... layer ',b+1,', zeta ',zeta0,'...'
        dZ = Zb(b+1) - z0
        Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
        L0(b,i) = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=0._dp,rtol=reltol)
    End Do
End Do
!Layer 7
b = 6
b6zs = (/ Zb(b),      &
        & 80._dp,  &
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
Do j = 0,12
    z0 = b6zs(j)
    r0 = R_earth + z0
    Do i = 0,n_zeta
        zeta0 = Real(i,dp)*(1._dp / Real(n_zeta,dp))
        Write(*,'(A,A,I0,A,I0,A,F0.3,A)',ADVANCE="NO") ACHAR(13),'Computing exact EPL... layer ',b+1,'-',j+1,', zeta ',zeta0,'...'
        dZ = b6zs(j+1) - z0
        Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
        b6L0(j,i) = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=0._dp,rtol=reltol)
    End Do
End Do
Write(*,'(A,A)') ACHAR(13),'Computing exact EPL... Done!                               '
!compute Gauss Approximations with various numbers of points
Do b = 0,5!Size(Zb)-2
    z0 = Zb(b)
    r0 = R_earth + z0
    Do i = 0,n_zeta
        zeta0 = Real(i,dp)*(1._dp / Real(n_zeta,dp))
        dZ = Zb(b+1) - z0
        Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
        Do n = 1,n_max
            Write(*,'(A,A,I0,A,F0.3,A,I0,A)',ADVANCE="NO") ACHAR(13),'Computing approx EPL... layer ',b+1,', zeta ',zeta0,' with ',n,' Gauss-Legendre points...'
            Ls(b,n,i) = GaussLegendreN(n,EPL_Integrand_dS,0._dp,Smax)
            If (i.GT.0) Lz(b,n,i) = GaussLegendreN(n,EPL_Integrand_dZ,0._dp,dZ)
        End Do
    End Do
End Do
!Layer 7
b = 6
Do j = 0,12
    z0 = b6zs(j)
    r0 = R_earth + z0
    Do i = 0,n_zeta
        zeta0 = Real(i,dp)*(1._dp / Real(n_zeta,dp))
        dZ = b6zs(j+1) - z0
        Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
        Do n = 1,n_max
            Write(*,'(A,A,I0,A,I0,A,F0.3,A,I0,A)',ADVANCE="NO") ACHAR(13),'Computing approx EPL... layer ',b+1,'-',j+1,', zeta ',zeta0,' with ',n,' Gauss-Legendre points...'
            b6Ls(j,n,i) = GaussLegendreN(n,EPL_Integrand_dS,0._dp,Smax)
            If (i.GT.0) b6Lz(j,n,i) = GaussLegendreN(n,EPL_Integrand_dZ,0._dp,dZ)
        End Do
    End Do
End Do
Write(*,*)

!Compute precisions and output
Open(NEWUNIT=unit,FILE='EPLquadPrecs.tst',ACTION='WRITE',STATUS='REPLACE')
Do b = 0,5!Size(Zb)-2
    Write(unit,'(A,I0,A,F7.3)') 'Layer #',b+1,': z0 = ',Zb(b)
    Write(unit,'(A8)',ADVANCE='NO') 'zeta0    '
    Do n = 1,n_max
        Write(unit,'(A3,I2,A3)',ADVANCE='NO') ' n=',n,''
    End Do
    Write(unit,*)
    Do n = 1,n_max+1
        Write(unit,'(A8)',ADVANCE='NO') '------  '
    End Do
    Write(unit,*)
    Do i = 0,n_zeta
        zeta0 = Real(i,dp)*(1._dp / Real(n_zeta,dp))
        Write(unit,'(F6.3,A2)',ADVANCE='NO') zeta0,''
        Do n = 1,n_max
            Write(unit,'(A2,F4.1,A2)',ADVANCE='NO') 's-',Prec(Ls(b,n,i),L0(b,i)),''
        End Do
        Write(unit,*)
        If (i.GT.0) Then
            Write(unit,'(A8)',ADVANCE='NO') ''
            Do n = 1,n_max
                Write(unit,'(A2,F4.1,A2)',ADVANCE='NO') 'z-',Prec(Lz(b,n,i),L0(b,i)),''
            End Do
            Write(unit,*)
        End If
    End Do
    Write(unit,*)
End Do
!Layer 7
b = 6
Do j = 0,12
    Write(unit,'(A,I0,A,I0,A,F7.3)') 'Layer #',b+1,'-',j+1,': z0 = ',b6Zs(j)
    Write(unit,'(A8)',ADVANCE='NO') 'zeta0    '
    Do n = 1,n_max
        Write(unit,'(A3,I2,A3)',ADVANCE='NO') ' n=',n,''
    End Do
    Write(unit,*)
    Do n = 1,n_max+1
        Write(unit,'(A8)',ADVANCE='NO') '------  '
    End Do
    Write(unit,*)
    Do i = 1,n_zeta
        zeta0 = Real(i,dp)*(1._dp / Real(n_zeta,dp))
        Write(unit,'(F6.3,A2)',ADVANCE='NO') zeta0,''
        Do n = 1,n_max
            Write(unit,'(A2,F4.1,A2)',ADVANCE='NO') 's-',Prec(b6Ls(j,n,i),b6L0(j,i)),''
        End Do
        Write(unit,*)
        If (i.GT.0) Then
            Write(unit,'(A8)',ADVANCE='NO') ''
            Do n = 1,n_max
                Write(unit,'(A2,F4.1,A2)',ADVANCE='NO') 'z-',Prec(b6Lz(j,n,i),b6L0(j,i)),''
            End Do
            Write(unit,*)
        End If
    End Do
End Do

Close(unit)

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
    
    f = inv_rho0 * rho(z0 + deltaZ) * (r0 + deltaZ) / Sqrt((r0*zeta0)**2 + 2._dp*r0*deltaZ + deltaZ**2)
End Function EPL_Integrand_dZ

End Program EPLquadpoints

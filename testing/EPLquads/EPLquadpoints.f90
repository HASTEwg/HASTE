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
Integer :: b,i,j,c
Integer, Parameter :: n_zeta = 10
Integer, Parameter :: n_max = 20
Real(dp) :: dZ,Smax
Real(dp) :: Ls
Real(dp) :: Lz
Real(dp) :: L0
Real(dp), Parameter :: reltol = 1.E-14_dp
Real(dp), Parameter :: abstol = 0._dp
Integer :: unit
Character(1) :: sub

inv_rho0 = 1._dp / rho(0._dp)
Open(NEWUNIT=unit,FILE='EPLquadPrecs.tst',ACTION='WRITE',STATUS='REPLACE')
!Layers 1-6 (b = 0-5) are all single layers
Do b = 0,6
    Write(*,*)
    z0 = Zb(b)
    Write(*,'(A,I3,A,F10.3,A,F10.3,A)') 'LAYER ',b+1,',  z = ',z0,' to ',Zb(b+1),' km'
    Write(*,'(A)') '---------------------------------------------'
    Write(unit,'(A,I3,A,F10.3,A,F10.3,A)') 'LAYER ',b+1,',  z = ',z0,' to ',Zb(b+1),' km'
    Write(unit,'(A)') '---------------------------------------------'
    r0 = R_earth + z0
    dZ = Zb(b+1) - z0
    !Shallow zetas
    Write(*,'(A)') 'zeta0 (shallow)'
    Write(*,'(A)') '-----'
    Write(unit,'(A)') 'zeta0 (shallow)'
    Write(unit,'(A)') '-----'
    Do i = 0,n_zeta-1
        !Compute exact EPL
        zeta0 = Real(i,dp) * 0.01_dp
        Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
        L0 = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=abstol,rtol=reltol)
        Write(*,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
        Write(unit,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
        !Compute approximate EPL, stopping when desired precision is achieved
        Do j = 3,n_max
            Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
            Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p12: ',Ls,'     : ',j
            If (Floor(Prec(Ls,L0)) .GE. 12) Then
                Write(*,'(A)') ' gauss-pts for prec > 12'
                Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts for prec > 12'
                Exit
            End If
            If (j .EQ. n_max) Then
                Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
                Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
            End If
        End Do
        Do j = 3,n_max
            Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
            Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p09: ',Ls,'        : ',j
            If (Floor(Prec(Ls,L0)) .GE. 9) Then
                Write(*,'(A)') ' gauss-pts for prec > 9'
                Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts for prec > 9'
                Exit
            End If
            If (j .EQ. n_max) Then
                Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
                Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
            End If
        End Do
        Do j = 3,n_max
            Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
            Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p06: ',Ls,'           : ',j
            If (Floor(Prec(Ls,L0)) .GE. 6) Then
                Write(*,'(A)') ' gauss-pts for prec > 6'
                Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts for prec > 6'
                Exit
            End If
            If (j .EQ. n_max) Then
                Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
                Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
            End If
        End Do
    End Do
    Write(*,*)
    Write(Unit,*)
    !Steep zetas
    Write(*,'(A)') 'zeta0 (steep)'
    Write(*,'(A)') '-----'
    Write(unit,'(A)') 'zeta0 (steep)'
    Write(unit,'(A)') '-----'
    Do i = 1,n_zeta
        zeta0 = Real(i,dp) * 0.1_dp
        Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
        L0 = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=abstol,rtol=reltol)
        Write(*,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
        Write(unit,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
        !Compute approximate EPL, stopping when desired precision is achieved
        Do j = 3,n_max
            Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
            Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p12: ',Ls,'     : ',j
            If (Floor(Prec(Ls,L0)) .GE. 12) Then
                Write(*,'(A)') ' gauss-pts for prec > 12'
                Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts for prec > 12'
                Exit
            End If
            If (j .EQ. n_max) Then
                Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
                Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
            End If
        End Do
        Do j = 3,n_max
            Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
            Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p12: ',Lz,'     : ',j
            If (Floor(Prec(Lz,L0)) .GE. 12) Then
                Write(*,'(A)') ' gauss-pts for prec > 12'
                Write(unit,'(A,ES19.11,A,I2,A)') '      dZ-p12: ',Lz,'     : ',j,' gauss-pts for prec > 12'
                Exit
            End If
            If (j .EQ. n_max) Then
                Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
                Write(unit,'(A,ES19.11,A,I2,A)') '      dZ-p12: ',Lz,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
            End If
        End Do
        Do j = 3,n_max
            Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
            Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p09: ',Ls,'        : ',j
            If (Floor(Prec(Ls,L0)) .GE. 9) Then
                Write(*,'(A)') ' gauss-pts for prec > 9'
                Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts for prec > 9'
                Exit
            End If
            If (j .EQ. n_max) Then
                Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
                Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
            End If
        End Do
        Do j = 3,n_max
            Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
            Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p09: ',Lz,'        : ',j
            If (Floor(Prec(Lz,L0)) .GE. 9) Then
                Write(*,'(A)') ' gauss-pts for prec > 9'
                Write(unit,'(A,ES16.8,A,I2,A)') '      dZ-p09: ',Lz,'        : ',j,' gauss-pts for prec > 9'
                Exit
            End If
            If (j .EQ. n_max) Then
                Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
                Write(unit,'(A,ES16.8,A,I2,A)') '      dZ-p09: ',Lz,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
            End If
        End Do
        Do j = 3,n_max
            Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
            Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p06: ',Ls,'           : ',j
            If (Floor(Prec(Ls,L0)) .GE. 6) Then
                Write(*,'(A)') ' gauss-pts for prec > 6'
                Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts for prec > 6'
                Exit
            End If
            If (j .EQ. n_max) Then
                Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
                Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
            End If
        End Do
        Do j = 3,n_max
            Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
            Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p06: ',Lz,'           : ',j
            If (Floor(Prec(Lz,L0)) .GE. 6) Then
                Write(*,'(A)') ' gauss-pts for prec > 6'
                Write(unit,'(A,ES13.5,A,I2,A)') '      dZ-p06: ',Lz,'           : ',j,' gauss-pts for prec > 6'
                Exit
            End If
            If (j .EQ. n_max) Then
                Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
                Write(unit,'(A,ES13.5,A,I2,A)') '      dZ-p06: ',Lz,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
            End If
        End Do
    End Do
    Write(Unit,*)
End Do
!Layer 7 (b = 6) is subdivided in half-km thick layers above 80km
! b = 6
! z0 = Zb(b)
! sub = 'a'
! Write(*,'(A,I3,A,F10.3,A,F10.3,A)') 'LAYER ',b+1,sub//',  z = ',z0,' to ',80._dp,' km'
! Write(*,'(A)') '---------------------------------------------'
! Write(unit,'(A,I3,A,F10.3,A,F10.3,A)') 'LAYER ',b+1,sub//',  z = ',z0,' to ',80._dp,' km'
! Write(unit,'(A)') '---------------------------------------------'
! r0 = R_earth + z0
! dZ = 80._dp - z0
! !Shallow zetas
! Write(*,'(A)') 'zeta0 (shallow)'
! Write(*,'(A)') '-----'
! Write(unit,'(A)') 'zeta0 (shallow)'
! Write(unit,'(A)') '-----'
! Do i = 0,n_zeta-1
!     !Compute exact EPL
!     zeta0 = Real(i,dp) * 0.01_dp
!     Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
!     L0 = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=abstol,rtol=reltol)
!     Write(*,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!     Write(unit,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!     !Compute approximate EPL, stopping when desired precision is achieved
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p12: ',Ls,'     : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 12) Then
!             Write(*,'(A)') ' gauss-pts for prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts for prec > 12'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
!         End If
!     End Do
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p09: ',Ls,'        : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 9) Then
!             Write(*,'(A)') ' gauss-pts for prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts for prec > 9'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
!         End If
!     End Do
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p06: ',Ls,'           : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 6) Then
!             Write(*,'(A)') ' gauss-pts for prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts for prec > 6'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
!         End If
!     End Do
! End Do
! Write(*,*)
! Write(Unit,*)
! !Steep zetas
! Write(*,'(A)') 'zeta0 (steep)'
! Write(*,'(A)') '-----'
! Write(unit,'(A)') 'zeta0 (steep)'
! Write(unit,'(A)') '-----'
! Do i = 1,n_zeta
!     zeta0 = Real(i,dp) * 0.1_dp
!     Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
!     L0 = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=abstol,rtol=reltol)
!     Write(*,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!     Write(unit,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!     !Compute approximate EPL, stopping when desired precision is achieved
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p12: ',Ls,'     : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 12) Then
!             Write(*,'(A)') ' gauss-pts for prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts for prec > 12'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
!         End If
!     End Do
!     Do j = 3,n_max
!         Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
!         Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p12: ',Lz,'     : ',j
!         If (Floor(Prec(Lz,L0)) .GE. 12) Then
!             Write(*,'(A)') ' gauss-pts for prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dZ-p12: ',Lz,'     : ',j,' gauss-pts for prec > 12'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dZ-p12: ',Lz,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
!         End If
!     End Do
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p09: ',Ls,'        : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 9) Then
!             Write(*,'(A)') ' gauss-pts for prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts for prec > 9'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
!         End If
!     End Do
!     Do j = 3,n_max
!         Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
!         Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p09: ',Lz,'        : ',j
!         If (Floor(Prec(Lz,L0)) .GE. 9) Then
!             Write(*,'(A)') ' gauss-pts for prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dZ-p09: ',Lz,'        : ',j,' gauss-pts for prec > 9'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dZ-p09: ',Lz,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
!         End If
!     End Do
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p06: ',Ls,'           : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 6) Then
!             Write(*,'(A)') ' gauss-pts for prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts for prec > 6'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
!         End If
!     End Do
!     Do j = 3,n_max
!         Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
!         Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p06: ',Lz,'           : ',j
!         If (Floor(Prec(Lz,L0)) .GE. 6) Then
!             Write(*,'(A)') ' gauss-pts for prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dZ-p06: ',Lz,'           : ',j,' gauss-pts for prec > 6'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dZ-p06: ',Lz,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
!         End If
!     End Do
! End Do
! Write(unit,*)
! Do c = 1,12  !12 sublayers from 80km to 86km
!     z0 = 80._dp + 0.5_dp * Real(c-1,dp)
!     Select Case (c)
!         Case(1)
!             sub = 'b'
!         Case(2)
!             sub = 'c'
!         Case(3)
!             sub = 'd'
!         Case(4)
!             sub = 'e'
!         Case(5)
!             sub = 'f'
!         Case(6)
!             sub = 'g'
!         Case(7)
!             sub = 'h'
!         Case(8)
!             sub = 'i'
!         Case(9)
!             sub = 'j'
!         Case(10)
!             sub = 'k'
!         Case(11)
!             sub = 'l'
!         Case(12)
!             sub = 'm'
!     End Select
!     Write(*,*)
!     Write(*,'(A,I3,A,F10.3,A,F10.3,A)') 'LAYER ',b+1,sub//',  z = ',z0,' to ',z0+0.5_dp,' km'
!     Write(*,'(A)') '---------------------------------------------'
!     Write(unit,'(A,I3,A,F10.3,A,F10.3,A)') 'LAYER ',b+1,sub//',  z = ',z0,' to ',z0+0.5_dp,' km'
!     Write(unit,'(A)') '---------------------------------------------'
!     r0 = R_earth + z0
!     dZ = 0.5_dp
!     !Shallow zetas
!     Write(*,'(A)') 'zeta0 (shallow)'
!     Write(*,'(A)') '-----'
!     Write(unit,'(A)') 'zeta0 (shallow)'
!     Write(unit,'(A)') '-----'
!     Do i = 0,n_zeta-1
!         !Compute exact EPL
!         zeta0 = Real(i,dp) * 0.01_dp
!         Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
!         L0 = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=abstol,rtol=reltol)
!         Write(*,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!         Write(unit,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!         !Compute approximate EPL, stopping when desired precision is achieved
!         Do j = 3,n_max
!             Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!             Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p12: ',Ls,'     : ',j
!             If (Floor(Prec(Ls,L0)) .GE. 12) Then
!                 Write(*,'(A)') ' gauss-pts for prec > 12'
!                 Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts for prec > 12'
!                 Exit
!             End If
!             If (j .EQ. n_max) Then
!                 Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
!                 Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
!             End If
!         End Do
!         Do j = 3,n_max
!             Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!             Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p09: ',Ls,'        : ',j
!             If (Floor(Prec(Ls,L0)) .GE. 9) Then
!                 Write(*,'(A)') ' gauss-pts for prec > 9'
!                 Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts for prec > 9'
!                 Exit
!             End If
!             If (j .EQ. n_max) Then
!                 Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
!                 Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
!             End If
!         End Do
!         Do j = 3,n_max
!             Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!             Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p06: ',Ls,'           : ',j
!             If (Floor(Prec(Ls,L0)) .GE. 6) Then
!                 Write(*,'(A)') ' gauss-pts for prec > 6'
!                 Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts for prec > 6'
!                 Exit
!             End If
!             If (j .EQ. n_max) Then
!                 Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
!                 Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
!             End If
!         End Do
!     End Do
!     Write(*,*)
!     Write(Unit,*)
!     !Steep zetas
!     Write(*,'(A)') 'zeta0 (steep)'
!     Write(*,'(A)') '-----'
!     Write(unit,'(A)') 'zeta0 (steep)'
!     Write(unit,'(A)') '-----'
!     Do i = 1,n_zeta
!         zeta0 = Real(i,dp) * 0.1_dp
!         Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
!         L0 = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=abstol,rtol=reltol)
!         Write(*,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!         Write(unit,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!         !Compute approximate EPL, stopping when desired precision is achieved
!         Do j = 3,n_max
!             Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!             Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p12: ',Ls,'     : ',j
!             If (Floor(Prec(Ls,L0)) .GE. 12) Then
!                 Write(*,'(A)') ' gauss-pts for prec > 12'
!                 Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts for prec > 12'
!                 Exit
!             End If
!             If (j .EQ. n_max) Then
!                 Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
!                 Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
!             End If
!         End Do
!         Do j = 3,n_max
!             Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
!             Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p12: ',Lz,'     : ',j
!             If (Floor(Prec(Lz,L0)) .GE. 12) Then
!                 Write(*,'(A)') ' gauss-pts for prec > 12'
!                 Write(unit,'(A,ES19.11,A,I2,A)') '      dZ-p12: ',Lz,'     : ',j,' gauss-pts for prec > 12'
!                 Exit
!             End If
!             If (j .EQ. n_max) Then
!                 Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
!                 Write(unit,'(A,ES19.11,A,I2,A)') '      dZ-p12: ',Lz,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
!             End If
!         End Do
!         Do j = 3,n_max
!             Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!             Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p09: ',Ls,'        : ',j
!             If (Floor(Prec(Ls,L0)) .GE. 9) Then
!                 Write(*,'(A)') ' gauss-pts for prec > 9'
!                 Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts for prec > 9'
!                 Exit
!             End If
!             If (j .EQ. n_max) Then
!                 Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
!                 Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
!             End If
!         End Do
!         Do j = 3,n_max
!             Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
!             Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p09: ',Lz,'        : ',j
!             If (Floor(Prec(Lz,L0)) .GE. 9) Then
!                 Write(*,'(A)') ' gauss-pts for prec > 9'
!                 Write(unit,'(A,ES16.8,A,I2,A)') '      dZ-p09: ',Lz,'        : ',j,' gauss-pts for prec > 9'
!                 Exit
!             End If
!             If (j .EQ. n_max) Then
!                 Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
!                 Write(unit,'(A,ES16.8,A,I2,A)') '      dZ-p09: ',Lz,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
!             End If
!         End Do
!         Do j = 3,n_max
!             Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!             Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p06: ',Ls,'           : ',j
!             If (Floor(Prec(Ls,L0)) .GE. 6) Then
!                 Write(*,'(A)') ' gauss-pts for prec > 6'
!                 Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts for prec > 6'
!                 Exit
!             End If
!             If (j .EQ. n_max) Then
!                 Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
!                 Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
!             End If
!         End Do
!         Do j = 3,n_max
!             Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
!             Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p06: ',Lz,'           : ',j
!             If (Floor(Prec(Lz,L0)) .GE. 6) Then
!                 Write(*,'(A)') ' gauss-pts for prec > 6'
!                 Write(unit,'(A,ES13.5,A,I2,A)') '      dZ-p06: ',Lz,'           : ',j,' gauss-pts for prec > 6'
!                 Exit
!             End If
!             If (j .EQ. n_max) Then
!                 Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
!                 Write(unit,'(A,ES13.5,A,I2,A)') '      dZ-p06: ',Lz,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
!             End If
!         End Do
!     End Do
!     Write(unit,*)
! End Do


!Layer 8 (b = 7) goes from 86 to 91 km
! b = 7
! z0 = Zb(b)
! Write(*,'(A,I3,A,F10.3,A,F10.3,A)') 'LAYER ',b+1,',  z = ',z0,' to ',Zb(b+1),' km'
! Write(*,'(A)') '---------------------------------------------'
! Write(unit,'(A,I3,A,F10.3,A,F10.3,A)') 'LAYER ',b+1,',  z = ',z0,' to ',Zb(b+1),' km'
! Write(unit,'(A)') '---------------------------------------------'
! r0 = R_earth + z0
! dZ = Zb(b+1) - z0
! !Shallow zetas
! Write(*,'(A)') 'zeta0 (shallow)'
! Write(*,'(A)') '-----'
! Write(unit,'(A)') 'zeta0 (shallow)'
! Write(unit,'(A)') '-----'
! Do i = 0,n_zeta-1
!     !Compute exact EPL
!     zeta0 = Real(i,dp) * 0.01_dp
!     Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
!     L0 = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=abstol,rtol=reltol)
!     Write(*,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!     Write(unit,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!     !Compute approximate EPL, stopping when desired precision is achieved
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p12: ',Ls,'     : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 12) Then
!             Write(*,'(A)') ' gauss-pts for prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts for prec > 12'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
!         End If
!     End Do
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p09: ',Ls,'        : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 9) Then
!             Write(*,'(A)') ' gauss-pts for prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts for prec > 9'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
!         End If
!     End Do
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p06: ',Ls,'           : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 6) Then
!             Write(*,'(A)') ' gauss-pts for prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts for prec > 6'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
!         End If
!     End Do
! End Do
! Write(*,*)
! Write(Unit,*)
! !Steep zetas
! Write(*,'(A)') 'zeta0 (steep)'
! Write(*,'(A)') '-----'
! Write(unit,'(A)') 'zeta0 (steep)'
! Write(unit,'(A)') '-----'
! Do i = 1,n_zeta
!     zeta0 = Real(i,dp) * 0.1_dp
!     Smax = dZ * (2._dp * r0 + dZ) / ( zeta0 * r0 + Sqrt( (zeta0 * r0)**2 + dZ * (2._dp * r0 + dZ) ) )
!     L0 = Romberg_Quad(EPL_Integrand_dS,0._dp,Smax,atol=abstol,rtol=reltol)
!     Write(*,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!     Write(unit,'(F4.2,A,ES23.15)') zeta0,'   exact: ',L0
!     !Compute approximate EPL, stopping when desired precision is achieved
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p12: ',Ls,'     : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 12) Then
!             Write(*,'(A)') ' gauss-pts for prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts for prec > 12'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dS-p12: ',Ls,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
!         End If
!     End Do
!     Do j = 3,n_max
!         Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
!         Write(*,'(A,ES19.11,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p12: ',Lz,'     : ',j
!         If (Floor(Prec(Lz,L0)) .GE. 12) Then
!             Write(*,'(A)') ' gauss-pts for prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dZ-p12: ',Lz,'     : ',j,' gauss-pts for prec > 12'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 12'
!             Write(unit,'(A,ES19.11,A,I2,A)') '      dZ-p12: ',Lz,'     : ',j,' gauss-pts DOES NOT yield prec > 12'
!         End If
!     End Do
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p09: ',Ls,'        : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 9) Then
!             Write(*,'(A)') ' gauss-pts for prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts for prec > 9'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dS-p09: ',Ls,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
!         End If
!     End Do
!     Do j = 3,n_max
!         Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
!         Write(*,'(A,ES16.8,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p09: ',Lz,'        : ',j
!         If (Floor(Prec(Lz,L0)) .GE. 9) Then
!             Write(*,'(A)') ' gauss-pts for prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dZ-p09: ',Lz,'        : ',j,' gauss-pts for prec > 9'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 9'
!             Write(unit,'(A,ES16.8,A,I2,A)') '      dZ-p09: ',Lz,'        : ',j,' gauss-pts DOES NOT yield prec > 9'
!         End If
!     End Do
!     Do j = 3,n_max
!         Ls = GaussLegendreN(j,EPL_Integrand_dS,0._dp,Smax)
!         Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dS-p06: ',Ls,'           : ',j
!         If (Floor(Prec(Ls,L0)) .GE. 6) Then
!             Write(*,'(A)') ' gauss-pts for prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts for prec > 6'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dS-p06: ',Ls,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
!         End If
!     End Do
!     Do j = 3,n_max
!         Lz = GaussLegendreN(j,EPL_Integrand_dZ,0._dp,dZ)
!         Write(*,'(A,ES13.5,A,I2)',ADVANCE='NO') ACHAR(13)//'      dZ-p06: ',Lz,'           : ',j
!         If (Floor(Prec(Lz,L0)) .GE. 6) Then
!             Write(*,'(A)') ' gauss-pts for prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dZ-p06: ',Lz,'           : ',j,' gauss-pts for prec > 6'
!             Exit
!         End If
!         If (j .EQ. n_max) Then
!             Write(*,'(A)') ' gauss-pts DOES NOT yield prec > 6'
!             Write(unit,'(A,ES13.5,A,I2,A)') '      dZ-p06: ',Lz,'           : ',j,' gauss-pts DOES NOT yield prec > 6'
!         End If
!     End Do
! End Do
! Write(unit,*)


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

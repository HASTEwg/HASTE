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
Module Astro_Utilities
    
    Use Kinds, Only: dp
    Use Global, Only: Rc => R_center
    Use Global, Only: mu => grav_param
    Implicit None
    Private
    Public :: Period
    Public :: FindTOF
    Public :: Hits_Center
    Public :: SAM
    Public :: SME
    Public :: Radius_of_Apoapsis
    Public :: Radius_of_Periapsis
    Public :: Velocity_of_Periapsis
    Public :: Time_Since_Periapsis
    Public :: Time_to_R
    Public :: Parabolic_TOF
    Public :: RV_from_COEs
    !Public :: Kepler
    !Public :: Kepler_R
    Public :: Kepler_Gooding
    !Public :: Lambert
    Public :: Lambert_minV
    Public :: Lambert_Gooding

    Interface SME
        Module Procedure SME_from_mags
        Module Procedure SME_from_vecs
    End Interface SME
    
    !CANNONICAL UNITS CONVERSIONS
    Real(dp), Parameter :: Rc_per_km = 1._dp / Rc ![Rc/km]
    Real(dp), Parameter :: km_per_Rc = Rc ![km/Rc]
    Real(dp), Parameter :: sec_per_TU = Sqrt(Rc**3 / mu) ![s/TU]
    Real(dp), Parameter :: TU_per_sec = 1._dp / Sqrt(Rc**3 / mu) ![TU/s]
    Real(dp), Parameter :: EpT_per_kps = Rc_per_km * sec_per_TU ![ (Rc*s) / (km*TU) ]
    Real(dp), Parameter :: kps_per_EpT = km_per_Rc * TU_per_sec ![ (km*TU) / (Rc*s) ]
    
Contains

Function SAM(r,v) Result(h)
    Use Kinds, Only: dp
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Cross_Product
    Implicit None
    Real(dp) :: h
    Real(dp), Intent(In) :: r(1:3),v(1:3)
    
    h = Vector_Length(Cross_Product(r,v))
End Function SAM

Function SME_from_mags(r,v) Result(SME)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Implicit None
    Real(dp) :: SME
    Real(dp), Intent(In) :: r,v
    
    SME = 0.5_dp * v**2 - mu / r
End Function SME_from_mags

Function SME_from_vecs(r,v) Result(SME)
    Use Kinds, Only: dp
    Use Utilities, Only: Vector_Length
    Implicit None
    Real(dp) :: SME
    Real(dp), Intent(In) :: r(1:3),v(1:3)
    
    SME = SME_from_mags(Vector_Length(r),Vector_Length(v))
End Function SME_from_vecs

Function Semilatus_Rectum(r,v) Result(p)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Use Utilities, Only: Cross_Product
    Use Utilities, Only: Vector_Length
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: r(1:3),v(1:3)
    
    p = Vector_Length(Cross_Product(r,v))**2 / mu
End Function Semilatus_Rectum

Function Radius_of_Apoapsis(r,v) Result(ra)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Converged
    Implicit None
    Real(dp) :: ra
    Real(dp), Intent(In) :: r(1:3),v(1:3)
    Real(dp) :: p
    Real(dp) :: xi
    
    p = Semilatus_Rectum(r,v)
    xi = SME(r,v) 
    If (xi .LT. 0._dp) Then  !closed orbit
        If (Converged(-2._dp*xi*p,mu)) Then !circular or near-circular orbit
            ra = Vector_Length(r)
        Else
            If (p .GT. 0._dp) Then
                ra = p / (1._dp - Sqrt(1._dp + 2._dp * xi * p / mu))
            Else  !rectilinear case
                ra = 1._dp / Vector_Length(r) - 0.5_dp * Vector_Length(v)**2 / mu
            End If
        End If
    Else  !no-return trajectory, apogee is at infinty
        ra = Huge(ra)
    End If
End Function Radius_of_Apoapsis

Function Radius_of_Periapsis(r,v) Result(rp)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Converged
    Implicit None
    Real(dp) :: rp
    Real(dp), Intent(In) :: r(1:3),v(1:3)
    Real(dp) :: p
    Real(dp) :: xi
    
    p = Semilatus_Rectum(r,v)
    xi = SME(r,v) 
    If (Converged(-2._dp*xi*p,mu)) Then !circular or near-circular orbit
        rp = Vector_Length(r)
    Else
        If (p .GT. 0._dp) Then
            rp = p / (1._dp + Sqrt(1._dp + 2._dp * xi * p / mu))
        Else  !rectilinear case
            rp = 1._dp / Vector_Length(r) - 0.5_dp * Vector_Length(v)**2 / mu
        End If
    End If
End Function Radius_of_Periapsis

Function Velocity_of_Periapsis(r,v) Result(vp)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Converged
    Implicit None
    Real(dp) :: vp
    Real(dp), Intent(In) :: r(1:3),v(1:3)
    Real(dp) :: p,rp
    Real(dp) :: xi
    
    p = Semilatus_Rectum(r,v)
    xi = SME(r,v)
    If (Converged(-2._dp*xi*p,mu)) Then !circular or near-circular orbit
        vp = Vector_Length(v)
    Else
        If (p .GT. 0._dp) Then
            rp = p / (1._dp + Sqrt(1._dp + 2._dp * xi * p / mu))
            vp = Sqrt(2._dp * (xi + mu / rp))
        Else !rectilinear case
            vp = 0._dp
        End If
    End If
End Function Velocity_of_Periapsis

Function Time_to_R(r0,v0,radius,t_min,t_max) Result(t)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: t
    Real(dp), Intent(In) :: r0(1:3)
    Real(dp), Intent(In) :: v0(1:3)
    Real(dp), Intent(In) :: radius
    Real(dp), Intent(In) :: t_min,t_max
    Real(dp) :: t1,t2
    
    t1 = t_min
    t2 = t_max
    t = 0.5_dp * (t1 + t2)
    !attempt Newton's method first
    If (Time_to_R_Newton(r0,v0,radius,t1,t2,t)) Return  !normal exit
    !Newton's method failed to converge on a root within known boundaries, use bisection for a starter
    If (Time_to_R_Bisect(r0,v0,radius,t1,t2,t,starter=.TRUE.)) Then  !bisection has refined the starter
        !attempt Newton again with the better starter
        If (Time_to_R_Newton(r0,v0,radius,t1,t2,t)) Return
    Else  !bisection failed to refine the starter
        Print*,'ERROR:  Astro_Utilities: Time_to_R:  Bisection failed to refine starter'
        ERROR STOP
    End If
    !If Newton failed again, fall back on an exhaustive bisection search
    If (Time_to_R_Bisect(r0,v0,radius,t1,t2,t,starter=.FALSE.)) Then
        Return
    Else
        Print*,'ERROR:  Astro_Utilities: Time_to_R:  Failed to converge on a root.'
        ERROR STOP
    End If
End Function Time_to_R

Function Time_to_R_Newton(r0,v0,radius,t1,t2,t) Result(bingo)
    Use Kinds, Only: dp
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Converged
    Implicit None
    Logical :: bingo
    Real(dp), Intent(In) :: r0(1:3)
    Real(dp), Intent(In) :: v0(1:3)
    Real(dp), Intent(In) :: radius
    Real(dp), Intent(in) :: t1,t2
    Real(dp), Intent(InOut) :: t
    Integer :: i
    Real(dp) :: t_old
    Real(dp) :: r(1:3),v(1:3)
    Real(dp) :: r_mag

    bingo = .FALSE.
    Do i = 1,100
        t_old = t
        Call Kepler_Gooding(r0,v0,t,r,v)
        r_mag = Vector_Length(r)
        t = t_old - (r_mag - radius) / Dot_Product(r/r_mag,v)
        If (t.LT.t1 .OR. t.GT.t2) Return  !Newton's method has diverged from known bounds, unconverged exit
        If (Converged(t,t_old,rTol=1.E-12_dp,aTol=1.E-9_dp)) Then
            bingo = .TRUE.
            Return  !normal exit
        End If
    End Do
End Function Time_to_R_Newton

Function Time_to_R_Bisect(r0,v0,radius,t1,t2,t,starter) Result(bingo)
    Use Kinds, Only: dp
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Converged
    Implicit None
    Logical :: bingo
    Real(dp), Intent(In) :: r0(1:3)
    Real(dp), Intent(In) :: v0(1:3)
    Real(dp), Intent(In) :: radius
    Real(dp), Intent(InOut) :: t1,t2
    Real(dp), Intent(InOut) :: t
    Logical, Intent(In) :: starter
    Real(dp) :: reltol,abstol
    Integer :: i
    Real(dp) :: t_old
    Real(dp) :: r(1:3),v(1:3)

    bingo = .FALSE.
    If (starter) Then
        reltol = 1.E-3_dp
        abstol = 1._dp
    Else
        reltol = 1.E-9_dp
        abstol = 1.E-12_dp
    End If
    t_old = t2
    Do i = 1,2054  !2054 stages is the number of bisections to get from HUGE to TINY in in double precision
        t = 0.5_dp * (t1 + t2)
        If (Converged(t,t_old,rTol=reltol,aTol=abstol)) Then
            bingo = .TRUE.
            Return !normal exit
        End If
        Call Kepler_Gooding(r0,v0,t,r,v) 
        If (Dot_Product(r,v) .GT. 0._dp) Then !ascending trajectory
            If (Vector_Length(r) .GT. radius) Then !time too late
                t2 = t
            Else !time too early
                t1 = t
            End If
        Else !descending trajectory
            If (Vector_Length(r) .GT. radius) Then !time too early
                t1 = t
            Else !time too late
                t2 = t
            End If
        End If
        t_old = t
    End Do
End Function Time_to_R_Bisect

Function Hits_Center(r1,r2,v1,v2)
    Use Kinds, Only: dp
    Use Global, Only: Rc => R_center
    Implicit None
    Logical :: Hits_Center
    Real(dp), Intent(In) :: r1(1:3),r2(1:3)
    Real(dp), Intent(In) :: v1(1:3),v2(1:3)
    
    Hits_Center = .FALSE.
    If (Dot_Product(r1,v1).LT.0._dp .AND. Dot_Product(r2,v2).GT.0._dp) Then
        !periapsis occurs between r1 and r2, check for collision
        If (Radius_of_Periapsis(r1,v1) .LT. Rc) Hits_Center = .TRUE.
    End If
End Function Hits_Center

!-------------------------------------------------------------------------------
!   Orbit Period
!
!   Formulae from:
!   Vallado, D. A. (2001). Fundamentals of Astrodynamics and Applications (2nd
!       ed.). El Segundo, CA: Microcosm Press.
!
!   Translated to modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Function Period(r,v) Result(p)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Use Global, Only: TwoPi
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: r(1:3),v(1:3)
    Real(dp) :: xi,a
    Real(dp), Parameter :: tolerance = 1.E-12_dp

    xi = SME(r,v)
    If (xi .GT. -tolerance) Then !escape or nearly escape trajectory
        p = Huge(p)
    Else !elliptical, period is defined
        a = -0.5_dp * mu / xi
        p = TwoPi * Sqrt(a**3 / mu)
    End If
End Function Period

!-------------------------------------------------------------------------------
!   Time of Flight Between Two Points on an Orbit
!
!   Formulae from:
!   Vallado, D. A. (2001). Fundamentals of Astrodynamics and Applications (2nd
!       ed.). El Segundo, CA: Microcosm Press.
!
!   Translated to modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Function FindTOF(r1_vec,v1_vec,r2_vec) Result(tof)
    Use Kinds, Only: dp
    Use Utilities, Only: Vector_Length
    Use Global, Only: mu => grav_param
    Implicit None
    Real(dp) :: tof    
    Real(dp), Intent(In) :: r1_vec(1:3)
    Real(dp), Intent(In) :: v1_vec(1:3)
    Real(dp), Intent(In) :: r2_vec(1:3)
    Real(dp) :: r1,r2,r1r2
    Real(dp) :: cos_nu,sin_nu,nu
    Real(dp) :: p
    Real(dp) :: k,l,m,alpha
    Real(dp) :: f,g,f_dot
    Real(dp) :: cos_E,sin_E,E
    Real(dp) :: cosh_H,sinh_H,H
    Real(dp) :: s,c
    Real(dp), Parameter :: tolerance = 1.E-12_dp
    Real(dp), Parameter :: two_thirds = 2._dp / 3._dp
    
    r1 = Vector_Length(r1_vec)
    r2 = Vector_Length(r2_vec)
    r1r2 = r1 * r2
    cos_nu = Dot_Product(r1_vec,r2_vec) / r1r2
    sin_nu = Sqrt(1._dp - cos_nu**2)
    k = r1r2 * (1._dp - cos_nu)
    l = r1 + r2
    m = r1 + r2 * (1._dp - cos_nu)
    p = Semilatus_Rectum(r1_vec,v1_vec)
    alpha = ((2._dp * m - l**2) * p**2 + 2._dp * k * l * p - k**2) / (m * k * p)
    f = 1._dp - r1 * (1._dp - cos_nu) / p
    g = r1r2 * sin_nu / Sqrt(mu * p)
    If (alpha .GT. tolerance) Then !elliptical
        nu = Atan2(sin_nu,cos_nu)
        f_dot = Sqrt(mu / p) * Tan(0.5_dp * nu) * ((1._dp - cos_nu) / p - 1._dp/r1 - 1._dp/r2)
        cos_E = 1._dp - alpha * r1 * (1._dp - f)
        sin_E = -r1r2 * f_dot / Sqrt(mu / alpha)
        E = Atan2(sin_E,cos_E)
        tof = g + (E - sin_E) / Sqrt(mu * alpha**3)
    Else If (alpha .LT. -tolerance) Then !hyperbolic
        cosh_H = 1._dp - r1 * alpha * (1._dp - f)
        sinh_H = Sqrt(cosh_H**2 - 1._dp)
        H = Acosh(cosh_H)
        tof = g + (sinh_H - H) / Sqrt(mu * (-alpha)**3)
    Else !parabolic
        c = Sqrt(r1**2 + r2**2 - 2._dp * r1r2 * cos_nu)
        s = 0.5_dp * (r1 + r2 + c)
        tof = two_thirds * Sqrt(0.5_dp * s**3 / mu) * (1._dp - ((s - c) / s)**1.5_dp)
    End If
End Function FindTOF

!-------------------------------------------------------------------------------
!   Time of Flight Since Periapsis
!
!   Formulae from:
!   Vallado, D. A. (2001). Fundamentals of Astrodynamics and Applications (2nd
!       ed.). El Segundo, CA: Microcosm Press.
!
!   Translated to modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Function Time_Since_Periapsis(r_vec,v_vec) Result(t)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Use Global, Only: TwoPi
    Use Utilities, Only: Vector_Length
    Implicit None
    Real(dp) :: t
    Real(dp), Intent(In) :: r_vec(1:3)
    Real(dp), Intent(In) :: v_vec(1:3)
    Real(dp) :: r,v
    Real(dp) :: xi
    Real(dp) :: alpha
    Real(dp) :: e_vec(1:3),e
    Real(dp) :: CosNu,nu
    Real(dp) :: CosE,SinE
    Real(dp) :: SinhH
    Real(dp) :: B,p
    Real(dp) :: rdotv
    Real(dp), Parameter :: tolerance = 1.E-12_dp
    Real(dp), Parameter :: one_third = 1._dp / 3._dp
    
    r = Vector_Length(r_vec)
    v = Vector_Length(v_vec)
    xi = SME(r,v)
    rdotv = Dot_Product(r_vec,v_vec)
    e_vec = (v**2 / mu - 1._dp / r) * r_vec - rdotv * v_vec / mu
    e = Vector_Length(e_vec)
    CosNu = Dot_Product(e_vec,r_vec) / (e * r)
    If (Abs(CosNu) .GT. 1._dp) CosNu = Sign(1._dp,CosNu) !test and correct for precision jitter around CosNu=1
    nu = ACos(CosNu)
    If (rdotv .LT. 0._dp) nu = TwoPi - nu
    alpha = -2._dp * xi / mu
    If (alpha .GT. tolerance) Then !elliptical
        CosE = (e + CosNu) / (1._dp + e * CosNu)
        SinE = Sin(nu) * Sqrt(1._dp - e**2) / (1._dp + e * CosNu)
        t = (ATan2(SinE,CosE) - e * SinE) / Sqrt(mu * alpha**3)
    Else If (alpha .LT. -tolerance) Then !hyperbolic
        SinhH = Sin(nu) * Sqrt(e**2 - 1._dp) / (1._dp + e * CosNu)
        t = (e * SinhH - ASinh(SinhH)) / Sqrt(-mu * alpha**3)
    Else !parabolic
        B = Sin(nu) / (CosNu + 1._dp)
        p = Semilatus_Rectum(r_vec,v_vec)
        t = 0.5_dp * Sqrt(p**3 / mu) * B * (one_third*(B**2) + 1._dp)
    End If
    !TODO Check if following line is needed... use of signed nu above may already correctly sign t
    t = Sign(t,rdotv)
End Function Time_Since_Periapsis

!-------------------------------------------------------------------------------
!   Parabolic Time of Flight
!
!   Formulae from:
!   Vallado, D. A. (2001). Fundamentals of Astrodynamics and Applications (2nd
!       ed.). El Segundo, CA: Microcosm Press.
!
!   Translated to modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Function Parabolic_TOF(r1_vec,r2_vec) Result(tof)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Use Utilities, Only: Vector_Length
    Implicit None
    Real(dp) :: tof
    Real(dp), Intent(In) :: r1_vec(1:3),r2_vec(1:3)
    Real(dp) :: r1,r2
    Real(dp) :: c,s
    Real(dp), Parameter :: two_thirds = 2._dp / 3._dp

    r1 = Vector_Length(r1_vec)
    r2 = Vector_Length(r2_vec)
    c = Sqrt(r1**2 + r2**2 - 2._dp*Dot_Product(r1_vec,r2_vec))
    s = 0.5_dp * (r1 + r2 + c)
    tof = two_thirds * Sqrt(s**3 / (2._dp*mu)) * (1._dp - ((s - c) / s)**(1.5_dp))  !parabolic flight time from r1 to r2
End Function Parabolic_TOF

!-------------------------------------------------------------------------------
!   Position and Velocity from Classical Orbital Elements
!
!   Formulae from:
!   Vallado, D. A. (2001). Fundamentals of Astrodynamics and Applications (2nd
!       ed.). El Segundo, CA: Microcosm Press.
!
!   Translated to modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Subroutine RV_from_COEs(p,e,i,RAAN,AoP,nu,r,v)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Implicit None
    Real(dp), Intent(In) :: p,e,i,RAAN,AoP,nu  !Classical Orbital Elements
    Real(dp), Intent(Out) :: r(1:3),v(1:3)
    Real(dp) :: O,w
    Real(dp) :: pqw_to_ijk(1:3,1:3)
    
    O = RAAN
    w = AoP
    If (e .EQ. 0._dp) Then !circular
        If (i .EQ. 0._dp) Then !circular equatorial
            O = 0._dp
            w = 0._dp
        Else !circular inclined
            w = 0._dp
        End If
    Else If (e .LT. 1._dp) Then !elliptical
        If (i .EQ. 0._dp) Then !circular equatorial
            O = 0._dp
        End If
    End If
    pqw_to_ijk = Reshape( (/  Cos(O)*Cos(w)-Sin(O)*Sin(w)*Cos(i), &
                           &  Sin(O)*Cos(w)+Cos(O)*Sin(w)*Cos(i), &
                           &  Sin(w)*Sin(i), &
                           & -Cos(O)*Sin(w)-Sin(O)*Cos(w)*Cos(i), &
                           & -Sin(O)*Sin(w)+Cos(O)*Cos(w)*Cos(i), &
                           &  Cos(w)*Sin(i), &
                           &  Sin(O)*Sin(i), &
                           & -Cos(O)*Sin(i), &
                           &  Cos(i) /), &
                           & Shape(pqw_to_ijk) )
    r = MatMul(pqw_to_ijk , (/ p * Cos(nu) / (1._dp + e * Cos(nu)) , &
                             & p * Sin(nu) / (1._dp + e * Cos(nu)) , &
                             & 0._dp /) )
    v = MatMul(pqw_to_ijk , (/ -Sqrt(mu / p) * Sin(nu) , &
                             & Sqrt(mu / p) * (e + Cos(nu)) , &
                             & 0._dp /) )
End Subroutine RV_from_COEs

!-------------------------------------------------------------------------------
!   Solution to Kepler's Problem
!
!   Formulae from:
!   Vallado, D. A. (2001). Fundamentals of Astrodynamics and Applications (2nd
!       ed.). El Segundo, CA: Microcosm Press.
!
!   Translated to modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Subroutine Kepler(r0_vec,v0_vec,t,r_vec,v_vec)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: r0_vec(1:3)
    Real(dp), Intent(In) :: v0_vec(1:3)
    Real(dp), Intent(In) :: t
    Real(dp), Intent(Out) :: r_vec(1:3)
    Real(dp), Intent(Out) :: v_vec(1:3)
    Real(dp) :: f,g,f_dot,g_dot
    Real(dp) :: r0_vec_Rc(1:3),v0_vec_Rc(1:3)

    !convert to cannonical units
    r0_vec_Rc = r0_vec * Rc_per_km
    v0_vec_Rc = v0_vec * EpT_per_kps
    !Find f, g, f_dot, and g_dot
    Call Kepler_f_g(r0_vec_Rc,v0_vec_Rc,t*TU_per_sec,f,g,f_dot,g_dot)
    !Compute new r and v, converting back to km and km/s
    r_vec = km_per_Rc * (f*r0_vec_Rc + g*v0_vec_Rc)
    v_vec = kps_per_EpT * (f_dot*r0_vec_Rc + g_dot*v0_vec_Rc)
End Subroutine Kepler

Function Kepler_R(r0_vec,v0_vec,t) Result(r_vec)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: r_vec(1:3)
    Real(dp), Intent(In) :: r0_vec(1:3)
    Real(dp), Intent(In) :: v0_vec(1:3)
    Real(dp), Intent(In) :: t
    Real(dp) :: f,g
    Real(dp) :: r0_vec_Rc(1:3),v0_vec_Rc(1:3)

    !convert to cannonical units
    r0_vec_Rc = r0_vec * Rc_per_km
    v0_vec_Rc = v0_vec * EpT_per_kps
    !Find f and g
    Call Kepler_f_g(r0_vec_Rc,v0_vec_Rc,t*TU_per_sec,f,g)
    !Compute new r converting back to km
    r_vec = km_per_Rc * (f*r0_vec_Rc + g*v0_vec_Rc)
End Function Kepler_R

Subroutine Kepler_f_g(r0_vec,v0_vec,t,f,g,f_dot,g_dot)
    !INPUTS & OUTPUTS ARE IN CANONICAL UNITS
    Use Kinds, Only: dp
    Use Global, Only: TwoPi
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Cross_Product
    Implicit None
    Real(dp), Intent(In) :: r0_vec(1:3)
    Real(dp), Intent(In) :: v0_vec(1:3)
    Real(dp), Intent(In) :: t
    Real(dp), Intent(Out) :: f
    Real(dp), Intent(Out) :: g
    Real(dp), Intent(Out), Optional :: f_dot
    Real(dp), Intent(Out), Optional :: g_dot
    Real(dp) :: r0,v0,alpha
    Real(dp) :: dt,x,dx,r_dot_v,z,C,S
    Real(dp) :: zS,zC
    Real(dp) :: fx,dfx,d2fx
    Real(dp), Parameter :: tolerance = 1.E-12_dp
    
    r0 = Vector_Length(r0_vec)
    v0 = Vector_Length(v0_vec)
    alpha = (2._dp / r0) - v0**2
    r_dot_v = Dot_Product(r0_vec,v0_vec)
    If (alpha .GT. tolerance) Then  !elliptical or circular
        dt = Mod(t,TwoPi / Sqrt(alpha**3))
        x = dt * alpha
    Else If (alpha .LT. -tolerance) Then  !hyperbolic
        dt = t
        x = Sign(1._dp,dt) * Log( (-2._dp*alpha*dt*Sqrt(-alpha)) / Abs(r_dot_v+Sign(1._dp,dt)*(1._dp-r0*alpha)) ) / Sqrt(-alpha)
    Else !parabolic, near-parabolic
        dt = t
        x = Vector_Length(Cross_Product(r0_vec,v0_vec)) * Tan(0.5_dp * v0)
    End If
    Do
        z = alpha * x**2
        Call Find_C_S(z,C,S)
        zS = 1._dp - z*S
        zC = 1._dp - z*C
        !LAGUERRE'S METHOD (with n = 5)
        fx = dt - x*( x*( x*S + r_dot_v*C ) + r0*zS )
        dfx = -(x * (C*x + r_dot_v*zS) + r0*zC)
        d2fx = -(r_dot_v*zC + zS*x*(1._dp - r0*alpha))
        dx = -(5._dp * fx / (dfx + Sign(Sqrt(Abs(16._dp*dfx**2 - 20._dp*fx*d2fx)),dfx)))
        x = x + dx
        !N2H Evaluate convergence criteria for Vallados Kepler algorithm
        If (Abs(dx) .LT. tolerance) Exit
    End Do
    f = 1._dp - x**2 * C / r0
    g = dt - x**3 * S
    If (Present(f_dot)) f_dot = x * zS / (dfx*r0)
    If (Present(g_dot)) g_dot = 1._dp + x**2 * C / dfx
End Subroutine Kepler_f_g

!-------------------------------------------------------------------------------
!   Minimum Velocity Solution to Lambert's Problem
!
!   Formulae from:
!   Vallado, D. A. (2001). Fundamentals of Astrodynamics and Applications (2nd
!       ed.). El Segundo, CA: Microcosm Press.
!
!   Translated to modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Subroutine Lambert_minV(r1_vec,r2_vec,v1_vec,tof)
    Use Kinds, Only: dp
    Use Global, Only: Pi
    Use Global, Only: mu => grav_param
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Unit_Vector
    Implicit None
    Real(dp), Intent(In) :: r1_vec(1:3)
    Real(dp), Intent(In) :: r2_vec(1:3)
    Real(dp), Intent(Out), Optional :: v1_vec(1:3)
    Real(dp), Intent(Out), Optional :: tof
    Real(dp) :: r1,r2,r1r2
    Real(dp) :: cos_nu,p
    Real(dp) :: c,s,B
    
    r1 = Vector_Length(r1_vec)
    r2 = Vector_Length(r2_vec)
    r1r2 = r1 * r2
    cos_nu = Dot_Product(r1_vec,r2_vec) / (r1r2)
    c = Sqrt(r1**2 + r2**2 - 2._dp * r1r2 * cos_nu)
    If (Present(v1_vec)) Then
        If (cos_nu .NE. 1._dp) Then
            p = r1r2 * (1._dp - cos_nu) / c
            v1_vec = Sqrt(mu * p) * (r2_vec - (1._dp - r2 * (1._dp - cos_nu) / p) * r1_vec) / (r1r2 * Sqrt(1._dp - cos_nu**2))
        Else  !Rectilinear orbit (straight up)
            v1_vec = Sqrt(2._dp * mu * Abs(1._dp/r1 - 1._dp/r2)) * Unit_Vector(r2_vec - r1_vec)
        End If
    End If
    If (Present(tof)) Then
        s = 0.5_dp * (r1 + r2 + c)
        B = 2._dp * Asin( Sqrt((s - c) / s) )
        tof = Sqrt((0.5_dp * s)**3 / mu) * (Pi - B + Sin(B))
    End If
End Subroutine Lambert_minV

!-------------------------------------------------------------------------------
!   Solution to Lambert's Problem
!
!   Formulae from:
!   Vallado, D. A. (2001). Fundamentals of Astrodynamics and Applications (2nd
!       ed.). El Segundo, CA: Microcosm Press.
!
!   Translated to modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Pure Subroutine Lambert(r0_vec,r_vec,tof,v0_vec,v_vec,long_way)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: r0_vec(1:3)
    Real(dp), Intent(In) :: r_vec(1:3)
    Real(dp), Intent(In) :: tof
    Real(dp), Intent(Out) :: v0_vec(1:3)
    Real(dp), Intent(Out) :: v_vec(1:3)
    Logical, Intent(In), Optional :: long_way
    Real(dp) :: f,g,g_dot
    Real(dp) :: r0_vec_Rc(1:3),r_vec_Rc(1:3)
    Logical :: tm

    If (Present(long_way)) Then
        tm = long_way
    Else  !default is short way
        tm = .FALSE.
    End If
    !convert to cannonical units
    r0_vec_Rc = r0_vec * Rc_per_km
    r_vec_Rc = r_vec * Rc_per_km
    !Find f, g, and g_dot
    Call Lambert_f_g(r0_vec_Rc,r_vec_Rc,tof*TU_per_sec,tm,f,g,g_dot)
    !Compute new v0 and v, converting back to km/s
    v0_vec = kps_per_EpT * (r_vec_Rc - f*r0_vec_Rc) / g
    v_vec = kps_per_EpT * (g_dot*r_vec_Rc - r0_vec_Rc) / g
End Subroutine Lambert

Pure Subroutine Lambert_f_g(r0_vec,r_vec,t,long_way,f,g,g_dot)
    !INPUTS & OUTPUTS ARE IN CANONICAL UNITS
    Use Kinds, Only: dp
    Use Global, Only: TwoPi
    Use Utilities, Only: Vector_Length
    Implicit None
    Real(dp), Intent(In) :: r0_vec(1:3)
    Real(dp), Intent(In) :: r_vec(1:3)
    Real(dp), Intent(In) :: t
    Logical, Intent(In) :: long_way
    Real(dp), Intent(Out) :: f
    Real(dp), Intent(Out) :: g
    Real(dp), Intent(Out) :: g_dot
    Real(dp) :: r0,r
    Real(dp) :: z,C,S
    Real(dp) :: z_min,z_max
    Real(dp) :: y,A,ti
    Real(dp), Parameter :: tolerance = 1.E-9_dp
    
    r0 = Vector_Length(r0_vec)
    r = Vector_Length(r_vec)
    A = Sqrt(r * r0 + Dot_Product(r0_vec,r_vec))
    If (A .LT. tolerance) Then
        !when A=0, this is a near-180deg transfer, universal variable formulation does not have unique solution,
        !use Battin's method instead assuming an elliptical transfer
        Call Lambert_f_g_Battin(r0_vec,r_vec,t,long_way,.TRUE.,f,g,g_dot)
        Return
    End If
    If (long_way) A = -A
    z = 0._dp
    z_max = TwoPi**2
    z_min = -TwoPi**2
    Do
        Do
            Call Find_C_S(z,C,S)
            y = r0 + r + A*(z*S - 1._dp) / Sqrt(C)
            If (y .GT. 0._dp) Exit
            z = 0.8_dp * (1._dp - Sqrt(C)*(r0+r)/A) / S
        End Do
        ti = Sqrt(y) * (A + y * S / (Sqrt(C)**3))
        If (Abs(t-ti) .LT. tolerance) Exit
        If (ti .LT. t) Then
            z_min = z
        Else
            z_max = z
        End If
        z = 0.5_dp * (z_min + z_max)
    End Do
    f = 1._dp - y / r0
    g = A * Sqrt(y)
    g_dot = 1._dp - y / r
End Subroutine Lambert_f_g

Pure Subroutine Find_C_S(z,C,S)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: z
    Real(dp), Intent(Out) :: C,S
    Real(dp) :: sqrt_Z
    Real(dp), Parameter :: series_thresh = 3.1732_dp  !Threshold below which to use series representation, preserves 15 digits
    
    If (Abs(z) .GT. series_thresh) Then  !direct evaluation of formulae
        If (z .GT. 0._dp) Then
            sqrt_Z = Sqrt(z)
            C = (1._dp - Cos(sqrt_Z)) / z
            S = (sqrt_Z - Sin(sqrt_Z)) / (z * sqrt_Z)
        Else
            sqrt_Z = Sqrt(-z)
            C = (1._dp - Cosh(sqrt_Z)) / z
            S = (Sinh(sqrt_Z) - sqrt_Z) / (-z * sqrt_Z)
        End If
    Else !evaluate by series
        Call Find_C_S_by_series(z,C,S)
    End If
End Subroutine Find_C_S

Pure Subroutine Find_C_S_by_series(z,C,S)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: z
    Real(dp), Intent(Out) :: C,S
    Real(dp) :: zi,Ci,Si,a
    Integer :: i,b
    Real(dp), Parameter :: one_sixth = 1._dp / 6._dp
    Real(dp), Parameter :: tol = 1.E-15_dp

    C = 0.5_dp
    S = one_sixth
    zi = -z
    a = one_sixth
    b = 3
    !add up to ten terms, checking for convergence along the way
    Do i = 1,10
        b = b + 1
        a = a / Real(b,dp)
        Ci = zi * a
        C = C + Ci
        b = b + 1
        a = a / Real(b,dp)
        Si = zi * a
        S = S + Si
        If (Abs(Ci) .LT. Abs(C)*tol .AND. Abs(Si) .LT. Abs(S)*tol) Exit
        zi = zi * (-z)
    End Do
End Subroutine Find_C_S_by_series

!-------------------------------------------------------------------------------
!   Solution to Lambert's Problem, Battin's Method
!
!   Formulae from:
!   Vallado, D. A. (2001). Fundamentals of Astrodynamics and Applications (2nd
!       ed.). El Segundo, CA: Microcosm Press.
!
!   Translated to modern Fortran by Whitman Dailey.  Jun 02, 2020.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Pure Subroutine Lambert_Battin(r0_vec,r_vec,tof,v0_vec,v_vec,long_way,ellipse)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: r0_vec(1:3)
    Real(dp), Intent(In) :: r_vec(1:3)
    Real(dp), Intent(In) :: tof
    Real(dp), Intent(Out) :: v0_vec(1:3)
    Real(dp), Intent(Out) :: v_vec(1:3)
    Logical, Intent(In), Optional :: long_way
    Logical, Intent(In), Optional :: ellipse
    Real(dp) :: f,g,g_dot
    Real(dp) :: r0_vec_Rc(1:3),r_vec_Rc(1:3)
    Logical :: tm,te

    If (Present(long_way)) Then
        tm = long_way
    Else  !default is short way
        tm = .FALSE.
    End If
    If (Present(ellipse)) Then
        te = ellipse
    Else  !default is elliptical transfer
        te = .TRUE.
    End If
    !convert to cannonical units
    r0_vec_Rc = r0_vec * Rc_per_km
    r_vec_Rc = r_vec * Rc_per_km
    !Find f, g, and g_dot
    Call Lambert_f_g_Battin(r0_vec_Rc,r_vec_Rc,tof*TU_per_sec,tm,te,f,g,g_dot)
    !Compute new v0 and v, converting back to km/s
    v0_vec = kps_per_EpT * (r_vec_Rc - f*r0_vec_Rc) / g
    v_vec = kps_per_EpT * (g_dot*r_vec_Rc - r0_vec_Rc) / g
End Subroutine Lambert_Battin

Pure Subroutine Lambert_f_g_Battin(r0_vec,r1_vec,tof,long_way,ellip_trans,f,g,g_dot)
    !INPUTS & OUTPUTS ARE IN CANONICAL UNITS
    Use Kinds, Only: dp
    Use Global, Only: Pi
    Use Global, Only: TwoPi
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Converged
    Implicit None
    Real(dp), Intent(In) :: r0_vec(1:3)
    Real(dp), Intent(In) :: r1_vec(1:3)
    Real(dp), Intent(In) :: tof
    Logical, Intent(In) :: long_way
    Logical, Intent(In) :: ellip_trans
    Real(dp), Intent(Out) :: f
    Real(dp), Intent(Out) :: g
    Real(dp), Intent(Out) :: g_dot
    Real(dp) :: r0,r1,r0r1,r1dr0
    Real(dp) :: cos_nu,sin_nu,nu
    Real(dp) :: c,s,eps
    Real(dp) :: tanSq2w,cosSqQtrNu,sinSqQtrNu
    Real(dp) :: rop,m,l
    Real(dp) :: x,x_old
    Real(dp) :: eta,c_eta
    Real(dp) :: d,d_old,t,t_old,sig
    Integer :: i
    Real(dp) :: xi,h,h1,h2
    Real(dp) :: b,u,k,y,c_u
    Real(dp) :: a,Ae,Be,a_min,t_min,dE
    Real(dp) :: Ah,Bh,dH
    Real(dp), Parameter :: tolerance = 1.E-12_dp
    Real(dp), Parameter :: one_third = 1._dp / 3._dp
    Real(dp), Parameter :: four_27ths = 4._dp / 27._dp

    r0 = Vector_Length(r0_vec)
    r1 = Vector_Length(r1_vec)
    r0r1 = r0 * r1
    r1dr0 = r1 / r0
    cos_nu = Dot_Product(r0_vec,r1_vec) / r0r1
    sin_nu = Sqrt(1._dp - cos_nu**2)
    If (long_way) sin_nu = -sin_nu
    nu = ATAN2(sin_nu,cos_nu)
    c = Sqrt(r0**2 + r1**2 - 2._dp * r0r1 * cos_nu)
    s = 0.5_dp * (r0 + r1 + c)
    eps = (r1 - r0) / r0
    tanSq2w = 0.25_dp * eps**2 / (Sqrt(r1dr0) + r1dr0*(2._dp + Sqrt(r1dr0)))
    cosSqQtrNu = Cos(0.25_dp*nu)**2
    sinSqQtrNu = Sin(0.25_dp*nu)**2
    rop = Sqrt(r0r1) * (cosSqQtrNu + tanSq2w)
    If (long_way) Then
        l = (cosSqQtrNu + tanSq2w - Cos(0.5_dp*nu)) / (cosSqQtrNu + tanSq2w)
    Else
        l = (sinSqQtrNu + tanSq2w) / (sinSqQtrNu + tanSq2w + Cos(0.5_dp*nu))
    End If
    m = mu * tof**2 / (8._dp * rop**3)
    If (ellip_trans) Then
        x = l
    Else
        x = 0._dp
    End If
    Do
        x_old = x
        eta = x / ( Sqrt(1._dp+x) + 1._dp )**2
        d_old = 1._dp
        t_old = 0.2_dp
        sig = t_old
        i = 3
        Do
            c_eta = Real( i**2 , dp ) / Real( ((2*i)**2)-1 , dp )
            d = 1._dp / (1._dp + c_eta * eta * d_old)
            t = t_old * (d - 1._dp)
            sig = sig + t
            If (Abs(t) .LT. tolerance) Exit
            d_old = d
            t_old = t
            i = i + 1
        End Do
        xi = 1._dp / ( (1._dp / (8._dp * (1._dp + Sqrt(1._dp+x))) ) * ( 3._dp + sig / ( 1._dp + eta*sig ) ) )
        h = 1._dp / ( (1._dp + 2._dp*x + l) * (3._dp + x*(1._dp + 4._dp*xi)) )
        h1 = ((l + x))**2 * (1._dp + xi * (1._dp + 3._dp*x)) * h
        h2 = m * (1._dp - xi * (x - l)) * h
        b = 27._dp * h2 / (4._dp*((1._dp + h1)**3))
        u = -b / (2._dp * (Sqrt(1._dp + b) + 1._dp))
        d_old = 1._dp / (1._dp + four_27ths * u * one_third)
        t_old = one_third * (d_old - 1._dp)
        k = one_third + t_old
        i = 1
        Do
            If (Mod(i,2) .EQ. 0) Then !even i
                c_u = Real( 2*(3*i+1)*(6*i-1) , dp) / Real( 9*(4*i-1)*(4*i+1) , dp)
            Else !odd i
                c_u = Real( 2*(3*i+2)*(6*i+1) , dp) / Real( 9*(4*i+1)*(4*i+3) , dp)
            End If
            d = 1._dp / (1._dp + c_u * u * d_old)
            t = t_old * (d - 1._dp)
            k = k + t
            If (Abs(t) .LT. tolerance) Exit
            d_old = d
            t_old = t
            i = i + 1
        End Do
        y = one_third * (1._dp + h1) * (2._dp + Sqrt(1._dp + b) / (1._dp - 2._dp * u * (k**2) ))
        x = Sqrt((0.5_dp * (1._dp - l))**2 + (m / (y**2))) - 0.5_dp * (1._dp + l)
        If (Converged(x,x_old)) Exit
    End Do
    a = tof**2 / (16._dp * rop**2 * x * y**2)
    If (a .GT. 0._dp) Then
        Be = ACos(1._dp - (s - c) / a)
        If (long_way) Be = -Be
        a_min = 0.5_dp * s
        t_min = Sqrt(a_min**3) * (Pi - Be + Sin(Be))
        Ae = ACos(1._dp - s / a)
        If (tof .GT. t_min) Ae = TwoPi - Ae
        dE = Ae - Be
        f = 1._dp - a * (1._dp - Cos(dE)) / r0
        g = tof - Sqrt(a**3) * (dE - Sin(dE))
        g_dot = 1._dp - a * (1._dp - Cos(dE)) / r1
    Else
        Bh = ACosh(1._dp - (s - c) / a )
        Ah = ACosh(1._dp - s / a)
        dH = Ah - Bh
        f = 1._dp - a * (1._dp - Cosh(dH)) / r0
        g = tof - Sqrt(-a**3) * (Sinh(dH) - dH)
        g_dot = 1._dp - a * (1._dp - Cosh(dH)) / r1
    End If
End Subroutine Lambert_f_g_Battin

!-------------------------------------------------------------------------------
!   Gooding's Method for Lambert's Problem
!
!   Original FORTRAN77 Source from:
!   Gooding, R. H. (1990). A Procedure for the Solution of Lambert's Oribital
!       Boundary-Value Problem. Celestial Mechanics and Dynamical Astronomy,
!       48, 145-165.
!
!   Revised for modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Pure Subroutine Lambert_Gooding(r1_vec,r2_vec,tof,v1,v2,long_way)
    Use Kinds, Only: dp
    Use Global, Only: TwoPi
    Use Utilities, Only: Vector_Length
    Use Utilities, Only: Cross_Product
    Use Utilities, Only: Unit_Vector
    implicit none
    Real(dp), Intent(In) :: r1_vec(1:3)  !! first cartesian position [km]
    Real(dp), Intent(In) :: r2_vec(1:3)  !! second cartesian position [km]
    Real(dp), Intent(In) :: tof          !! time of flight [sec]
    Real(dp), Intent(Out) :: v1(1:3)     !! cartesian velocity at r1
    Real(dp), Intent(Out) :: v2(1:3)     !! cartesian velocity at r2
    Logical, Intent(In), Optional :: long_way   !! when true, do "long way" (>pi) transfers
    Logical :: tm
    Real(dp) :: pa,ta,r1,r2
    Real(dp),dimension(3)   :: r1_hat,r2_hat,r1xr2,rho,r1xr2_hat,eta1,eta2
    Real(dp) :: vr1,vt1,vr2,vt2

    If (Present(long_way)) Then
        tm = long_way
    Else  !default is short way
        tm = .FALSE.
    End If
    r1 = Vector_Length(r1_vec)
    r2 = Vector_Length(r2_vec)
    r1_hat = r1_vec / r1
    r2_hat = r2_vec / r2
    r1xr2 = Cross_Product(r1_vec,r2_vec)
    If (All(r1xr2 .EQ. 0._dp)) Then  !the vectors are parallel, so the transfer plane is undefined
        !when A=0, this is a near-180deg transfer, universal variable formulation does not have unique solution,
        !use Battin's method instead assuming an elliptical transfer
        Call Lambert_Battin(r1_vec,r2_vec,tof,v1,v2,long_way,.TRUE.)
        Return
        !r1xr2 = (/ 0._dp , 0._dp , 1._dp /)    !degenerate conic...choose the x-y plane
    End If
    r1xr2_hat = Unit_Vector(r1xr2)
    !a trick to make sure argument is between [-1 and 1]:
    pa = Acos(Max(-1._dp,Min(1._dp,Dot_Product(r1_hat,r2_hat))))
    !transfer angle and normal vector:
    If (tm) Then ! greater than pi
        ta  =  TwoPi - pa
        rho = -r1xr2_hat
    Else ! less than pi
        ta  = pa
        rho = r1xr2_hat
    End If
    eta1 = Cross_Product(rho,r1_hat)
    eta2 = Cross_Product(rho,r2_hat)
    !Call Gooding
    call vlamb(r1,r2,ta,tof,vr1,vt1,vr2,vt2)
    !convert transverse and radial velocities to inertial vectors
    v1 = vr1 * r1_hat + vt1 * eta1
    v2 = vr2 * r2_hat + vt2 * eta2
End Subroutine Lambert_Gooding

Pure Subroutine vlamb(r1,r2,th,tdelt,vr1,vt1,vr2,vt2)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Implicit None
    Real(dp), Intent(In) :: r1
    Real(dp), Intent(In) :: r2
    Real(dp), Intent(In) :: th
    Real(dp), Intent(In) :: tdelt
    Real(dp), Intent(Out) :: vr1
    Real(dp), Intent(Out) :: vt1
    Real(dp), Intent(Out) :: vr2
    Real(dp), Intent(Out) :: vt2
    Real(dp) :: thr2,r1r2th,csq,c,s,gms,qsqfm1,q,rho,sig,t,x!,unused
    Real(dp) :: qzminx,qzplx,zplqx
    Real(dp) :: dr,r1r2

    thr2 = 0.5_dp * th
    dr = r1 - r2
    r1r2 = r1 * r2
    r1r2th = 4._dp * r1r2 * Sin(thr2)**2
    csq = dr * dr + r1r2th
    c = Sqrt(csq)
    s = 0.5_dp * (r1 + r2 + c)
    gms = Sqrt(0.5_dp * mu * s)
    qsqfm1 = c / s
    q = Sqrt(r1r2) * Cos(thr2) / s
    If (c .NE. 0._dp) Then
        rho = dr / c
        sig = r1r2th / csq
    Else
        rho = 0._dp
        sig = 1._dp
    End If
    t = 4._dp * gms * tdelt / s**2
    Call xLamb(q,qsqfm1,t,x)
    Call tLamb_from_vLamb(q,qsqfm1,x,qzminx,qzplx,zplqx)
    vt2 = gms * zplqx * Sqrt(sig)
    vr1 = gms * (qzminx - qzplx * rho) / r1
    vt1 = vt2 / r1
    vr2 = -gms * (qzminx + qzplx * rho) / r2
    vt2 = vt2 / r2
End Subroutine vLamb

Pure Subroutine xLamb(q,qsqfm1,tin,x)
    Use Kinds, Only: dp
    Use Global, Only: Pi
    Implicit None
    Real(dp), Intent(In)  :: q
    Real(dp), Intent(In)  :: qsqfm1
    Real(dp), Intent(In)  :: tin
    Real(dp), Intent(Out) :: x
    Integer  :: i
    Real(dp) :: thr2,t0,dt,d2t
    Real(dp) :: tdiff,w,t

    thr2 = Atan2(qsqfm1 , 2._dp * q) / Pi
    !single-rev starter from t (at x = 0) & bilinear (usually)
    Call tLamb_from_xLamb1(q,qsqfm1,t0)
    tdiff = tin - t0
    If (tdiff .LE. 0._dp) Then
        x = t0 * tdiff / (-4._dp * tin)
        !(-4 is the value of dt, for x = 0)
    Else
        x = -tdiff / (tdiff + 4._dp)
        w = x + 1.7_dp * Sqrt(2._dp * (1._dp - thr2))
        If (w .LT. 0._dp) x = x - Sqrt(Sqrt(Sqrt(Sqrt(-w)))) * (x + Sqrt(tdiff / (tdiff + 1.5_dp * t0)))
        w = 4._dp / (4._dp + tdiff)
        x = x * (1._dp + x * (0.5_dp * w - 0.03_dp * x * Sqrt(w)))
    End If
    !(now have a starter, so proceed by halley)
    Do i = 1,3
        Call tLamb_from_xLamb2(q,qsqfm1,x,t,dt,d2t)
        t = tin - t
        If (dt .NE. 0._dp) x = x + t * dt / (dt * dt + 0.5_dp * t * d2t)
    End Do
End Subroutine xLamb

Pure Subroutine tLamb_from_vLamb(q,qsqfm1,x,dt,d2t,d3t)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: q
    Real(dp), Intent(In) :: qsqfm1
    Real(dp), Intent(In) :: x
    Real(dp), Intent(Out) :: dt
    Real(dp), Intent(Out) :: d2t
    Real(dp), Intent(Out) :: d3t
    Real(dp) :: qsq,xsq,u,z,qx,a,b,aa,bb

    qsq = q * q
    xsq = x * x
    u = (1._dp - x) * (1._dp + x)
    z = Sqrt(qsqfm1 + qsq * xsq)
    qx = q * x
    If (qx .LE. 0._dp) Then
        a = z - qx
        b = q * z - x
        If (qx .LT. 0._dp) then
            aa = qsqfm1 / a
            bb = qsqfm1 * (qsq * u - xsq) / b
        End If
    End If
    If (qx .GE. 0._dp) Then
        aa = z + qx
        bb = q * z + x
        If (qx .GT. 0._dp) Then
            a = qsqfm1 / aa
            b = qsqfm1 * (qsq * u - xsq) / bb
        End If
    End If
    dt = b
    d2t = bb
    d3t = aa
End Subroutine tLamb_from_vLamb

Pure Subroutine tLamb_from_xLamb1(q,qsqfm1,t)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: q
    Real(dp), Intent(In) :: qsqfm1
    Real(dp), Intent(Out) :: t
    Real(dp) :: z

    z = Sqrt(qsqfm1 + q*q)
    t = Atan2(z, q)
    t = 2._dp * (t + q*z)
End Subroutine tLamb_from_xLamb1

Pure Subroutine tLamb_from_xLamb2(q,qsqfm1,x,t,dt,d2t)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: q
    Real(dp), Intent(In) :: qsqfm1
    Real(dp), Intent(In) :: x
    Real(dp), Intent(Out) :: t
    Real(dp), Intent(Out) :: dt,d2t
    Integer :: i
    Real(dp) :: qsq,xsq,u,y,z
    Real(dp) :: qx,a,b,aa,bb,g,f,fg1,term,fg1sq,twoi1
    Real(dp) :: qz3,u0i,u1i,u2i
    Real(dp) :: tq,tqsum,ttmold,p,tterm,tqterm
    Real(dp) :: deltat
    Real(dp), Parameter :: sw  = 0.4_dp

    qsq = q * q
    xsq = x * x
    u = (1._dp - x) * (1._dp + x)
    dt = 0._dp
    d2t = 0._dp
    If (x.LT.0._dp .OR. Abs(u).GT.sw) Then  !direct computation (not series)
        y = Sqrt(Abs(u))
        z = Sqrt(qsqfm1 + qsq * xsq)
        qx = q * x
        If (qx .LE. 0._dp) Then
            a = z - qx
            b = q * z - x
        Else !(qx .GT. 0._dp)
            aa = z + qx
            bb = q * z + x
            a = qsqfm1 / aa
            b = qsqfm1 * (qsq * u - xsq) / bb
        End If
        If (qx * u .GE. 0._dp) Then
            g = x * z + q * u
        Else
            g = (xsq - qsq * u) / (x * z - q * u)
        End If
        f = a * y
        If (x .LE. 1._dp) Then
            t = Atan2(f, g)
        Else
            If (f .GT. sw) Then
                t = Log(f + g)
            Else
                fg1 = f / (g + 1._dp)
                term = 2._dp * fg1
                fg1sq = fg1 * fg1
                t = term
                twoi1 = 1._dp
                Do
                    twoi1 = twoi1 + 2._dp
                    term = term * fg1sq
                    deltat = term / twoi1
                    t = t + deltat
                    If (Abs(deltat) .LT. 1.E-16_dp*t) Exit
                End Do    !(continue looping for inverse tanh)
            End If
        End If
        t = 2._dp * (t / y + b) / u
        If (z .NE. 0._dp) Then
            dt = (3._dp * x * t - 4._dp * (a + qx * qsqfm1) / z) / u
            qz3 = (q / z)**3
            d2t = (3._dp * t + 5._dp * x * dt + 4._dp * qz3 * qsqfm1) / u
        End If
    Else  !compute by series
        u0i = 1._dp
        u1i = 1._dp
        u2i = 1._dp
        term = 4._dp
        tq = q * qsqfm1
        i = 0
        If (q .LT. 0.5_dp) Then
            tqsum = 1._dp - q * qsq
        Else !q .GE. 0.5_wp
            tqsum = (1._dp / (1._dp + q) + q) * qsqfm1
        End If
        ttmold = term / 3._dp
        t = ttmold * tqsum
        Do
            i = i + 1
            p = Real(i,dp)
            u0i = u0i * u
            If (i .GT. 1) u1i = u1i * u
            If (i .GT. 2) u2i = u2i * u
            term = term * (p - 0.5_dp) / p
            tq = tq * qsq
            tqsum = tqsum + tq
            tterm = term / (2._dp * p + 3._dp)
            tqterm = tterm*tqsum
            deltat = -u0i * ((1.5_dp * p + 0.25_dp) * tqterm / (p * p - 0.25_dp) - ttmold * tq)
            t = t + deltat
            ttmold = tterm
            tqterm = tqterm * p
            dt = dt + tqterm * u1i
            d2t = d2t + tqterm * u2i * (p - 1._dp)
            If (i.GE.2 .AND. Abs(deltat).LT.1.E-16*t) Exit
        End Do
        d2t = 2._dp * (2._dp * xsq * d2t - dt)
        dt = -2._dp * x * dt
        t = t / xsq
    End If
End Subroutine tLamb_from_xLamb2

!-------------------------------------------------------------------------------
!   Gooding's Method for Kepler's Problem
!
!   Original FORTRAN77 Source from:
!   Gooding, R. H., & Odell, A. W. (1986). Procedures for solving Kepler's 
!       Equation. Celestial Mechanics, 38(4), 307-334.
!   Gooding, R. H., & Odell, A. W. (1988). The Hyperbolic Kepler Equation (and
!       the Elliptic Equation Revisited). Celestial Mechanics, 44(3), 267-282.
!
!   Revised for modern Fortran by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
Pure Subroutine Kepler_Gooding(r0,v0,dt,r1,v1)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: r0(1:3)
    Real(dp), Intent(In) :: v0(1:3)
    Real(dp), Intent(In) :: dt
    Real(dp), Intent(Out) :: r1(1:3)
    Real(dp), Intent(Out) :: v1(1:3)
    Real(dp) :: els(1:6)

    !convert to Gooding's elements
    Call pv3els(r0,v0,els)
    !increment time
    els(6) = els(6) + dt
    !convert back to cartesian
    Call els3pv(els,r1,v1)
End Subroutine Kepler_Gooding

Pure Subroutine pv3els(r0,v0,els)
    Use Kinds, Only: dp
    Use Global, Only: HalfPi
    Implicit None
    Real(dp), Intent(In) :: r0(1:3)
    Real(dp), Intent(In) :: v0(1:3)
    Real(dp), Intent(Out) :: els(1:6)

    Real(dp) :: x,y,z
    Real(dp) :: xdot,ydot,zdot
    Real(dp) :: al,q,ei,bom,om,tau
    Real(dp) :: xsqysq,rsq,r,vr,hx,hy,hz,hsq,u,vt,bx,by,bz,w,h

    els = 0._dp
    If (All(r0 .EQ. 0._dp) .AND. All(v0 .EQ. 0._dp)) Return
    x    = r0(1)
    y    = r0(2)
    z    = r0(3)
    xdot = v0(1)
    ydot = v0(2)
    zdot = v0(3)
    xsqysq = x*x + y*y
    rsq = xsqysq + z*z
    r = Sqrt(rsq)
    vr = (x*xdot + y*ydot + z*zdot) / r
    hx = y*zdot - z*ydot
    hy = z*xdot - x*zdot
    hz = x*ydot - y*xdot
    hsq = hx*hx + hy*hy + hz*hz
    If (hsq .EQ. 0._dp) Then  !rectilinear orbit
        ei = HalfPi
        If (xsqysq .EQ. 0._dp) Then  !axial orbit
            bom = 0._dp
        Else  !general rectilinear orbit
            bom = Atan2(y, x)
        End If
        u = Atan2(z, Sqrt(xsqysq))
        vt = 0._dp
    Else  !non-degenerate orbit
        bx = hy*z - hz*y
        by = hz*x - hx*z
        bz = hx*y - hy*x
        hx = y*bz - z*by
        hy = z*bx - x*bz
        hz = x*by - y*bx
        w = hx*hx + hy*hy
        h = Sqrt(w + hz*hz)
        ei = Atan2(Sqrt(w), hz)
        If (w .EQ. 0._dp) Then  !orbit in reference plane
            bom = 0._dp
            u = Atan2(y*Sign(1._dp,hz), x)
        Else  !general orbit
            bom = Atan2(hx, -hy)
            u = Atan2(h*z, rsq*bz)
        End If
        vt = h / (r*rsq)
    End If
    Call pv2els (r, u, vr, vt, al, q, om, tau)
    els(1) = al
    els(2) = q
    els(3) = ei
    els(4) = bom
    els(5) = om
    els(6) = tau
End Subroutine pv3els

Pure Subroutine pv2els (r, u, vr, vt, al, q, om, tau)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Implicit None
    Real(dp), Intent(In) :: r     !! radial distance [km]
    Real(dp), Intent(In) :: u     !! angle from assumed reference direction [rad]
    Real(dp), Intent(In) :: vr    !! radial velocity [km/2]
    Real(dp), Intent(In) :: vt    !! transverse velocity >=0 [km/s]
    Real(dp), Intent(Out) :: al    !! alpha: gm/a [km^2/s^2]
    Real(dp), Intent(Out) :: q     !! periapsis distance [km]
    Real(dp), Intent(Out) :: om    !! argument of periapsis relative to reference direction [rad]
    Real(dp), Intent(Out) :: tau   !! time from periapsis [sec]
    Real(dp) :: esq1,es,eses,ec,ecec,esq,e,v,e1
    Real(dp) :: eh,em,ecesq,en,vsq,rtal,d,h,p,alp
    Real(dp), Parameter :: one_sixth = 1._dp / 6._dp
    
    !(all orbits)
    vsq = vr*vr + vt*vt
    al = 2._dp*mu/r - vsq
    alp = Abs(al)
    rtal = Sqrt(alp)
    d = r*vr
    h = r*vt
    p = h*h
    esq1 = p*al
    es = d*rtal
    eses = es*es
    ec = r*vsq - mu
    ecec = ec*ec
    If (al .GT. 0._dp) Then  !one esq formula superior for the ellipse
        esq = ecec + eses
    Else  !different formula superior for the hyperbola
        esq = mu*mu - esq1
    End If
    e = Sqrt(esq)
    q = p / (mu + e)
    If (al .EQ. 0._dp) Then  !parabola
        tau = d*(2._dp*q + r) / (3._dp*mu)
        v = 2._dp*Atan2(vr, vt)
    Else If (e .EQ. 0._dp) Then  !circle
        tau = 0._dp
        v = 0._dp
    Else  !ellipse or hyperbola
        e1 = al*q
        If (al .GT. 0._dp) Then  !ellipse
            eh = Atan2(es, ec)
            If (mu*eh*eh*one_sixth + e1 .GE. mu*0.25_dp) Then  !general case
                em = mu*eh - es
                ecesq = mu*ec - esq
            Else  !for e1 & eh both near zero
                em = mu * emkep(e1/mu, eh)
                ecesq = (esq1*ecec - esq*eses)/(esq + mu*ec)
            End If
        Else  !hyperbola
            eh = Asinh(es/e)
            If (mu*eh*eh*one_sixth - e1 .GE. mu*0.25_dp) Then  !general case
                em = es - mu*eh
                ecesq = esq - mu*ec
            Else  !for e1 & eh both near zero
                em = e * shmkep(-e1/e, es/e)
                ecesq = -(esq1*ecec + esq*eses)/(esq + mu*ec)
            End If
        End If
        en = alp*rtal
        tau = em/en
        v = Atan2(es*h*rtal, ecesq)
    End If
    om = u - v
End Subroutine pv2els

Pure Function emkep(e1,ee) Result(x)
    Use Kinds, Only: dp
    Implicit None

    Real(dp) :: x
    Real(dp), Intent(In) :: e1
    Real(dp), Intent(In) :: ee

    Real(dp) :: ee2, term, x0
    Integer :: d

    x = e1 * Sin(ee)
    ee2 = -ee*ee
    term = ee
    d = 0
    Do
        d = d + 2
        term = term*ee2 / Real(d*(d + 1),dp)
        x0 = x
        x = x - term
        If (x .EQ. x0) Exit
    End Do
End Function emkep

Pure Function shmkep(g1,s)  Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x
    Real(dp), Intent(In) :: g1
    Real(dp), Intent(In) :: s

    Real(dp) :: g,t,tsq,term,x0
    Integer :: twoi1

    g = 1._dp - g1
    t = s / (1._dp + Sqrt(1._dp + s*s))
    tsq = t*t
    x = s*(g1 + g*tsq)
    term = 2._dp*g*t
    twoi1 = 1
    Do
        twoi1 = twoi1 + 2
        term = term*tsq
        x0 = x
        x = x - term/Real(twoi1,dp)
        If (x .EQ. x0) Exit
     End Do
End Function shmkep

Pure Subroutine els3pv(els,r1,v1)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: els(1:6)    !! [al, q, ei, bom, om, tau]
    Real(dp), Intent(Out) :: r1(1:3)
    Real(dp), Intent(Out) :: v1(1:3)
    Real(dp) :: x,y,z
    Real(dp) :: xdot,ydot,zdot
    Real(dp) :: al,q,ei,bom,om,tau
    Real(dp) :: r,u,vr,vt,c,s,x1,x2,y1,y2

    r1 = 0._dp
    v1 = 0._dp
    If (All(els .EQ. 0._dp)) Return
    al  = els(1)
    q   = els(2)
    ei  = els(3)
    bom = els(4)
    om  = els(5)
    tau = els(6)
    Call els2pv (al, q, om, tau, r, u, vr, vt)
    c = Cos(u)
    s = Sin(u)
    x1 = r*c
    y1 = r*s
    x2 = vr*c - vt*s
    y2 = vr*s + vt*c
    c = Cos(ei)
    s = Sin(ei)
    z = y1*s
    y1 = y1*c
    zdot = y2*s
    y2 = y2*c
    c = Cos(bom)
    s = Sin(bom)
    x = x1*c - y1*s
    y = x1*s + y1*c
    xdot = x2*c - y2*s
    ydot = x2*s + y2*c

    r1(1) = x
    r1(2) = y
    r1(3) = z
    v1(1) = xdot
    v1(2) = ydot
    v1(3) = zdot
End Subroutine els3pv

Pure Subroutine els2pv(al, q, om, tau, r, u, vr, vt)
    Use Kinds, Only: dp
    Use Global, Only: mu => grav_param
    Use Global, Only: FourPi
    Implicit None
    Real(dp), Intent(In) :: al    !! alpha [km^2/s^2]
    Real(dp), Intent(In) :: q     !! periapsis distance [km]
    Real(dp), Intent(In) :: om    !! argument of periapsis relative to assumed reference direction [rad]
    Real(dp), Intent(In) :: tau   !! time from periapsis [sec]
    Real(dp), Intent(Out) :: r     !! radial distance [km]
    Real(dp), Intent(Out) :: u     !! angle from reference direction [rad]
    Real(dp), Intent(Out) :: vr    !! radial velocity [km/2]
    Real(dp), Intent(Out) :: vt    !! transverse velocity >=0 [km/s]
    Real(dp) :: d,h,v,e1,e,ep1,alp,rtal,em,ee2,s2,c2,emv,s,c

    If (al .EQ. 0._dp) Then  !parabola
        d = dcbsol(0.5_dp/mu, q, 1.5_dp*mu*tau)
        r = q + 0.5_dp*d*d/mu
        h = Sqrt(2._dp*mu*q)
        v = 2._dp*Atan2(d,h)
    Else  !ellipse or hyperbola
        e1 = al*q
        e = mu - e1
        ep1 = mu + e
        h = Sqrt(q*ep1)
        alp = Abs(al)
        rtal = Sqrt(alp)
        em = tau*alp*rtal
        If (al .GT. 0._dp) Then  !ellipse
            ! make sure e1 argument to ekepl is between [0,1]
            ee2 = 0.5_dp * ekepl(em/mu, Max(0._dp,Min(1._dp,e1/mu)))
            s2 = Sin(ee2)
            c2 = Cos(ee2)
            r = q + 2._dp*e*s2*s2/al
            d = 2._dp*e*s2*c2/rtal
            v = 2._dp*Atan2(ep1*s2, h*rtal*c2)
            emv = em/mu - v
            v = v + FourPi*Sign(Real(Int(Abs(emv/FourPi) + 0.5_dp),dp), emv)
        Else  !hyperbola
            s = shkepl(em/e, -e1/e)
            s2 = s*s
            c = Sqrt(1._dp + s2)
            s2 = s2 / (c + 1._dp)
            r = q - e*s2/al
            d = e*s/rtal
            v = Atan2(s*h*rtal, -mu*s2 - e1)
        End If
    End If
    u = om + v
    vr = d/r
    vt = h/r
End Subroutine els2pv

Pure Function ekepl(em, e1)
    Use Kinds, Only: dp
    Use Global, Only: Pi
    Use Global, Only: TwoPi
    Implicit None
    Real(dp) :: ekepl
    Real(dp), Intent(In) :: em
    Real(dp), Intent(In) :: e1
    Real(dp) :: emr,ee,e,w,fdd,fddd,f,fd,dee
    Integer :: iter
    Real(dp), Parameter :: one_sixth = 1._dp / 6._dp
    Real(dp), Parameter :: one_third = 1._dp / 3._dp

    !range-reduce em to lie in range -pi to pi
    emr = Mod(em,TwoPi)
    If (emr .LT. -Pi) emr = emr + TwoPi
    If (emr .GT. Pi) emr = emr - TwoPi
    ee = emr
    If (ee .NE. 0._dp) Then
        If (ee .LT. 0._dp) ee = -ee
        !(emr is range-reduced em & ee is absolute value of emr)
        !starter by first solving cubic equation
        e = 1._dp - e1
        w = dcbsol(e,2._dp*e1,3._dp*ee)
        !effectively interpolate in emr (absolute value)
        ee = (ee*ee + (Pi - ee)*w) / Pi
        If (emr .LT. 0._dp) ee = -ee
        !do two iterations of halley, each followed by newton
        Do iter = 1,2
            fdd = e*Sin(ee)
            fddd = e*Cos(ee)
            If (ee*ee*one_sixth + e1 .GE. 0.25_dp) Then
                f = (ee - fdd) - emr
                fd = 1._dp - fddd
            Else
                f = emkep(e1,ee) - emr
                fd = 2._dp*e*Sin(0.5_dp*ee)**2 + e1
            End If
            dee = f*fd/(0.5_dp*f*fdd - fd*fd)
            f = f + dee*(fd + 0.5_dp*dee*(fdd + one_third*dee*fddd))
            !to reduce the danger of underflow replace the last line by
            !    w = fd + half*dee*(fdd + athird*dee*fddd)
            fd = fd + dee*(fdd + 0.5_dp*dee*fddd)
            ee = ee + dee - f/fd
            !if replacing as above, then also replace the last line by
            !ee = ee - (f - dee*(fd - w))/fd
        End Do
    End If
    !range-expand
    ekepl = ee + (em - emr)
End Function ekepl

Pure Function dcbsol (a, b, c) result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x
    Real(dp), Intent(In) :: a,b,c
    Real(dp) :: bsq,d

    If (a.EQ.0._dp .AND. b.EQ.0._dp .OR. c.EQ.0._dp) Then
        x = 0._dp
    Else
        bsq = b*b
        d = Sqrt(a) * Abs(c)
        d = dcubrt(d + Sqrt(b*bsq + d*d))**2
        x = 2._dp * c / (d + b + bsq / d)
    End if
End Function dcbsol

Pure Function dcubrt(x) Result(c)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: c
    Real(dp), Intent(In) :: x
    Real(dp) :: y
    Real(dp), Parameter :: one_third = 1._dp / 3._dp

    If (x .EQ. 0._dp) Then
        c = 0._dp
    Else
        y = Abs(x)
        c = y**one_third
        c = c - one_third*(c - y/c**2)
        c = Sign(c,x)
    End If
End Function dcubrt

Pure Function shkepl(el, g1) Result(s)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: s
    Real(dp), Intent(In) :: el
    Real(dp), Intent(In) :: g1
    Real(dp) :: g,cl,al,w,s0,s1,s2,s3,fdd,fddd,f,fd,ds,stemp
    Integer :: iter
    Real(dp), Parameter :: one_sixth = 1._dp / 6._dp
    Real(dp), Parameter :: one_third = 1._dp / 3._dp

    s = el
    If (el .NE. 0._dp) Then  !started based on lagrange's theorem
        g = 1._dp - g1
        cl = Sqrt(1._dp + el**2)
        al = Asinh(el)
        w = g**2 * al / cl**3
        s = 1._dp - g/cl
        s = el + g*al/dcubrt(s**3 + w*el*(1.5_dp - g/0.75_dp))
        !two iterations (at most) of halley-then-newton process
        Do iter = 1,2
            s0 = s*s
            s1 = s0 + 1._dp
            s2 = Sqrt(s1)
            s3 = s1*s2
            fdd = g*s/s3
            fddd = g*(1._dp - 2._dp*s0)/(s1*s3)
            If (one_sixth*s0 + g1 .GE. 0.5_dp) Then
                f = (s - g*Asinh(s)) - el
                fd = 1._dp - g/s2
            Else
                f = shmkep(g1, s) - el
                fd = (s0/(s2 + 1._dp) + g1)/s2
            End If
            ds = f*fd/(0.5_dp*f*fdd - fd*fd)
            stemp = s + ds
            If (stemp .EQ. s) Exit
            f = f + ds*(fd + 0.5_dp*ds*(fdd + one_third*ds*fddd))
            fd = fd + ds*(fdd + 0.5_dp*ds*fddd)
            s = stemp - f/fd
        End Do
    End If
End Function shkepl

End Module Astro_Utilities

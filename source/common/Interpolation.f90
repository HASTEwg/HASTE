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
Module Interpolation
    
    Implicit None
    Private
    Public :: Linear_Interp
    Public :: Cubic_Interp
    Public :: Hermite_Cubic_Interp
    Public :: LogLog_Interp
    Public :: LogLin_Interp
    Public :: LinLog_Interp
    
    Interface Linear_Interp
        Module Procedure Lin_Interp
        Module Procedure BiLin_Interp
    End Interface
    
    Interface Cubic_Interp
        Module Procedure Cub_Interp  !1-D cubic interpolation
        Module Procedure Cub_Sec_Interp  !1-D cubic interpolation using secant approximation
        Module Procedure BiCub_Interp  !2-D cubic interpolation
        Module Procedure BiCub_Sec_Interp  !2-D cubic interpolation using secant approximation
    End Interface
    
    !Hermite routines provided by Kirk Mathews
    Interface Hermite_Cubic_Interp
        Module Procedure Hermite_Cubic_1D
        Module Procedure Hermite_Cubic_2D
    End Interface
    ! This module provides tools for Hermite cubic interpolation in 1 dimension
    !   and for Cartesian-Product Hermite cubic interpolation in 2 dimensions
    ! Note that the cost of general bi-cubic interpolation is 272 arithmetic operations
    !   whereas the cost of Cartesian-product Hermite cubic interpolation is only 60.
    ! Hermite cubic interpolation for a function f(x) 
    !   that has continuous derivatives through order 4 has error term E(x) = f(x) - h(x), where
    ! E(x) = (1-u)^2 * (1+u)^2 * ((xR-xL)^4 / 384) * d4f(x)/dx4 at some point xi in [xL,xR]
    ! Note that, with this definition, E is the correction to the error, because f(x) = h(x) + E(x)
    ! The corresponding error for linear interpolation is
    !   E(x) = -(1-u) * (1+u) * ((xR-xL)^2 / 2) * d2f(x)/dx2
    ! Once the interval is reduced enough that the relative change in the derivatives
    !   is small in the interval, halving the interval reduces 
    !   linear interpolation by a factor of 4, but reduces
    !   Hermite cubic interterpolation by a factor of 16.
    ! For the same amount of stored data, in 2 dimensions, 
    !   linear has half the grid spacing as Hermite cubic, 
    !   but for higher accuracy requirements, the factor of 16 dominates.
    !   Also, the denominator of 384 vs. the denominator of 2 
    !   gives Hermite a factor of 174 better performance.
    
Contains

Elemental Function Cub_Interp(x,x1,x2,y1,y2,dy1,dy2) Result(y)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: y
    Real(dp), Intent(In) :: x
    Real(dp), Intent(In) :: x1,x2
    Real(dp), Intent(In) :: y1,y2
    Real(dp), Intent(In) :: dy1,dy2
    Real(dp) :: t,a,b
    
    t = (x-x1) / (x2-x1)
    a = dy1*(x2-x1) - (y2-y1)
    b = -dy2*(x2-x1) + (y2-y1)
    y = (1._dp - t)*y1 + t*y2 + t*(1._dp - t)*(a*(1._dp - t) + b*t)
End Function Cub_Interp

Function Cub_Sec_Interp(x,xi,yi) Result(y)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: y
    Real(dp), Intent(In) :: x
    Real(dp), Intent(In) :: xi(0:3)
    Real(dp), Intent(In) :: yi(0:3)
    Real(dp) :: dy1,dy2
    
    dy1 = (yi(2)-yi(0)) / (xi(2)-xi(0))
    dy2 = (yi(3)-yi(1)) / (xi(3)-xi(1))
    y = Cub_Interp(x,xi(1),xi(2),yi(1),yi(2),dy1,dy2)
End Function Cub_Sec_Interp

Function BiCub_Sec_Interp(x,xi,y,yi,zi) Result(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: z
    Real(dp), Intent(In) :: x
    Real(dp), Intent(In) :: xi(0:3)
    Real(dp), Intent(In) :: y
    Real(dp), Intent(In) :: yi(0:3)
    Real(dp), Intent(In) :: zi(0:3,0:3)
    Real(dp) :: dz_dx11,dz_dx12,dz_dx21,dz_dx22
    Real(dp) :: dz_dy11,dz_dy12,dz_dy21,dz_dy22
    Real(dp) :: d2z_dx_dy11,d2z_dx_dy12,d2z_dx_dy21,d2z_dx_dy22
    
    dz_dx11 = (zi(2,1)-zi(0,1)) / (xi(2)-xi(0))
    dz_dx12 = (zi(2,2)-zi(0,2)) / (xi(2)-xi(0))
    dz_dx21 = (zi(3,1)-zi(1,1)) / (xi(3)-xi(1))
    dz_dx22 = (zi(3,2)-zi(1,2)) / (xi(3)-xi(1))
    dz_dy11 = (zi(1,2)-zi(1,0)) / (yi(2)-yi(0))
    dz_dy12 = (zi(1,3)-zi(1,1)) / (yi(3)-yi(1))
    dz_dy21 = (zi(2,2)-zi(2,0)) / (yi(2)-yi(0))
    dz_dy22 = (zi(2,3)-zi(2,1)) / (yi(3)-yi(1))
    d2z_dx_dy11 = (zi(2,2)-zi(2,0)-zi(0,2)+zi(0,0)) / ((xi(2)-xi(0))*(yi(2)-yi(0)))
    d2z_dx_dy12 = (zi(2,3)-zi(2,1)-zi(0,3)+zi(0,1)) / ((xi(2)-xi(0))*(yi(3)-yi(1)))
    d2z_dx_dy21 = (zi(3,2)-zi(3,0)-zi(1,2)+zi(1,0)) / ((xi(3)-xi(1))*(yi(2)-yi(0)))
    d2z_dx_dy22 = (zi(3,3)-zi(3,1)-zi(1,3)+zi(1,1)) / ((xi(3)-xi(1))*(yi(3)-yi(1)))
    z = BiCub_Interp( x,xi(1),xi(2),y,yi(1),yi(2), &
                    & zi(1,1),zi(1,2),zi(2,1),zi(2,2), &
                    & dz_dx11,dz_dx12,dz_dx21,dz_dx22, &
                    & dz_dy11,dz_dy12,dz_dy21,dz_dy22, &
                    & d2z_dx_dy11,d2z_dx_dy12,d2z_dx_dy21,d2z_dx_dy22 )
End Function BiCub_Sec_Interp

Elemental Function BiCub_Interp( x,x1,x2,y,y1,y2, & 
                               & z11,z12,z21,z22, &
                               & dz_dx11,dz_dx12,dz_dx21,dz_dx22, &
                               & dz_dy11,dz_dy12,dz_dy21,dz_dy22, &
                               & d2z_dx_dy11,d2z_dx_dy12,d2z_dx_dy21,d2z_dx_dy22 ) Result(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: z
    Real(dp), Intent(In) :: x
    Real(dp), Intent(In) :: x1,x2
    Real(dp), Intent(In) :: y
    Real(dp), Intent(In) :: y1,y2
    Real(dp), Intent(In) :: z11,z12,z21,z22
    Real(dp), Intent(In) :: dz_dx11,dz_dx12,dz_dx21,dz_dx22
    Real(dp), Intent(In) :: dz_dy11,dz_dy12,dz_dy21,dz_dy22
    Real(dp), Intent(In) :: d2z_dx_dy11,d2z_dx_dy12,d2z_dx_dy21,d2z_dx_dy22
    Real(dp) :: z1,z2
    Real(dp) :: dz_dy1,dz_dy2
    
    z1 = Cub_Interp(x,x1,x2,z11,z12,dz_dx11,dz_dx12)
    z2 = Cub_Interp(x,x1,x2,z21,z22,dz_dx21,dz_dx22)
    dz_dy1 = Cub_Interp(x,x1,x2,dz_dy11,dz_dy12,d2z_dx_dy11,d2z_dx_dy12)
    dz_dy2 = Cub_Interp(x,x1,x2,dz_dy21,dz_dy22,d2z_dx_dy21,d2z_dx_dy22)
    z = Cub_Interp(y,y1,y2,z1,z2,dz_dy1,dz_dy2)
End Function BiCub_Interp

Elemental Function Lin_Interp(x,x1,x2,y1,y2) Result(y)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: y
    Real(dp), Intent(In) :: x
    Real(dp), Intent(In) :: x1,x2
    Real(dp), Intent(In) :: y1,y2
    
    y = y1 + (y2-y1) * (x-x1) / (x2-x1)
End Function Lin_Interp

Elemental Function BiLin_Interp(x,x1,x2,y,y1,y2,z11,z12,z21,z22) Result(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: z
    Real(dp), Intent(In) :: x
    Real(dp), Intent(In) :: x1,x2
    Real(dp), Intent(In) :: y
    Real(dp), Intent(In) :: y1,y2
    Real(dp), Intent(In) :: z11,z12,z21,z22
    Real(dp) :: z1,z2
    
    z1 = lin_interp(x,x1,x2,z11,z12)
    z2 = lin_interp(x,x1,x2,z21,z22)
    z = lin_interp(y,y1,y2,z1,z2)
End Function BiLin_Interp

Elemental Function LogLog_Interp(x,x1,x2,y1,y2) Result(y)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: y
    Real(dp), Intent(In) :: x
    Real(dp), Intent(In) :: x1,x2
    Real(dp), Intent(In) :: y1,y2
    
    !Exponentiate a Linearly interpolated in Log(x) and Log(y)
    y = Exp( Linear_Interp(Log(x),Log(x1),Log(x2),Log(y1),Log(y2)) )
End Function LogLog_Interp

Elemental Function LogLin_Interp(x,x1,x2,y1,y2) Result(y)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: y
    Real(dp), Intent(In) :: x
    Real(dp), Intent(In) :: x1,x2
    Real(dp), Intent(In) :: y1,y2
    
    !Exponentiate a Linearly interpolated in Lin(x) and Log(y)
    y = Exp( Linear_Interp(x,x1,x2,Log(y1),Log(y2)) )
End Function LogLin_Interp

Elemental Function LinLog_Interp(x,x1,x2,y1,y2) Result(y)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: y
    Real(dp), Intent(In) :: x
    Real(dp), Intent(In) :: x1,x2
    Real(dp), Intent(In) :: y1,y2
    
    !Linearly interpolated in Log(x) and Lin(y)
    y = Linear_Interp(Log(x),Log(x1),Log(x2),y1,y2)
End Function LinLog_Interp

Function Hermite_Cubic_1D(x, xL, xR, f, dfdx) Result(h)
    ! Hermite cubic interpolation for a function f(x),
    !   given tables of f and df(x)/dx
    ! h(x) = wfL*f(xL) + wfR*f(xR) + wfxL*dfdx(xL) + wfxR*gR
    ! The weights are computed by Function Hermite_Cubic_Weights
    ! The interpolation in 1D is performed using Dot_Product(w, fg), where:
    ! w = (/ wfL, wfR, wgL, wgR /)
    ! fg = (/ fL, fR, gL, gR /)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: h                        ! Interpolated value
    Real(dp), Intent(In):: x            ! location at which to interpolate
    Real(dp), Intent(In):: xL, xR       ! locations of the data
    Real(dp), Intent(In):: f(1:2)       ! function at xL and xR
    Real(dp), Intent(In):: dfdx(1:2)    ! derivative at xL and xR
    Real(dp):: w(1:4)                   ! Vector of weights
    Real(dp):: d(1:4)                   ! (/ f(xL), f(xR), dfdx(xL), dfdx(xR) /)
    
    d(1:2) = f
    d(3:4) = dfdx
    w = Hermite_Cubic_Weights(x, xL, xR)
    h = Dot_Product(w, d)   ! This linear-algebra formalism is used because it extends to 2D
    ! computational cost: 24 arithmetic operations
    !   (multiply & add is one operation in dot products)
End Function Hermite_Cubic_1D    

Function Hermite_Cubic_2D(x, xL, xR, y, yB, yT, f, fx, fy, fxy) Result(h)
    ! Cartesian-product of Hermite cubic in x and in y within a rectangle of data.
    !   L is for Left side of rectangle, R is for Right side, B for Bottom, and T for Top
    ! To pass in f, where the calling routine has an array of data F(0:iMax,0:jMax),
    !   to interpolate at some point (X,Y) with x(i) <= X <= x(i+1) and y(j) <= Y <= y(j+1)
    !   put F(i:i+1,j:j+1) in the location for argument f,
    !   and similarly for the various derivatives.
    ! Note: Module Utilities, Function Find_Data_Pair_Index(x, xList, kStart) 
    !   returns the index i that is needed when passed the list x(0:iMax)
    !   and similarly for j when passed y(0:iMax)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: h                        ! Interpolated value
    Real(dp), Intent(In):: x, y         ! location at which to interpolate
    Real(dp), Intent(In):: xL, xR       ! x coordinates of the data
    Real(dp), Intent(In):: yB, yT       ! y coordinates of the data
    Real(dp), Intent(In):: f(1:2,1:2)   ! f(1,1) is f(xL,yB), f(1,2) is f(xL,yT), and so on
    Real(dp), Intent(In):: fx(1:2,1:2)  ! same for fx = df/dx
    Real(dp), Intent(In):: fy(1:2,1:2)  ! same for fy = df/dy
    Real(dp), Intent(In):: fxy(1:2,1:2) ! same for fxy = d2f/dxdy
    Real(dp):: wx(1:4)                  ! Vector of weights for interpolation in x
    Real(dp):: wy(1:4)                  ! Vector of weights for interpolation in y
    Real(dp):: D(1:4, 1:4)              ! This is a matrix of data that combines f, fx, fy, fxy
    Integer:: j
    
    D(1:2,1:2) = f
    D(1:2,3:4) = fx
    D(3:4,1:2) = fy
    D(3:4,3:4) = fxy
    wx = Hermite_Cubic_Weights(x, xL, xR)   ! cost 20
    wy = Hermite_Cubic_Weights(y, yB, yT)   ! cost 20
        ! h is calculated as the general quadratic form wy.D.wx
        ! where wy is a vector, D is a matrix, and wx is a vector
        ! The dot-product is used for multiplying wy with each column-vector of D.
        !   This choice takes advantage of the column-major storage used by Fortran
    h = 0.0_dp
    Do j = 1, 4
        h = h + Dot_Product(wy,D(:,j)) * wx(j) ! cost 5 each j
    End do
    ! computational cost: 60 arithmetic operations
    !   (counting multiply & add as one operation in accumulations)
End Function Hermite_Cubic_2D

Function Hermite_Cubic_Weights(x, xL, xR) Result(w)
    Use Kinds, Only: dp
    Implicit None
    Real(dp):: w(1:4)                   ! Weight vector to dot with (/ fL, fR, gL, gR /)
    Real(dp), Intent(In):: x            ! location at which to interpolate
    Real(dp), Intent(In):: xL, xR       ! locations of the data
    Real(dp):: u                        ! As x goes from xL to xR, u goes from -1 to +1
    Real(dp):: xBar                     ! center of interval
    Real(dp):: DeltaX                   ! half-width of interval
        ! Units of DeltaX cancel the units of dx in the derivative df/dx
    Real(dp):: m, p                     ! (1-u) & (1+u) for efficiency
    Real(dp):: wfL, wfR, wgL, wgR       ! scalar variables for elements of w
    
    xBar = (xR + xL) / 2.0_dp
    DeltaX = (xR - xL) / 2.0_dp
    u = (x - xBar) / DeltaX
    m = 1.0_dp - u
    p = 1.0_dp + u
    wfL = m**2
    wgL = p * wfL
    wfL = (wfL + wgL) / 4.0_dp 
    wgL = wgL * DeltaX / 4.0_dp
    wfR = p**2
    wgR = -m * wfR
    wfR = (wfR - wgR) / 4.0_dp
    wgR = wgR * DeltaX / 4.0_dp
    w = (/ wfL, wfR, wgL, wgR /)
    ! Cost 20 arithmetic operations
End Function Hermite_Cubic_Weights

End Module Interpolation

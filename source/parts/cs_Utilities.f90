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
Module cs_Utilities

    Implicit None
    Private
    Public :: Tabular_Cosine_pdf

Contains

Function Tabular_Cosine_pdf(mu0cm,n1,ua1,n2,ua2,Econv) Result(p)
    Use Kinds, Only: dp
    Use Utilities, Only: Bisection_Search
    Use Interpolation, Only: Linear_Interp
    Implicit None
    Real(dp) :: p
    Real(dp), Intent(In) :: mu0cm
    Integer, Intent(In) :: n1,n2
    Real(dp), Intent(In) :: ua1(1:n1,1:2),ua2(1:n2,1:2)
    Real(dp), Intent(In) :: Econv
    Integer :: i
    Real(dp) :: p1,p2
    
    i = Bisection_Search(mu0cm,ua1(:,1),n1)
    p1 = Exp(Linear_Interp(mu0cm,ua1(i-1,1),ua1(i,1),Log(ua1(i-1,2)),Log(ua1(i,2))))
    i = Bisection_Search(mu0cm,ua2(:,1),n2)
    p2 = Exp(Linear_Interp(mu0cm,ua2(i-1,1),ua2(i,1),Log(ua2(i-1,2)),Log(ua2(i,2))))
    p = p1 + Econv * (p2 - p1)
End Function Tabular_Cosine_pdf

End Module cs_Utilities
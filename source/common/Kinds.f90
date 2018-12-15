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
Module Kinds
    
    Implicit None
    Public

    Integer, Parameter :: sp = Selected_Real_Kind(p=6)   !single precision
    Integer, Parameter :: dp = Selected_Real_Kind(p=15)  !double precision
    Integer, Parameter :: qp = Selected_Real_Kind(p=33)  !quadruple precision

    Integer, Parameter :: is = Selected_Int_Kind(3)  !short word, +/- 10**4
    Integer, Parameter :: il = Selected_Int_Kind(8)  !long word, +/- 10**8
    Integer, Parameter :: id = Selected_Int_Kind(14)  !double word, +/- 10**14
    
End Module Kinds

Module PRNGs
!-------------------------------------------------------------------------------
!   A C-program for MT19937: Integer and real number versions
!      Coded by Takuji Nishimura, considering the suggestions by
!       Topher Cooper and Marc Rieffel in July-Aug. 1997.
!       Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.

!   A C-program for MT19937-64: 64-bit version of Mersenne Twister PRNG
!       Coded by Takuji Nishimura and Makoto Matsumoto.
!       Copyright (C) 2004, Makoto Matsumoto and Takuji Nishimura.
!
!   Redistribution and use in source and binary forms, with or without
!   modification, are permitted provided that the following conditions
!   are met:
!
!     1. Redistributions of source code must retain the above copyright
!        notice, this list of conditions and the following disclaimer.
!
!     2. Redistributions in binary form must reproduce the above copyright
!        notice, this list of conditions and the following disclaimer in the
!        documentation and/or other materials provided with the distribution.
!
!     3. The names of its contributors may not be used to endorse or promote 
!        products derived from this software without specific prior written 
!        permission.
!
!   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
!   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
!   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
!   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
!   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
!   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
!   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
!   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!-------------------------------------------------------------------------------
!   Fortran Translation of MT19937
!
!   Original Fortran translation by Hiroshi Takano.  Jan. 13, 1999.
!
!   Fortran translation rewritten as an f90 module by Richard Woloshyn,
!   (rwww@triumf.ca). June 30, 1999
!
!   Revised Fortran translation by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
!   Fortran Translation of MT19937-64
!
!   Original Fortran translation by Rﾃｩmi Piatek.  Nov. 04, 2016.
!   The University of Copenhagen, Department of Economics
!   email: {first}.{last}@econ.ku.dk
!
!   Revised Fortran translation by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
    Use Kinds, Only: i4
    Use Kinds, Only: i8
    Implicit None
    Private
    Public :: MT19937_Type
    Public :: MT19937x64_Type

    !Parameters and type for MT19937 PRNG
    Integer(i4), Parameter :: default_seed = 4357_i4
    Integer, Parameter :: n = 624
    Integer, Parameter :: m = 397
    !the MT19937 state
    Type :: MT19937_Type
        Integer(i4) :: mt(1:n)
        Integer :: mti = HUGE(n) !mti>n means mt[n] is not initialized
        Logical :: seeded = .FALSE.
    Contains
        Procedure, Pass :: seed => seed_rng_mt19937
        Procedure, Pass :: i => rng_mt19937_i
        Procedure, Pass :: r => rng_mt19937_r
        Procedure, Pass :: save => save_rng_mt19937
        Procedure, Pass :: load => load_rng_mt19937
    End Type MT19937_type
    
    !Parameters and type for MT19937x64 PRNG
    Integer(i8), Parameter :: default_seed64 = 4357_i8
    Integer, Parameter :: n64 = 312
    Integer, Parameter :: m64 = 156
    !the MT19937x64 state
    Type :: MT19937x64_Type
        Integer(i8) :: mt(1:n64)
        Integer :: mti = HUGE(n64) !mti>n means mt[n] is not initialized
        Logical :: seeded = .FALSE.
    Contains
        Procedure, Pass :: seed => seed_rng_mt19937x64
        Procedure, Pass :: i => rng_mt19937x64_i
        Procedure, Pass :: r => rng_mt19937x64_r
    End Type MT19937x64_type

Contains

Subroutine seed_rng_mt19937(RNG,seed)
    Use Kinds, Only: dp
    Use Kinds, Only: i4
    Implicit None
    Class(MT19937_Type), Intent(InOut) :: RNG
    Integer(i4), Intent(In) :: seed
    Integer :: i
    !Real(dp) :: temp
    !Integer(i4) :: itemp, itemp2
    !Real(dp), Parameter :: two31 = 2._dp**31
    
    RNG%mt(1) = IAND(seed,-1_i4)
    Do i = 2,n
        RNG%mt(i) = IAND(69069_i4 * RNG%mt(i-1),-1_i4)
        ! temp = 69069._dp * Real(RNG%mt(i-1),dp)
        ! itemp = Int(Mod(temp,two31))
        ! itemp2 = Int(temp / two31)
        ! If (Mod(itemp2,2) .NE. 0) itemp = Int( Real(itemp,dp) + Sign(two31,Real(-itemp,dp)) )
        ! RNG%mt(i) = itemp
    End Do
    RNG%mti = n
    RNG%seeded = .TRUE.
End Subroutine seed_rng_mt19937

Function rng_mt19937_i(RNG) Result(y)
    Use Kinds, Only: i4
    Implicit None
    Integer(i4) :: y
    Class(MT19937_Type), Intent(InOut) :: RNG
    Integer :: i
    Integer(i4) :: umasku
    Integer(i4), Parameter :: mag01(0:1) = (/ 0_i4 , -1727483681_i4 /)!constant vector a
    Integer(i4), Parameter :: lmask = 2147483647_i4 !least significant r bits
    Integer(i4), Parameter :: umask = -2147483647_i4!-1_i4 !most significant w-r bits
    Integer(i4), Parameter :: tmaskb = -1658038656_i4 !tempering parameters
    Integer(i4), Parameter :: tmaskc = -272236544_i4 !tempering parameters
    Real(dp), Parameter :: two32 = 2._dp**32
    Real(dp), Parameter :: invtwo32m1 = 1._dp / (two32 - 1._dp)

    If (RNG%mti .GE. n) Then  !generate N words at one time
        If (RNG%seeded) Then  !needs new words
            umasku = umask - 1_i4  !integer overflow
            Do  i = 1,n-m
                y = IOR(IAND(RNG%mt(i),umasku), IAND(RNG%mt(i+1),lmask))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+m), ISHFT(y,-1_i4)),mag01(IAND(y,1_i4)))
            End Do
            Do  i = n-m+1,n-1
                y = IOR(IAND(RNG%mt(i),umasku), IAND(RNG%mt(i+1),lmask))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+(m-n)), ISHFT(y,-1_i4)),mag01(IAND(y,1_i4)))
            End Do
            y = IOR(IAND(RNG%mt(n),umasku), IAND(RNG%mt(1),lmask))
            RNG%mt(n) = IEOR(IEOR(RNG%mt(m), ISHFT(y,-1_i4)),mag01(IAND(y,1_i4)))
            RNG%mti = 0
        Else  !needs seeding
            Call RNG%seed(default_seed)
        End If
    End If
    y = RNG%mt(RNG%mti)
    RNG%mti = RNG%mti + 1
    y = IEOR(y, ISHFT(y,-11_i4))
    y = IEOR(y, IAND(ISHFT(y,7_i4),tmaskb))
    y = IEOR(y, IAND(ISHFT(y,15_i4),tmaskc))
    y = IEOR(y, ISHFT(y,-18_i4))
End Function rng_mt19937_i

Function rng_mt19937_r(RNG) Result(x)
    Use Kinds, Only: dp
    Use Kinds, Only: i4
    Implicit None
    Real(dp) :: x
    Class(MT19937_Type), Intent(InOut) :: RNG
    Integer(i4) :: y
    Real(dp), Parameter :: two32 = 2._dp**32
    Real(dp), Parameter :: invtwo32m1 = 1._dp / (two32 - 1._dp)
    
    y = RNG%i()
    If (y .LT. 0_i4) Then
        x = (Real(y,dp) + two32) * invtwo32m1
    Else
        x = Real(y,dp) * invtwo32m1
    End If
Function rng_mt19937_r

Subroutine save_RNG_mt19937(RNG,fname)
    Use FileIO_Utilities, Only: Var_to_file
    Implicit None
    Class(MT19937_Type), Intent(In) :: RNG
    Character(*), Intent(In) :: fname
    Integer :: state(1:n+1)
    
    state(1:n) = RNG%stream%mt
    state(n+1) = RNG%stream%mti
    Call Var_to_File(state,fname)
End Subroutine save_RNG_mt19937

Subroutine load_RNG_mt19937(RNG,fname)
    Use FileIO_Utilities, Only: Var_from_file
    Implicit None
    Class(MT19937_Type), Intent(InOut) :: RNG
    Character(*), Intent(In) :: fname
    Integer :: state(1:n+1)
    
    Call Var_from_File(state,fname)
    RNG%stream%mt = state(1:n)
    RNG%stream%mti = state(n+1)
    RNG%stream%seeded = .TRUE.
End Subroutine load_RNG_mt19937

Subroutine seed_rng_mt19937x64(RNG,seed)
    Use Kinds, Only: i8
    Implicit None
    Class(MT19937x64_Type), Intent(InOut) :: RNG
    Integer(i8), Intent(In) :: seed
    Integer :: i

    RNG%mt(1) = seed
    Do i = 1,n64-1
        RNG%mt(i+1) = 6364136223846793005_i8 * IEOR(RNG%mt(i), ISHFT(RNG%mt(i), -62_i8)) + i
    End Do
    RNG%mti = n64
    RNG%seeded = .TRUE.
End Subroutine seed_rng_mt19937x64

Function rng_mt19937x64_i(RNG) Result(y)
    Use Kinds, Only: i8
    Implicit None
    Integer(i8) :: y
    Class(MT19937x64_Type), Intent(InOut) :: RNG
    Integer :: i
    Integer(i8), Parameter :: mag01(0:1) = (/ 0_i8, -5403634167711393303_i8 /)
    Integer(i8), Parameter :: um = -2147483648_i8 ! most significant 33 bits
    Integer(i8), Parameter :: lm =  2147483647_i8 ! least significant 31 bits

    If (RNG%mti .GE. n64) Then ! generate n words at one time
        If (RNG%seeded) Then  !needs new words
            Do i = 1,n64-m64
                y = IOR(IAND(RNG%mt(i),um), IAND(RNG%mt(i+1), lm))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+m64), ISHFT(y, -1_i8)), mag01(IAND(y, 1_i8)))
            End Do
            Do i = n64-m64+1,n64-1
                y = IOR(IAND(RNG%mt(i), um), IAND(RNG%mt(i+1), lm))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+m64-n64), ISHFT(y, -1_i8)), mag01(IAND(y, 1_i8)))
            End Do
            y = IOR(IAND(RNG%mt(n64), um), IAND(RNG%mt(1), lm))
            RNG%mt(n64) = IEOR(IEOR(RNG%mt(m64), ISHFT(y, -1_i8)), mag01(IAND(y, 1_i8)))
            RNG%mti = 0
        Else  !needs seeding
            Call RNG%seed(default_seed64)
        End If
    End If
    RNG%mti = RNG%mti + 1
    y = RNG%mt(RNG%mti)
    y = IEOR(y, IAND(ISHFT(y,-29_i8), 6148914691236517205_i8))
    y = IEOR(y, IAND(ISHFT(y, 17_i8), 8202884508482404352_i8))
    y = IEOR(y, IAND(ISHFT(y, 37_i8),   -2270628950310912_i8))
    y = IEOR(y, ISHFT(y, -43_i8))
End Function rng_mt19937x64_i

Function rng_mt19937x64_r(RNG) Result(x)
    Use Kinds, Only: dp
    Use Kinds, Only: i8
    Implicit None
    Real(dp) :: x
    Class(MT19937x64_Type), Intent(InOut) :: RNG
    Real(dp), Parameter :: invtwo53 = 1._dp / (2._dp**53)
    
    x = Real(ISHFT(RNG%i(), -11_i8),dp) * invtwo53
End Function rng_mt19937x64_r

Subroutine save_RNG_mt19937x64(RNG,fname)
    Use FileIO_Utilities, Only: Var_to_file
    Implicit None
    Class(MT19937x64_Type), Intent(In) :: RNG
    Character(*), Intent(In) :: fname
    Integer :: state(1:n64+1)
    
    state(1:n64) = RNG%stream%mt
    state(n64+1) = RNG%stream%mti
    Call Var_to_File(state,fname)
End Subroutine save_RNG_mt19937x64

Subroutine load_RNG_mt19937x64(RNG,fname)
    Use FileIO_Utilities, Only: Var_from_file
    Implicit None
    Class(MT19937x64_Type), Intent(InOut) :: RNG
    Character(*), Intent(In) :: fname
    Integer :: state(1:n64+1)
    
    Call Var_from_File(state,fname)
    RNG%stream%mt = state(1:n64)
    RNG%stream%mti = state(n64+1)
    RNG%stream%seeded = .TRUE.
End Subroutine load_RNG_mt19937x64

End Module PRNGs

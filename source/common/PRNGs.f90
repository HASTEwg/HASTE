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
!   FORTRAN 77 translation with updates to MT seeding by Tsuyoshi TADA.
!   December 19, 2005
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
!   Fortran Translation of MT2203
!
!   Fortran translation by Whitman Dailey.  Jun 02, 2018.
!   Air Force Institute of Technology, Department of Engineering Physics
!-------------------------------------------------------------------------------
    Use Kinds, Only: il !Long integer (4-bytes)
    Use Kinds, Only: id !Double integer
    Implicit None
    Private
    Public :: MT19937_Type
    Public :: MT19937x64_Type
    Public :: MT2203_Type

    Integer(il), Parameter :: TOPBIT = ISHFT(1073741824_il,1_il)
    Integer(il), Parameter :: ALLBIT = IOR(2147483647_il,TOPBIT)
    !Parameters and type for MT19937 PRNG
    Integer(il), Parameter :: default_seed = 4357_il
    Integer, Parameter :: n = 624
    Integer, Parameter :: m = 397
    !the MT19937 state
    Type :: MT19937_Type
        Integer(il) :: mt(1:n)
        Integer :: mti = HUGE(n) !mti>n+1 means mt[n] is not initialized
        Logical :: seeded = .FALSE.
    Contains
        Procedure, Pass :: seed => seed_rng_mt19937
        Procedure, Pass :: seeds => seed_ar_rng_mt19937
        Procedure, Pass :: i => rng_mt19937_i
        Procedure, Pass :: r => rng_mt19937_r
        Procedure, Pass :: save => save_rng_mt19937
        Procedure, Pass :: load => load_rng_mt19937
    End Type MT19937_type
    
    !Parameters and type for MT19937x64 PRNG
    Integer(id), Parameter :: default_seed64 = 4357_id
    Integer, Parameter :: n64 = 312
    Integer, Parameter :: m64 = 156
    !the MT19937x64 state
    Type :: MT19937x64_Type
        Integer(id) :: mt(1:n64)
        Integer :: mti = HUGE(n64) !mti>n+1 means mt[n] is not initialized
        Logical :: seeded = .FALSE.
    Contains
        Procedure, Pass :: seed => seed_rng_mt19937x64
        Procedure, Pass :: i => rng_mt19937x64_i
        Procedure, Pass :: r => rng_mt19937x64_r
        Procedure, Pass :: save => save_rng_mt19937x64
        Procedure, Pass :: load => load_rng_mt19937x64
    End Type MT19937x64_type

    !Parameters and type for MT2203 PRNGs
    Integer, Parameter :: n2203 = 69
    Integer, Parameter :: m2203 = 34
    !the MT2203 state
    Type :: MT2203_Type
        Integer(il) :: aj,bj,cj !index specific tempering parameters
        Integer(il) :: mt(1:n2203)
        Integer :: mti = HUGE(n2203) !mti>n+1 means mt[n] is not initialized
        Logical :: seeded = .FALSE.
    Contains
        Procedure, Pass :: seed => seed_rng_mt2203
        Procedure, Pass :: seeds => seed_ar_rng_mt2203
        Procedure, Pass :: i => rng_mt2203_i
        Procedure, Pass :: r => rng_mt2203_r
        Procedure, Pass :: save => save_rng_mt2203
        Procedure, Pass :: load => load_rng_mt2203
    End Type MT2203_type

Contains

Subroutine seed_rng_mt19937(RNG,seed,burn)
    Use Kinds, Only: il
    Implicit None
    Class(MT19937_Type), Intent(InOut) :: RNG
    Integer(il), Intent(In) :: seed
    Integer(il), Intent(In), Optional :: burn
    Integer :: i
    Integer(il) :: k,burns
    
    RNG%mt(1) = IAND(seed,-1_il)
    Do i = 2,n
        RNG%mt(i) = IAND(1812433253 * IEOR(RNG%mt(i-1),ISHFT(RNG%mt(i-1),-30_il)) + i,ALLBIT)
    End Do
    RNG%mti = n + 1
    RNG%seeded = .TRUE.
    If (Present(burn)) Then
        Do k = 1,burn
            burns = RNG%i()
        End Do
    End If
End Subroutine seed_rng_mt19937

Subroutine seed_ar_rng_mt19937(RNG,seeds)
    Use Kinds, Only: il
    Implicit None
    Class(MT19937_Type), Intent(InOut) :: RNG
    Integer(il), Intent(In) :: seeds(:)
    Integer :: i,j,k
      
    Call RNG%seed(19650218_il)
    i = 1
    j = 0
    Do k = 1,max(n,Size(seeds))
        RNG%mt(i+1)=IAND(IEOR(RNG%mt(i+1),IEOR(RNG%mt(i),ISHFT(RNG%mt(i),-30_il))*1664525_il)+seeds(j+1)+Int(j,il),ALLBIT)
        i = i + 1
        j = j + 1
        If (i .GT. n) Then
            RNG%mt(1) = RNG%mt(n)
            i = 1
        End If
        If (j .GE. Size(seeds)) j = 0
    End Do
    Do k = 1,n
        RNG%mt(i+1)=IAND(IEOR(RNG%mt(i+1),IEOR(RNG%mt(i),ISHFT(RNG%mt(i),-30_il))*1566083941_il)-Int(i,il),ALLBIT)
        i = i + 1
        If (i .GT. n) Then
            RNG%mt(1) = RNG%mt(n)
            i = 1
        End If
    End Do
    RNG%mt(1) = TOPBIT
End Subroutine seed_ar_rng_mt19937

Function rng_mt19937_i(RNG) Result(y)
    Use Kinds, Only: il
    Implicit None
    Integer(il) :: y
    Class(MT19937_Type), Intent(InOut) :: RNG
    Integer :: i
    Integer(il), Parameter :: ta = -1727483681_il !constant vector a
    Integer(il), Parameter :: mag01(0:1) = (/ 0_il , ta /) !constant vector a
    Integer(il), Parameter :: lm = 2147483647_il !least significant r bits
    Integer(il), Parameter :: um = ISHFT(1073741824,1) !-2147483647_il-1_il !most significant w-r bits
    Integer(il), Parameter :: tb = -1658038656_il !tempering parameters
    Integer(il), Parameter :: tc = -272236544_il !tempering parameters

    If (RNG%mti .GT. n) Then  !generate N words at one time
        If (RNG%seeded) Then  !needs new words
            Do  i = 1,n-m
                y = IOR(IAND(RNG%mt(i),um), IAND(RNG%mt(i+1),lm))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+m), ISHFT(y,-1_il)),mag01(IAND(y,1_il)))
            End Do
            Do  i = n-m+1,n-1
                y = IOR(IAND(RNG%mt(i),um), IAND(RNG%mt(i+1),lm))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+(m-n)), ISHFT(y,-1_il)),mag01(IAND(y,1_il)))
            End Do
            y = IOR(IAND(RNG%mt(n),um), IAND(RNG%mt(1),lm))
            RNG%mt(n) = IEOR(IEOR(RNG%mt(m), ISHFT(y,-1_il)),mag01(IAND(y,1_il)))
            RNG%mti = 1
        Else  !needs seeding
            Call RNG%seed(default_seed)
        End If
    End If
    y = RNG%mt(RNG%mti)
    RNG%mti = RNG%mti + 1
    y = IEOR(y, ISHFT(y,-11_il))
    y = IEOR(y, IAND(ISHFT(y,7_il),tb))
    y = IEOR(y, IAND(ISHFT(y,15_il),tc))
    y = IEOR(y, ISHFT(y,-18_il))
End Function rng_mt19937_i

Function rng_mt19937_r(RNG) Result(x)
    Use Kinds, Only: dp
    Use Kinds, Only: il
    Implicit None
    Real(dp) :: x
    Class(MT19937_Type), Intent(InOut) :: RNG
    Integer(il) :: y
    Real(dp), Parameter :: two32 = 2._dp**32
    Real(dp), Parameter :: invtwo32 = 1._dp / two32
    
    y = RNG%i()
    If (y .LT. 0_il) Then
        x = (Real(y,dp) + two32) * invtwo32
    Else
        x = Real(y,dp) * invtwo32
    End If
End Function rng_mt19937_r

Function rng_mt19937_r53(RNG) Result(x)
    Use Kinds, Only: dp
    Use Kinds, Only: il
    Implicit None
    Real(dp) :: x
    Class(MT19937_Type), Intent(InOut) :: RNG
    Real(dp) :: a,b
    Real(dp), Parameter :: two32 = 2._dp**32
    Real(dp), Parameter :: two26 = 2._dp**26
    Real(dp), Parameter :: invtwo53 = 1._dp / (2._dp**53)
    
    a = ISHFT(RNG%i(),-5_il)
    b = ISHFT(RNG%i(),-6_il)
    If (a .LT. 0._dp) a = a + two32
    If (b .LT. 0._dp) b = b + two32
    x = (a * two26 + b) * invtwo53
End Function rng_mt19937_r53

Subroutine save_RNG_mt19937(RNG,fname)
    Use Kinds, Only: il
    Use FileIO_Utilities, Only: Var_to_file
    Implicit None
    Class(MT19937_Type), Intent(In) :: RNG
    Character(*), Intent(In) :: fname
    Integer(il) :: state(1:n+1)
    
    state(1:n) = RNG%mt
    state(n+1) = RNG%mti
    Call Var_to_File(state,fname)
End Subroutine save_RNG_mt19937

Subroutine load_RNG_mt19937(RNG,fname)
    Use Kinds, Only: il
    Use FileIO_Utilities, Only: Var_from_file
    Implicit None
    Class(MT19937_Type), Intent(InOut) :: RNG
    Character(*), Intent(In) :: fname
    Integer(il) :: state(1:n+1)
    
    Call Var_from_File(state,fname)
    RNG%mt = state(1:n)
    RNG%mti = state(n+1)
    RNG%seeded = .TRUE.
End Subroutine load_RNG_mt19937

Subroutine seed_rng_mt19937x64(RNG,seed,burn)
    Use Kinds, Only: id
    Implicit None
    Class(MT19937x64_Type), Intent(InOut) :: RNG
    Integer(id), Intent(In) :: seed
    Integer(id), Intent(In), Optional :: burn
    Integer :: i
    Integer(id) :: k,burns

    RNG%mt(1) = seed
    Do i = 2,n64
        RNG%mt(i) = 6364136223846793005_id * IEOR(RNG%mt(i-1), ISHFT(RNG%mt(i-1), -62_id)) + Int(i,id)
    End Do
    RNG%mti = n64 + 1
    RNG%seeded = .TRUE.
    If (Present(burn)) Then
        Do k = 1,burn
            burns = RNG%i()
        End Do
    End If
End Subroutine seed_rng_mt19937x64

Subroutine seed_ar_rng_mt19937x64(RNG,seeds)
    Use Kinds, Only: id
    Implicit None
    Class(MT19937x64_Type), Intent(InOut) :: RNG
    Integer(id), Intent(In) :: seeds(:)
    Integer :: i,j,k

    Call RNG%seed(19650218_id)
    i = 1
    j = 0
    Do k = 1,Max(n64,Size(seeds))
        RNG%mt(i+1) = IEOR(RNG%mt(i+1),3935559000370003845_id*IEOR(RNG%mt(i),ISHFT(RNG%mt(i),-62_id))) + seeds(j+1) + Int(j,id)
        i = i + 1
        j = j + 1
        If (i .GT. n64) Then
            RNG%mt(1) = RNG%mt(n64)
            i = 1
        End If
        If (j .GE. Size(seeds)) j = 0
    End Do
    Do k = 1,n64
      RNG%mt(i+1) = IEOR(RNG%mt(i+1),2862933555777941757_id*IEOR(RNG%mt(i),ISHFT(RNG%mt(i),-62_id))) - Int(i,id)
      i = i + 1
      If (i .GT. n64) Then
          RNG%mt(1) = RNG%mt(n64)
          i = 1
      End If
    End Do
    RNG%mt(1) = ISHFT(1_id, 63_id)  ! MSB is 1; assuring non-zero initial array
End Subroutine seed_ar_rng_mt19937x64

Function rng_mt19937x64_i(RNG) Result(y)
    Use Kinds, Only: id
    Implicit None
    Integer(id) :: y
    Class(MT19937x64_Type), Intent(InOut) :: RNG
    Integer :: i
    Integer(id), Parameter :: mag01(0:1) = (/ 0_id, -5403634167711393303_id /)
    Integer(id), Parameter :: um = -2147483648_id ! most significant 33 bits
    Integer(id), Parameter :: lm =  2147483647_id ! least significant 31 bits

    If (RNG%mti .GT. n64) Then ! generate n words at one time
        If (RNG%seeded) Then  !needs new words
            Do i = 1,n64-m64
                y = IOR(IAND(RNG%mt(i),um), IAND(RNG%mt(i+1), lm))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+m64), ISHFT(y, -1_id)), mag01(IAND(y, 1_id)))
            End Do
            Do i = n64-m64+1,n64-1
                y = IOR(IAND(RNG%mt(i), um), IAND(RNG%mt(i+1), lm))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+m64-n64), ISHFT(y, -1_id)), mag01(IAND(y, 1_id)))
            End Do
            y = IOR(IAND(RNG%mt(n64), um), IAND(RNG%mt(1), lm))
            RNG%mt(n64) = IEOR(IEOR(RNG%mt(m64), ISHFT(y, -1_id)), mag01(IAND(y, 1_id)))
            RNG%mti = 1
        Else  !needs seeding
            Call RNG%seed(default_seed64)
        End If
    End If
    y = RNG%mt(RNG%mti)
    RNG%mti = RNG%mti + 1
    y = IEOR(y, IAND(ISHFT(y,-29_id), 6148914691236517205_id))
    y = IEOR(y, IAND(ISHFT(y, 17_id), 8202884508482404352_id))
    y = IEOR(y, IAND(ISHFT(y, 37_id),   -2270628950310912_id))
    y = IEOR(y, ISHFT(y, -43_id))
End Function rng_mt19937x64_i

Function rng_mt19937x64_r(RNG) Result(x)
    Use Kinds, Only: dp
    Use Kinds, Only: id
    Implicit None
    Real(dp) :: x
    Class(MT19937x64_Type), Intent(InOut) :: RNG
    Real(dp), Parameter :: invtwo53 = 1._dp / 2._dp**53
    
    x = Real(ISHFT(RNG%i(), -11_id),dp) * invtwo53
End Function rng_mt19937x64_r

Subroutine save_RNG_mt19937x64(RNG,fname)
    Use Kinds, Only: id
    Use FileIO_Utilities, Only: Var_to_file
    Implicit None
    Class(MT19937x64_Type), Intent(In) :: RNG
    Character(*), Intent(In) :: fname
    Integer(id) :: state(1:n64+1)
    
    state(1:n64) = RNG%mt
    state(n64+1) = Int(RNG%mti,id)
    Call Var_to_File(state,fname)
End Subroutine save_RNG_mt19937x64

Subroutine load_RNG_mt19937x64(RNG,fname)
    Use Kinds, Only: id
    Use Kinds, Only: il
    Use FileIO_Utilities, Only: Var_from_file
    Implicit None
    Class(MT19937x64_Type), Intent(InOut) :: RNG
    Character(*), Intent(In) :: fname
    Integer(id) :: state(1:n64+1)
    
    Call Var_from_File(state,fname)
    RNG%mt = state(1:n64)
    RNG%mti = Int(state(n64+1),il)
    RNG%seeded = .TRUE.
End Subroutine load_RNG_mt19937x64

Subroutine seed_rng_mt2203(RNG,jj,seed,burn,resourcedir)
    Use Kinds, Only: il
    Use FileIO_Utilities, Only: max_path_len
    Use FileIO_Utilities, Only: slash
    Use FileIO_Utilities, Only: Output_Message
    Implicit None
    Class(MT2203_Type), Intent(InOut) :: RNG
    Integer(il), Intent(In) :: jj  !Index of the MT2203 stream for this PRNG
    Integer(il), Intent(In) :: seed  !Seed for PRNG
    Integer(il), Intent(In), Optional :: burn  !OPTIONAL: Number of random numbers to 'burn' after seeding
    Character(*), Intent(In), Optional :: resourcedir  !OPTIONAL: A directory, other than the default resources directory, in which to find the MT2203 parameters file
    Integer :: i
    Integer(il) :: k,burns
    Integer(il) :: unit,stat
    Integer(il) :: nj2203  !number of parameter sets for MT2203 in the parameter file
    Character(:), Allocatable :: fname

    Allocate(Character(max_path_len) :: fname)
    If (Present(resourcedir)) Then
        fname = Trim(resourcedir)//'mt'//slash//'mt2203params.txt'
    Else
        fname = 'Resources'//slash//'mt'//slash//'mt2203params.txt'
    End If
    Open(NEWUNIT = unit , FILE = fname , STATUS = 'OLD' , ACTION = 'READ' , FORM = 'FORMATTED' , IOSTAT = stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  PRNGs: seed_rng_mt2203:  Open MT prameter file failed',kill=.TRUE.)
    !Check available number of MT2203 parameter sets
    Read(unit,'(I11)') nj2203
    If (jj.LT.1 .OR. jj.GT.nj2203) Then
        Write(*,'(A)')         'ERROR:  PRNGs: seed_rng_mt2203:  MT2203 parameter index out of range.'
        Write(*,'(A,I0,A,I0)') '        Specified index:  ',jj,'  Available range:  1-',nj2203
        ERROR STOP
    End If
    !Load MT2203 parameters from resource file
    Do i = 1,jj
        Read(unit,'(I11,2I12)') RNG%aj , RNG%bj , RNG%cj
    End Do
    Close(unit)
    !Seed the generator
    RNG%mt(1) = IAND(seed,-1_il)
    Do i = 2,n2203
        RNG%mt(i) = IAND(1812433253 * IEOR(RNG%mt(i-1),ISHFT(RNG%mt(i-1),-30_il)) + Int(i,il),-1_il)
    End Do
    RNG%mti = n2203 + 1
    RNG%seeded = .TRUE.
    If (Present(burn)) Then
        Do k = 1,burn
            burns = RNG%i()
        End Do
    End If
End Subroutine seed_rng_mt2203

Subroutine seed_ar_rng_mt2203(RNG,jj,seeds,resourcedir)
    Use Kinds, Only: il
    Implicit None
    Class(MT2203_Type), Intent(InOut) :: RNG
    Integer, Intent(In) :: jj
    Integer(il), Intent(In) :: seeds(:)
    Character(*), Intent(In), Optional :: resourcedir  !OPTIONAL: A directory, other than the default resources directory, in which to find the MT2203 parameters file
    Integer :: i,j,k
      
    If (Present(resourcedir)) Then
        Call RNG%seed(jj,19650218_il,RESOURCEDIR=resourcedir)
    Else
        Call RNG%seed(jj,19650218_il)
    End If
    i = 1
    j = 0
    Do k = 1,max(n2203,Size(seeds))
        RNG%mt(i+1)=IAND(IEOR(RNG%mt(i+1),IEOR(RNG%mt(i),ISHFT(RNG%mt(i),-30_il))*1664525_il)+seeds(j+1)+j,ALLBIT)
        i = i + 1
        j = j + 1
        If (i .GT. n2203) Then
            RNG%mt(1) = RNG%mt(n2203)
            i = 1
        End If
        If (j .GE. Size(seeds)) j = 0
    End Do
    Do k = 1,n2203
        RNG%mt(i+1)=IAND(IEOR(RNG%mt(i+1),IEOR(RNG%mt(i),ISHFT(RNG%mt(i),-30_il))*1566083941_il)-Int(i,il),ALLBIT)
        i = i + 1
        If (i .GT. n2203) Then
            RNG%mt(1) = RNG%mt(n2203)
            i = 1
        End If
    End Do
    RNG%mt(1) = TOPBIT
End Subroutine seed_ar_rng_mt2203

Function rng_mt2203_i(RNG) Result(y)
    Use Kinds, Only: il
    Implicit None
    Integer(il) :: y
    Class(MT2203_Type), Intent(InOut) :: RNG
    Integer :: i
    Integer(il) :: mag01(0:1)
    Integer(il), Parameter :: lmask = 31_il !least significant r bits
    Integer(il), Parameter :: umask = -32_il !most significant w-r bits

    mag01 = (/ 0_il , RNG%aj /)
    If (RNG%mti .GT. n2203) Then  !generate N words at one time
        If (RNG%seeded) Then  !needs new words
            Do  i = 1,n2203-m2203
                y = IOR(IAND(RNG%mt(i),umask), IAND(RNG%mt(i+1),lmask))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+m2203), ISHFT(y,-1_il)),mag01(IAND(y,1_il)))
            End Do
            Do  i = n2203-m2203+1,n2203-1
                y = IOR(IAND(RNG%mt(i),umask), IAND(RNG%mt(i+1),lmask))
                RNG%mt(i) = IEOR(IEOR(RNG%mt(i+(m2203-n2203)), ISHFT(y,-1_il)),mag01(IAND(y,1_il)))
            End Do
            y = IOR(IAND(RNG%mt(n2203),umask), IAND(RNG%mt(1),lmask))
            RNG%mt(n2203) = IEOR(IEOR(RNG%mt(m2203), ISHFT(y,-1_il)),mag01(IAND(y,1_il)))
            RNG%mti = 1
        Else  !needs seeding
            !HACK In the unseeded case, default behavior uses MT2203 index #1
            Call RNG%seed(1,default_seed)
        End If
    End If
    y = RNG%mt(RNG%mti)
    RNG%mti = RNG%mti + 1
    y = IEOR(y, ISHFT(y,-12_il))
    y = IEOR(y, IAND(ISHFT(y,7_il),RNG%bj))
    y = IEOR(y, IAND(ISHFT(y,15_il),RNG%cj))
    y = IEOR(y, ISHFT(y,-18_il))
End Function rng_mt2203_i

Function rng_mt2203_r(RNG) Result(x)
    Use Kinds, Only: dp
    Use Kinds, Only: il
    Implicit None
    Real(dp) :: x
    Class(MT2203_Type), Intent(InOut) :: RNG
    Integer(il) :: y
    Real(dp), Parameter :: two32 = 2._dp**32
    Real(dp), Parameter :: invtwo32m1 = 1._dp / (two32 - 1._dp)
    
    y = RNG%i()
    If (y .LT. 0_il) Then
        x = (Real(y,dp) + two32) * invtwo32m1
    Else
        x = Real(y,dp) * invtwo32m1
    End If
End Function rng_mt2203_r

Subroutine save_RNG_mt2203(RNG,fname)
    Use Kinds, Only: il
    Use FileIO_Utilities, Only: Var_to_file
    Implicit None
    Class(MT2203_Type), Intent(In) :: RNG
    Character(*), Intent(In) :: fname
    Integer(il) :: state(1:n2203+1+3)
    
    state(1:n2203) = RNG%mt
    state(n2203+1) = RNG%mti
    state(n2203+1+1) = RNG%aj
    state(n2203+1+2) = RNG%bj
    state(n2203+1+3) = RNG%cj
    Call Var_to_File(state,fname)
End Subroutine save_RNG_mt2203

Subroutine load_RNG_mt2203(RNG,fname)
    Use Kinds, Only: il
    Use FileIO_Utilities, Only: Var_from_file
    Implicit None
    Class(MT2203_Type), Intent(InOut) :: RNG
    Character(*), Intent(In) :: fname
    Integer(il) :: state(1:n2203+1+3)
    
    Call Var_from_File(state,fname)
    RNG%mt = state(1:n2203)
    RNG%mti = state(n2203+1)
    RNG%aj = state(n2203+1+1)
    RNG%bj = state(n2203+1+2)
    RNG%cj = state(n2203+1+3)
    RNG%seeded = .TRUE.
End Subroutine load_RNG_mt2203

End Module PRNGs

Module PRNGs

    Use Kinds, Only: i4
    Use Kinds, Only: i8
    Implicit None
    Private
    Public :: seed_rng_mt19937
    Public :: rng_mt19937
    Public :: seed_rng_mt19937x64
    Public :: rng_mt19937x64

    Integer(i4), Parameter :: default_seed = 4357_i4
    ! Period parameters
    Integer(i4), Parameter :: n = 624_i4
    Integer(i4), Parameter :: m = 397_i4
    !the array for the state vector
    Integer(i4), SAVE :: mt(0:n-1)
    Integer(i4), SAVE :: mti = n+1_i4 !mti==n+1 means mt[n] is not initialized
    Logical, SAVE :: seeded = .FALSE.
    
    Integer(i8), Parameter :: default_seed64 = 4357_i8
    ! Period parameters
    Integer(i8), Parameter :: n64 = 312_i8
    Integer(i8), Parameter :: m64 = 156_i8
    !the array for the state vector
    Integer(i8), SAVE :: mt64(n64)
    Integer(i8), SAVE :: mti64 = n64+1_i8 !mti==n+1 means mt[n] is not initialized
    Logical, SAVE :: seeded64 = .FALSE.

Contains

Subroutine seed_rng_mt19937x64(seed)
    Use Kinds, Only: i8
    Implicit None
    Integer(i8), Intent(In) :: seed
    Integer(i8) :: i

    mt64(1) = seed
    Do i = 1_i8,n64-1_i8
        mt64(i+1_i8) = 6364136223846793005_i8 * IEOR(mt64(i), ISHFT(mt64(i), -62_i8)) + i
    End Do
    mti64 = n64
    seeded64 = .TRUE.
End Subroutine seed_rng_mt19937x64

Subroutine seed_rng_mt19937(seed)
    Use Kinds, Only: dp
    Use Kinds, Only: i4
    Implicit None
    Integer(i4), Intent(In) :: seed
    Integer(i4) :: i
    !Real(dp) :: temp
    !Integer(i4) :: itemp, itemp2
    !Real(dp), Parameter :: two31 = 2._dp**31
    
    mt(0) = IAND(seed, -1_i4)
    Do i = 1_i4,n-1_i4
        mt(i) = IAND(69069 * mt(i-1_i4),-1_i4)
        ! temp = 69069._dp * Real(mt(i-1),dp)
        ! itemp = Int(Mod(temp,two31))
        ! itemp2 = Int(temp / two31)
        ! If (Mod(itemp2,2) .NE. 0) itemp = Int( Real(itemp,dp) + Sign(two31,Real(-itemp,dp)) )
        ! mt(i) = itemp
    End Do
    mti = n
    seeded = .TRUE.
End Subroutine seed_rng_mt19937

Function rng_mt19937x64() Result(x)
    Use Kinds, Only: dp
    Use Kinds, Only: i8
    Implicit None
    Real(dp) :: x
    Integer(i8) :: y
    Integer(i8) :: i
    Integer(i8), Parameter :: mag01(0:1) = (/ 0_i8, -5403634167711393303_i8 /)
    Integer(i8), Parameter :: um = -2147483648_i8 ! most significant 33 bits
    Integer(i8), Parameter :: lm =  2147483647_i8 ! least significant 31 bits
    Real(dp), Parameter :: two53 = 2._dp**53

    If (mti64 .GE. n64) Then ! generate nn words at one time
        If (seeded64) Then  !needs new words
            Do i = 1_i8,n64-m64
                y = IOR(IAND(mt64(i),um), IAND(mt64(i+1), lm))
                mt64(i) = IEOR(IEOR(mt64(i+m64), ISHFT(y, -1_i8)), mag01(IAND(y, 1_i8)))
            End Do
            Do i = n64-m64+1_i8,n64-1_i8
                y = IOR(IAND(mt64(i), um), IAND(mt64(i+1_i8), lm))
                mt64(i) = IEOR(IEOR(mt64(i+m64-n64), ISHFT(y, -1_i8)), mag01(IAND(y, 1_i8)))
            End Do
            y = IOR(IAND(mt64(n64), um), IAND(mt64(1_i8), lm))
            mt64(n64) = IEOR(IEOR(mt64(m64), ISHFT(y, -1_i8)), mag01(IAND(y, 1_i8)))
            mti64 = 0_i8
        Else  !needs seeding
            Call seed_rng_mt19937x64(default_seed64)
        End If
    End If
    mti64 = mti64 + 1_i8
    y = mt64(mti64)
    y = IEOR(y, IAND(ISHFT(y,-29_i8), 6148914691236517205_i8))
    y = IEOR(y, IAND(ISHFT(y, 17_i8), 8202884508482404352_i8))
    y = IEOR(y, IAND(ISHFT(y, 37_i8),   -2270628950310912_i8))
    y = IEOR(y, ISHFT(y, -43_i8))
    x = real(ISHFT(y, -11_i8),dp) / two53
End Function rng_mt19937x64

Function rng_mt19937() Result(x)
    Use Kinds, Only: dp
    Use Kinds, Only: i4
    Implicit None
    Real(dp) :: x
    Integer(i4) :: kk,y
    Integer(i4), Parameter :: mag01(0:1) = (/ 0_i4 , -1727483681_i4 /)!constant vector a
    Integer(i4), Parameter :: lmask = 2147483647_i4 !least significant r bits
    Integer(i4), Parameter :: umask = -2147483647_i4 - 1_i4 !most significant w-r bits
    Integer(i4), Parameter :: tmaskb = -1658038656_i4 !tempering parameters
    Integer(i4), Parameter :: tmaskc = -272236544_i4 !tempering parameters
    Real(dp), Parameter :: two32 = 2._dp**32
    Real(dp), Parameter :: two32m1 = 2._dp**32 - 1._dp

    If (mti .GE. n) Then  !generate N words at one time
        If (seeded) Then  !needs new words
            Do  kk = 0_i4,n-m-1_i4
                y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1_i4),lmask))
                mt(kk) = IEOR(IEOR(mt(kk+m), ISHFT(y,-1_i4)),mag01(IAND(y,1_i4)))
            End Do
            Do  kk = n-m,n-2_i4
                y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1_i4),lmask))
                mt(kk) = IEOR(IEOR(mt(kk+(m-n)), ISHFT(y,-1_i4)),mag01(IAND(y,1_i4)))
            End Do
            y = IOR(IAND(mt(n-1_i4),umask), IAND(mt(0_i4),lmask))
            mt(n-1_i4) = IEOR(IEOR(mt(m-1_i4), ISHFT(y,-1_i4)),mag01(IAND(y,1_i4)))
            mti = 0_i4
        Else  !needs seeding
            Call seed_rng_mt19937(default_seed)
        End If
    End If
    y = mt(mti)
    mti = mti + 1_i4
    y = IEOR(y, tshftu(y))
    y = IEOR(y, IAND(tshfts(y),tmaskb))
    y = IEOR(y, IAND(tshftt(y),tmaskc))
    y = IEOR(y, tshftl(y))
    If (y .LT. 0_i4) Then
      x = (Real(y,dp) + two32) / two32m1
    Else
      x = Real(y,dp) / two32m1
    End If
End Function rng_mt19937

Function tshftu(y) Result(x)
    Implicit None
    Integer :: x
    Integer, Intent(In) :: y

    x = ISHFT(y,-11)
End Function tshftu

Function tshfts(y) Result(x)
    Implicit None
    Integer :: x
    Integer, Intent(In) :: y
    
    x = ISHFT(y,7)
End Function tshfts

Function tshftt(y) Result(x)
    Implicit None
    Integer :: x
    Integer, Intent(In) :: y

    x = ISHFT(y,15)
End Function tshftt

Function tshftl(y) Result(x)
    Implicit None
    Integer :: x
    Integer, Intent(In) :: y

    x = ISHFT(y,-18)
End Function tshftl

End Module PRNGs

Module PRNGs

    Implicit None
    Private
    Public :: seed_rng_mt19937
    Public :: rng_mt19937

    Integer, Parameter :: default_seed = 4357
    ! Period parameters
    Integer, Parameter :: n = 624
    Integer, Parameter :: n1 = n+1
    Integer, Parameter :: m = 397
    Integer, Parameter :: mata = -1727483681 !constant vector a
    Integer, Parameter :: umask = -2147483647 - 1 !most significant w-r bits
    Integer, Parameter :: lmask =  2147483647 !least significant r bits
    ! Tempering parameters
    Integer, Parameter :: tmaskb = -1658038656
    Integer, Parameter :: tmaskc = -272236544

    !the array for the state vector
    Integer, SAVE :: mt(0:n-1)
    Integer, SAVE :: mti = n1 !mti==N+1 means mt[N] is not initialized
    Integer, SAVE :: mag01(0:1) = (/ 0, mata /) !mag01(x) = x * MATA for x=0,1
    
Contains

Subroutine seed_rng_mt19937(seed)
    Use Kinds, Only: dp
    Implicit None
    Integer, Intent(In) :: seed
    Real(dp) :: temp
    Integer :: itemp, itemp2
    Real(dp), Parameter :: two31 = 2._dp**31
    !    setting initial seeds to mt[N] using the generator Line 25 of Table 1 in
    !    [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.), pp102]

    mt(0) = IAND(seed, -1)
    Do mti = 1,n-1
    ! The following code in this loop is equivalent to:
    ! the single line of code:
    ! mt(mti) = IAND(69069 * mt(mti-1), -1)
    !
    ! The code here is used instead to prevent integer overflow.
        temp = 69069._dp * Real(mt(mti-1),dp)
        itemp = Int(Mod(temp,two31))
        itemp2 = Int(temp / two31)
        If (Mod(itemp2,2) .NE. 0) Then
            itemp = Int( Real(itemp,dp) + Sign(two31,Real(-itemp,dp)) )
            ! If (itemp .GT. 0) Then
            !     itemp = itemp - two31
            ! Else
            !     itemp = itemp + two31
            ! End If
        End If
        mt(mti) = itemp
    End Do
End Subroutine seed_rng_mt19937

Function rng_mt19937() Result(x)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x
    Integer :: kk,y
    Real(dp), Parameter :: two32 = 2._dp**32
    Real(dp), Parameter :: two32m1 = 2._dp**32 - 1._dp

    If (mti .GE. n) Then  !generate N words at one time
        If (mti .EQ. n+1) Call seed_rng_mt19937(default_seed)  !not yet seeded, a default initial seed is used
        Do  kk = 0,n-m-1
            y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
            mt(kk) = IEOR(IEOR(mt(kk+m), ISHFT(y,-1)),mag01(IAND(y,1)))
        End Do
        Do  kk = n-m,n-2
            y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
            mt(kk) = IEOR(IEOR(mt(kk+(m-n)), ISHFT(y,-1)),mag01(IAND(y,1)))
        End Do
        y = IOR(IAND(mt(n-1),umask), IAND(mt(0),lmask))
        mt(n-1) = IEOR(IEOR(mt(m-1), ISHFT(y,-1)),mag01(IAND(y,1)))
        mti = 0
    End If
    y = mt(mti)
    mti = mti + 1
    y = IEOR(y, tshftu(y))
    y = IEOR(y, IAND(tshfts(y),tmaskb))
    y = IEOR(y, IAND(tshftt(y),tmaskc))
    y = IEOR(y, tshftl(y))
    If (y .LT. 0) Then
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

MODULE mt19937

    IMPLICIT NONE
    INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12, 60)

    ! Period parameters
    INTEGER, PARAMETER :: n = 624, n1 = n+1, m = 397, mata = -1727483681
    !                                    constant vector a
    INTEGER, PARAMETER :: umask = -2147483647 - 1
    !                                    most significant w-r bits
    INTEGER, PARAMETER :: lmask =  2147483647
    !                                    least significant r bits
    ! Tempering parameters
    INTEGER, PARAMETER :: tmaskb= -1658038656, tmaskc= -272236544

    !                     the array for the state vector
    INTEGER, SAVE      :: mt(0:n-1), mti = n1
    !                     mti==N+1 means mt[N] is not initialized

    PRIVATE
    PUBLIC :: dp, sgrnd, grnd

CONTAINS

SUBROUTINE sgrnd(seed)

    INTEGER, INTENT(IN)   :: seed

    REAL (dp)  :: two31, temp
    INTEGER    :: itemp, itemp2
    !    setting initial seeds to mt[N] using the generator Line 25 of Table 1 in
    !    [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.), pp102]

    mt(0)= IAND(seed, -1)
    two31 = 2.D0**31
    DO  mti=1,n-1
    ! The following code in this loop is equivalent to:
    ! the single line of code:
    ! mt(mti) = IAND(69069 * mt(mti-1), -1)
    !
    ! The code here is used instead to prevent integer overflow.
       temp=69069.D0*DBLE(mt(mti-1))
       itemp=mod(temp,two31)
       itemp2=temp/two31
       if (mod(itemp2,2).ne.0) then
         if (itemp.gt.0) then
           itemp=itemp-two31
         else
           itemp=itemp+two31
         endif
       endif
       mt(mti)=itemp
    END DO


    RETURN
END SUBROUTINE sgrnd

FUNCTION grnd() RESULT(fn_val)

    REAL (dp) :: fn_val

    INTEGER, SAVE :: mag01(0:1) = (/ 0, mata /)
    !                        mag01(x) = x * MATA for x=0,1
    INTEGER       :: kk, y

    ! These statement functions have been replaced with separate functions
    ! tshftu(y) = ISHFT(y,-11)
    ! tshfts(y) = ISHFT(y,7)
    ! tshftt(y) = ISHFT(y,15)
    ! tshftl(y) = ISHFT(y,-18)

    IF(mti >= n) THEN
    !                       generate N words at one time
      IF(mti == n+1) THEN
    !                            if sgrnd() has not been called,
        CALL sgrnd(4357)
    !                              a default initial seed is used
      END IF

      DO  kk = 0, n-m-1
        y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
        mt(kk) = IEOR(IEOR(mt(kk+m), ISHFT(y,-1)),mag01(IAND(y,1)))
      END DO
      DO  kk = n-m, n-2
        y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
        mt(kk) = IEOR(IEOR(mt(kk+(m-n)), ISHFT(y,-1)),mag01(IAND(y,1)))
      END DO
      y = IOR(IAND(mt(n-1),umask), IAND(mt(0),lmask))
      mt(n-1) = IEOR(IEOR(mt(m-1), ISHFT(y,-1)),mag01(IAND(y,1)))
      mti = 0
    END IF

    y = mt(mti)
    mti = mti + 1
    y = IEOR(y, tshftu(y))
    y = IEOR(y, IAND(tshfts(y),tmaskb))
    y = IEOR(y, IAND(tshftt(y),tmaskc))
    y = IEOR(y, tshftl(y))

    IF(y < 0) THEN
      fn_val = (DBLE(y) + 2.0D0**32) / (2.0D0**32 - 1.0D0)
    ELSE
      fn_val = DBLE(y) / (2.0D0**32 - 1.0D0)
    END IF
End Function grnd

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

End Module MT19937

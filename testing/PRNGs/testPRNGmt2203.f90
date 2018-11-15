Program testPRNGmt2203

Use Kinds, Only: dp
Use Kinds, Only: il
Use Kinds, Only: id
Use PRNGs, Only: MT2203_Type
Use FileIO_Utilities, Only: delta_Time
Use FileIO_Utilities, Only: creturn

Implicit None

Integer, Parameter :: n = 2**10
Integer, Parameter :: n2203s = 6024
Type(MT2203_Type) :: RNG_2203
Real(dp) :: r(1:n)
Integer :: i,j,unit1,unit2
Integer(il) :: c(1:n2203s)
Real(dp) :: t(1:n2203s)

Open(NEWUNIT=unit2,FILE='randoms2203.stream',ACTION='WRITE',STATUS='REPLACE',ACCESS='STREAM')
Open(NEWUNIT=unit1,FILE='randoms2203.tst',ACTION='WRITE',STATUS='REPLACE')
Write(*,*)
Do j = 1,n2203s
    Write(*,'(A,I4.4,A)',ADVANCE='NO') 'Running PRNG:  MT2203-',j,creturn
    !seed the generator
    Call RNG_2203%seed(j,7777777_il)
    !generate a stream of n integer and real values from each generator and collect timing data
    r = 0._dp
    Call SYSTEM_CLOCK(c(j))
    Do i = 1,n
        r(i) = RNG_2203%r()
    End Do
    t(j) = delta_Time(clock_then=c(j))
    Do i = 1,n
        Write(unit1,'(ES30.16)') r(i)
    End Do
    !Write 4n binary values to file (in stream format)
    Do i = 1,n*4
        Write(unit2) RNG_2203%i()
    End Do

End Do
Write(*,*)
Close(unit1)
Close(unit2)

!Print timing data
Write(*,*)
Write(*,'(A)') 'Real generation rate:'
Write(*,'(A,ES13.6E2,A)') 'MT2203:    ',1._dp / (Sum(t) / Real(n*n2203s,dp)),' rand per sec'

End Program testPRNGmt2203

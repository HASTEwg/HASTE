Program testPRNGmt19937

Use Kinds, Only: dp
Use Kinds, Only: il
Use Kinds, Only: id
Use PRNGs, Only: MT19937_Type
Use PRNGs, Only: MT19937x64_Type
Use FileIO_Utilities, Only: delta_Time

Implicit None

Integer, Parameter :: n = 2**20
Integer, Parameter :: nb = (n/32)*1000
Integer, Parameter :: n64 = 2**20
Integer, Parameter :: nb64 = (n/64)*1000
Type(MT19937_Type) :: RNG_19937
Type(MT19937x64_Type) :: RNG_19937x64
Real(dp) :: r(1:n),r64(1:n)
Integer :: i,unit1,unit2
Integer(il) :: c,c64
Real(dp) :: t,t64

!seed the generators
Call RNG_19937%seed(7777777_il)
Call RNG_19937x64%seed(7777777_id)
!generate a stream of n integer and real values from each generator and collect timing data
r = 0._dp
Call SYSTEM_CLOCK(c)
Do i = 1,n
    r(i) = RNG_19937%r()
End Do
t = delta_Time(clock_then=c)
r64 = 0._dp
Call SYSTEM_CLOCK(c64)
Do i = 1,n64
    r64(i) = RNG_19937x64%r()
End Do
t64 = delta_Time(clock_then=c64)
!Write n binary values to file (in stream format)
Open(NEWUNIT=unit1,FILE='randoms19937.stream',ACTION='WRITE',STATUS='REPLACE',ACCESS='STREAM')
Open(NEWUNIT=unit2,FILE='randoms19937x64.stream',ACTION='WRITE',STATUS='REPLACE',ACCESS='STREAM')
Do i = 1,nb
    Write(unit1) RNG_19937%i()
End Do
Do i = 1,nb64
    Write(unit2) RNG_19937x64%i()
End Do
Close(unit1)
Close(unit2)
!Write n real values to file
Open(NEWUNIT=unit1,FILE='randoms19937.tst',ACTION='WRITE',STATUS='REPLACE')
Open(NEWUNIT=unit2,FILE='randoms19937x64.tst',ACTION='WRITE',STATUS='REPLACE')
Do i = 1,n
    Write(unit1,'(ES30.16)') r(i)
End Do
Do i = 1,n64
    Write(unit2,'(ES30.16)') r64(i)
End Do
Close(unit1)
Close(unit2)

!Print timing data
Write(*,*)
Write(*,'(A)') 'Real generation rate:'
Write(*,'(A,ES13.6E2,A)') 'MT19937:    ',Real(n  ,dp) / t  ,' rand per sec'
Write(*,'(A,ES13.6E2,A)') 'MT19937x64: ',Real(n64,dp) / t64,' rand per sec'

End Program testPRNGmt19937

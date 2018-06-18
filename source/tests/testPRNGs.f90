Program testPRNGs

Use Kinds, Only: dp
Use Kinds, Only: i8
Use PRNGs, Only: MT19937_Type
Use PRNGs, Only: MT19937x64_Type
Use Statistics, Only: Check_Uniform_AD

Implicit None

Integer, Parameter :: n = 2**20 !just over 1 million
Type(MT19937_Type) :: RNG_19937
Type(MT19937x64_Type) :: RNG_19937x64
Real(dp) :: r(1:n),r64(1:n)
Real(dp) :: b(1:n),b64(1:n)
Integer :: i,unit1,unit2
Logical :: AD_result(1:4)
Real(dp) :: AD

!Write a stream of n reals
Open(NEWUNIT=unit1,FILE='randoms19937.tst',ACTION='WRITE',STATUS='REPLACE')
Open(NEWUNIT=unit2,FILE='randoms19937x64.tst',ACTION='WRITE',STATUS='REPLACE')
Call RNG_19937%seed(7777777_i4)
Call RNG_19937x64%seed(7777777_i8)
Do i = 1,n
    r(i) = RNG_19937%r()
    r64(i) = RNG_19937x64%r()
    Write(unit1,'(ES30.20)') r(i)
    Write(unit2,'(ES30.20)') r64(i)
End Do
Close(unit1)
Close(unit2)

!Write a stream of n binary values
Open(NEWUNIT=unit1,FILE='randoms19937b.tst',ACTION='WRITE',STATUS='REPLACE')
Open(NEWUNIT=unit2,FILE='randoms19937x64b.tst',ACTION='WRITE',STATUS='REPLACE')
Call RNG_19937%seed(7777777_i4)
Call RNG_19937x64%seed(7777777_i8)
Do i = 1,n
    b(i) = RNG_19937%i()
    b64(i) = RNG_19937x64%i()
    Write(unit1,'(B32)') b(i)
    Write(unit2,'(B32)') b64(i)
End Do
Close(unit1)
Close(unit2)

!Apply a quick check (Anderson Darling) for uniform distribution
Call Check_Uniform_AD(x,AD_result,AD)
Write(*,'(A)') 'AD Test for Uniform Distribution: mt19937'
Write(*,'(A,F0.6)') '  AD statistic: ',AD
Write(*,'(A,L1,A)') '  Assert non-uniformity?  ',.NOT.AD_result(1),'  Possibly non-uniform.'
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(2),'  |  |  |  |  |  |  | '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(3),'  v  v  v  v  v  v  v '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(4),'  Very likely non-uniform.'
If (All(AD_result)) Then
    Write(*,'(A)')  '  The sample is LIKELY to be from a uniform distribution.'
Else
    Write(*,'(A)')  '  The sample is UNLIKELY to be from a uniform distribution.'
End If
Write(*,*)
Call Check_Uniform_AD(x64,AD_result,AD)
Write(*,'(A)') 'AD Test for Uniform Distribution: mt19937 x64'
Write(*,'(A,F0.6)') '  AD statistic: ',AD
Write(*,'(A,L1,A)') '  Assert non-uniformity?  ',.NOT.AD_result(1),'  Possibly non-uniform.'
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(2),'  |  |  |  |  |  |  | '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(3),'  v  v  v  v  v  v  v '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(4),'  Very likely non-uniform.'
If (All(AD_result)) Then
    Write(*,'(A)')  '  The sample is LIKELY to be from a uniform distribution.'
Else
    Write(*,'(A)')  '  The sample is UNLIKELY to be from a uniform distribution.'
End If
Write(*,*)
End Program testPRNGs

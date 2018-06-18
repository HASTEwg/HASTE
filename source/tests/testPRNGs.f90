Program testPRNGs

Use Kinds, Only: dp
Use Kinds, Only: i8
Use PRNGs, Only: seed_rng_mt19937
Use PRNGs, Only: rng_mt19937
Use PRNGs, Only: seed_rng_mt19937x64
Use PRNGs, Only: rng_mt19937x64
Use Statistics, Only: Check_Uniform_AD

Implicit None

Integer, Parameter :: n = 100000
Real(dp) :: x(1:n),x64(1:n)
Integer :: i,unit1,unit2
Logical :: AD_result(1:4)
Real(dp) :: AD

Open(NEWUNIT=unit1,FILE='randoms19937.tst',ACTION='WRITE',STATUS='REPLACE')
Open(NEWUNIT=unit2,FILE='randoms19937x64.tst',ACTION='WRITE',STATUS='REPLACE')
Call seed_rng_mt19937(77777)
Call seed_rng_mt19937x64(77777_i8)
Do i = 1,n
    x(i) = rng_mt19937()
    x64(i) = rng_mt19937x64()
    Write(unit1,'(ES30.20)') x(i)
    Write(unit2,'(ES30.20)') x64(i)
End Do
Close(unit1)
Close(unit2)

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
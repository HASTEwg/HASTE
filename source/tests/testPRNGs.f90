Program testPRNGs

Use Kinds, Only: dp
Use Kinds, Only: i4
Use Kinds, Only: i8
Use PRNGs, Only: MT19937_Type
Use PRNGs, Only: MT19937x64_Type
Use Sorting, Only: Quick_Sort
Use Statistics, Only: Check_Uniform_AD
Use Statistics, Only: Check_Exponential_AD

Implicit None

Integer, Parameter :: n = 100000
Type(MT19937_Type) :: RNG_19937
Type(MT19937x64_Type) :: RNG_19937x64
Real(dp) :: r(1:n),r64(1:n)
Integer(i4) :: b(1:n)
Integer(i8) :: b64(1:n)
Integer :: i,unit1,unit2
Logical :: AD_result(1:4)
Real(dp) :: AD


!Write a stream of n binary values
Open(NEWUNIT=unit1,FILE='randoms19937b.tst',ACTION='WRITE',STATUS='REPLACE')
Open(NEWUNIT=unit2,FILE='randoms19937x64b.tst',ACTION='WRITE',STATUS='REPLACE')
Call RNG_19937%seed(7777777_i4)
Call RNG_19937x64%seed(7777777_i8)
Do i = 1,n
    b(i) = RNG_19937%i()
    b64(i) = RNG_19937x64%i()
    Write(unit1,'(B32.32)') b(i)
    Write(unit2,'(B64.64)') b64(i)
End Do
Close(unit1)
Close(unit2)

!Print a block of binary values from each generator for visualization
Write(*,'(A)') 'MT19937'
Do i = 1,32
    Write(*,'(B32.32)') b(i)
End Do
Write(*,'(A)') 'MT19937x64'
Do i = 1,32
    Write(*,'(B64.64)') b64(i)
End Do
Write(*,*)

!Write a stream of n reals
Open(NEWUNIT=unit1,FILE='randoms19937.tst',ACTION='WRITE',STATUS='REPLACE')
Open(NEWUNIT=unit2,FILE='randoms19937x64.tst',ACTION='WRITE',STATUS='REPLACE')
Do i = 1,n
    r(i) = RNG_19937%r()
    r64(i) = RNG_19937x64%r()
    Write(unit1,'(ES30.20)') r(i)
    Write(unit2,'(ES30.20)') r64(i)
End Do
Close(unit1)
Close(unit2)

!Apply a quick check (Anderson Darling) for uniform distribution
Call Check_Uniform_AD(r,AD_result,AD)
Write(*,'(A)') 'AD Test for Uniform Distribution: mt19937'
Write(*,'(A,F0.6)') '  AD statistic: ',AD
Write(*,'(A,L1,A)') '  Assert non-uniformity?  ',.NOT.AD_result(1),'  Possibly non-uniform.'
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(2),'  |  |  |  |  |  |  | '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(3),'  V  V  V  V  V  V  V '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(4),'  Very likely non-uniform.'
If (All(AD_result)) Then
    Write(*,'(A)')  '  The sample is LIKELY to be from a uniform distribution.'
Else
    Write(*,'(A)')  '  The sample is UNLIKELY to be from a uniform distribution.'
End If
Write(*,*)
!Compute spacings in the real stream (in place)
Call Quick_Sort(r)
Do i = n,2,-1
    r(i) = r(i) - r(i-1)
End Do
Call Check_Exponential_AD(r,AD_result,AD)
Write(*,'(A)') 'AD Test for Exponentially Distributed Spacing: mt19937'
Write(*,'(A,F0.6)') '  AD statistic: ',AD
Write(*,'(A,L1,A)') '  Assert non-exp spaces?  ',.NOT.AD_result(1),'  Possibly non-exp spaced.'
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(2),'  |  |  |  |  |  |  |  | '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(3),'  V  V  V  V  V  V  V  V '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(4),'  Very likely non-exp spaced.'
If (All(AD_result)) Then
    Write(*,'(A)')  '  The spaces in the sample are LIKELY to be exponentially distributed.'
Else
    Write(*,'(A)')  '  The spaces in the sample are UNLIKELY to be exponentially distributed.'
End If
Write(*,*)
Write(*,*)
Call Check_Uniform_AD(r64,AD_result,AD)
Write(*,'(A)') 'AD Test for Uniform Distribution: mt19937 x64'
Write(*,'(A,F0.6)') '  AD statistic: ',AD
Write(*,'(A,L1,A)') '  Assert non-uniformity?  ',.NOT.AD_result(1),'  Possibly non-uniform.'
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(2),'  |  |  |  |  |  |  | '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(3),'  V  V  V  V  V  V  V '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(4),'  Very likely non-uniform.'
If (All(AD_result)) Then
    Write(*,'(A)')  '  The sample is LIKELY to be from a uniform distribution.'
Else
    Write(*,'(A)')  '  The sample is UNLIKELY to be from a uniform distribution.'
End If
Write(*,*)
!Compute spacings in the real stream (in place)
Call Quick_Sort(r64)
Do i = n,2,-1
    r64(i) = r64(i) - r64(i-1)
End Do
Call Check_Exponential_AD(r64,AD_result,AD)
Write(*,'(A)') 'AD Test for Exponentially Distributed Spacing: mt19937'
Write(*,'(A,F0.6)') '  AD statistic: ',AD
Write(*,'(A,L1,A)') '  Assert non-exp spaces?  ',.NOT.AD_result(1),'  Possibly non-exp spaced.'
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(2),'  |  |  |  |  |  |  |  | '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(3),'  V  V  V  V  V  V  V  V '
Write(*,'(A,L1,A)') '                          ',.NOT.AD_result(4),'  Very likely non-exp spaced.'
If (All(AD_result)) Then
    Write(*,'(A)')  '  The spaces in the sample are LIKELY to be exponentially distributed.'
Else
    Write(*,'(A)')  '  The spaces in the sample are UNLIKELY to be exponentially distributed.'
End If
Write(*,*)
End Program testPRNGs

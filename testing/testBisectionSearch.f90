Program testBisectionSearch

Use Kinds, Only: dp
Use Utilities, Only: Bisection_Search
Use Sorting, Only: Quick_Sort

Implicit None

Real(dp), Parameter :: seed(1) = 7777777
Integer, Parameter :: n = 20
Integer, Parameter :: m = 10
Real(dp) :: r_list(1:n)
Real(dp) :: r_test(1:m*n)
Integer :: i,j
Integer :: found(1:n)
Integer :: missed(1:n)

Call RANDOM_SEED(seed)
Call RANDOM_NUMBER(r_list)
Call Quick_Sort(r_list)
Call RANDOM_NUMBER(r_test)

found = 0
missed = 0
!search for distributed values in the list
Do i = 1,n*m
    r = r_test(i)
    j = Bisection_Search(r,r_list,n)
    !check the returned location
    If (r.GT.r_list(j-1) .AND. r.LE.r_list(j)) Then
        found(i) = found(i) + 1
    Else
        missed(i) = missed(i) + 1
    End If
End Do
!search for boundary values in the list
Do i = 1,n
    r = r_list(i)
    j = Bisection_Search(r,r_list,n)
    !check the returned location
    If (r.GT.r_list(j-1) .AND. r.LE.r_list(j)) Then
        found(i) = found(i) + 1
    Else
        missed(i) = missed(i) + 1
    End If
End Do
!print summary of found/missed to screen
Write(*,*)
Write(*,'(A,I0,A,I0,A,F0.2,A)') 'Found ',Sum(found),' of ',n+n*m,' (',100._dp*Real(Sum(found),dp)/Real(n+n*m,dp),'%)'
Write(*,'(A)') '----------------------------------------'
Write(*,'(3A10)') '   Bin  ','  Found ',' Missed '
Write(*,'(3A10)') '--------','--------','--------'
Do i = 1,n
    Write(*,'(3I10)') i,found(i),missed(i)
End Do
Write(*,'(A)') '----------------------------------------'
!check out of bounds behavior and print to screen
Write(*,'(A)') 'Checking out-of-bounds behavior...'
j = Bisection_Search(0._dp-SPACING(0._dp),r_list,n)
Write(*,'(A,I0)',ADVANCE="NO") '    Below min: j = ',j
If (j .EQ. 1) Then
    Write(*,'(A)') ' (FOUND)'
Else
    Write(*,'(A)') ' (MISSED)'
End If
j = Bisection_Search(1._dp+SPACING(1._dp),r_list,n)
Write(*,'(A,I0)',ADVANCE="NO") '    Above max: j = ',j
If (j .EQ. n+1) Then
    Write(*,'(A)') ' (FOUND)'
Else
    Write(*,'(A)') ' (MISSED)'
End If
Write(*,*)

End Program testBisectionSearch

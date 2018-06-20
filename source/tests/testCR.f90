Program testCR

Use FileIO_Utilities, Only: creturn,wait
Implicit None
Integer:: i

Write(*,*)
Do i = 1,10
    Write(*,'(A,I0,A)',ADVANCE='NO') 'Testing...',i,creturn
    Call Wait(500)
End Do
Write(*,'(A)') 'Done.          '
Write(*,*)

End Program
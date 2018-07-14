Program testKinds

Use Kinds

Implicit None

Write(*,*)
Write(*,'(A,I0,A,F10.6,A)') 'SINGLE precision (',sp,' bytes) gives ',-Log10(2._sp*Spacing(1._sp)),' digits of precision.'
Write(*,'(A,I0,A,F19.15,A)') 'DOUBLE precision (',dp,' bytes) gives ',-Log10(2._dp*Spacing(1._dp)),' digits of precision.'

End Program testKinds
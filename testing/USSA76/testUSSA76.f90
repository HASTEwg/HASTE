Program testUSSA76

Use Kinds, Only: dp
Use US_Std_Atm_1976, Only: rho
Implicit None

Integer :: i
Integer, Parameter :: n = 10
Real(dp) :: z


z = 0._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 86._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 90._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 91._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 92._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 97._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 100._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 115._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 250._dp
Write(*,'(F9.3,F32.15)') z,rho(z)
z = 500._dp
Write(*,'(F9.3,F32.15)') z,rho(z)

End Program
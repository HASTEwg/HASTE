Program testDirGridWt

Use Kinds, Only: dp

Implicit None

Real(dp), Parameter :: x_hat(1:3) = (/1._dp,0._dp,0._dp/)
Real(dp), Parameter :: y_hat(1:3) = (/0._dp,1._dp,0._dp/)
Real(dp), Parameter :: z_hat(1:3) = (/0._dp,0._dp,1._dp/)
Real(dp), Parameter :: Pi = 3.141592653589793238462643383279502884_dp
Real(dp), Parameter :: TwoPi = 2._dp * Pi
Integer, Parameter :: n_mu = 3
Integer, Parameter :: n_omega = 3
Real(dp) :: dirs_list(1:3,1:n_mu,1:n_omega)
Integer :: dirs_tally(1:n_mu,1:n_omega)
Integer :: i,j,n
Integer, Parameter :: n_trials = 100000000
Real(dp) :: mu,omega
Real(dp) :: d(1:3),dot
Integer :: loc(1:2)

Do i = 1,n_mu
    mu = (Real(2*(i-1)+1,dp) / Real(n_mu,dp)) - 1._dp!-1._dp + (1._dp + Real(i-1,dp)*2._dp) / Real(n_mu,dp)
    Do j = 1,n_omega
        omega = Pi*(Real(2*(j-1)+1,dp) / Real(n_omega,dp)) - Pi!(Pi + Real(j-1,dp)*2._dp*Pi) / Real(n_omega,dp)
        dirs_list(1:3,i,j) = mu * z_hat + Sqrt(1._dp - mu**2) * (Cos(omega) * x_hat + Sin(omega) * y_hat)
    End Do
End Do
dirs_tally = 0

Call RANDOM_SEED()
Do n = 1,n_trials
    Call RANDOM_NUMBER(omega)
    omega = TwoPi * omega
    Call RANDOM_NUMBER(mu)
    mu = 2._dp * mu - 1._dp
    d = (/ Sqrt(1._dp - mu**2) * Cos(omega), &
         & Sqrt(1._dp - mu**2) * Sin(omega), &
         & mu                                /)
    dot = -1._dp
    loc = (/0,0/)
    Do i = 1,n_mu
        Do j = 1,n_omega
            If (Dot_Product(dirs_list(:,i,j),d) .GT. dot) Then
                loc = (/i,j/)
                dot = Dot_Product(dirs_list(:,i,j),d)
            End If
        End Do
    End Do
    dirs_tally(loc(1),loc(2)) = dirs_tally(loc(1),loc(2)) + 1
    If (Mod(n,10000).EQ.0) Then
        Write(*,'(F8.2,A)',ADVANCE='NO') 100._dp * Real(n,dp) / Real(n_trials,dp),'%'//achar(13)
        !Write(*,'(5F9.6,A)',ADVANCE='NO') Real(dirs_tally(:,n_omega/2),dp) / (Real(n,dp)/Real(n_mu*n_omega,dp)),achar(13)
    End If
End Do

Write(*,*)
Write(*,*)
Do i = 1,n_omega
    Write(*,'(3F9.6)') Real(dirs_tally(:,i),dp) / (Real(n_trials)/Real(n_mu*n_omega,dp))
End Do
End Program testDirGridWt

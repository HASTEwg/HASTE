!-------------------------------------------------------------------------------
!   Copyright (C) 2017  Whitman T. Dailey
!   
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License version 3 as 
!   published by the Free Software Foundation.
!   
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!   
!   You should have received a copy of the GNU General Public License
!   along with this program.  If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------
Module Statistics
    
    Use Kinds, Only: dp
    Implicit None
    Private
    Public :: Do_stats
    Public :: Std_Err
    Public :: std_devs_for_95CI
    Public :: aMean
    Public :: gMean
    Public :: Check_normal_AD
    Public :: Check_exponential_AD
    Public :: Check_uniform_AD

    Interface Std_Err
        Module Procedure Std_Err_real_N
        Module Procedure Std_Err_int4_N
        Module Procedure Std_Err_int8_N
        ! Module Procedure Std_Err_wt_real_N
        ! Module Procedure Std_Err_wt_int4_N
        ! Module Procedure Std_Err_wt_int8_N
    End Interface
    
    Real(dp), Parameter :: std_devs_for_95CI = 1.9599639845400542355245944305205515279555500778695483984769526464_dp
    
Contains

Elemental Function Std_Err_real_N(N,mu,mu_sq) Result(s_mu)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: s_mu
    Real(dp), Intent(In) :: N  !n is passed as a real representation of the integer quantity to accomodate numbers > Huge(n)
    Real(dp), Intent(In) :: mu  !sum of N estimates
    Real(dp), Intent(In) :: mu_sq  !sum of squares of N estimates
    Real(dp) :: x
    
    If (N .LE. 1._dp) Then !default to a 100% rel std err, but make negative to flag
        s_mu = -mu
    Else  !otherwise, 2 or more trials were performed, compute standard error
        x = mu_sq - (mu**2 / N)
        If (x .LT. 0._dp) Then !default to a 100% rel std err, but make negative to flag
            s_mu = -mu
            Return
        End If
        s_mu = Sqrt( x / (N * (N - 1._dp)) )
    End If
End Function Std_Err_real_N

! Elemental Function Std_Err_wt_real_N(N,mu,w_mu,w_mu_sq,w,w_sq) Result(s_mu)
!     Use Kinds, Only: dp
!     Implicit None
!     Real(dp) :: s_mu
!     Real(dp), Intent(In) :: N  !n is passed as a real representation of the integer quantity to accomodate numbers > Huge(n)
!     Real(dp), Intent(In) :: mu  !sum of N estimates
!     Real(dp), Intent(In) :: w_mu  !sum of weight*estimate products
!     Real(dp), Intent(In) :: w_mu_sq  !sum of squared estimates multiplied by weight
!     Real(dp), Intent(In) :: w  !sum of weights applied to N estimates
!     Real(dp), Intent(In) :: w_sq  !sum of squared weights applied to N estimates
!     Real(dp) :: x
    
!     If (N .LE. 1._dp) Then  !default to a 100% rel std err, but make negative to flag
!         s_mu = -mu
!     Else  !otherwise, 2 or more trials were performed, compute standard error
!         x = w_mu_sq + N * w_mu * (w_mu / w - 2._dp * mu) / w
!         s_mu = Sqrt( x / (N * (w - w_sq / w)) )
!     End If
! End Function Std_Err_wt_real_N

Elemental Function Std_Err_int4_N(N,mu,mu_sq) Result(s_mu)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: s_mu
    Integer(4), Intent(In) :: N  !number of trials
    Real(dp), Intent(In) :: mu  !sum of N estimates
    Real(dp), Intent(In) :: mu_sq  !sum of squares of N estimates

    s_mu = Std_Err_real_N(Real(N,dp),mu,mu_sq)
End Function Std_Err_int4_N

Elemental Function Std_Err_int8_N(N,mu,mu_sq) Result(s_mu)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: s_mu
    Integer(8), Intent(In) :: N  !number of trials
    Real(dp), Intent(In) :: mu  !sum of N estimates
    Real(dp), Intent(In) :: mu_sq  !sum of squares of N estimates
    
    s_mu = Std_Err_real_N(Real(N,dp),mu,mu_sq)
End Function Std_Err_int8_N

! Elemental Function Std_Err_wt_int4_N(N,mu,w_mu,w_mu_sq,w,w_sq) Result(s_mu)
!     Use Kinds, Only: dp
!     Implicit None
!     Real(dp) :: s_mu
!     Integer(4), Intent(In) :: N  !number of trials
!     Real(dp), Intent(In) :: mu  !sum of N estimates
!     Real(dp), Intent(In) :: w_mu  !sum of weight*estimate products
!     Real(dp), Intent(In) :: w_mu_sq  !sum of squares of N estimates multiplied by weight
!     Real(dp), Intent(In) :: w  !sum of weights applied to N estimates
!     Real(dp), Intent(In) :: w_sq  !sum of squared weights applied to N estimates

!     s_mu = Std_Err_wt_real_N(Real(N,dp),mu,w_mu,w_mu_sq,w,w_sq)
! End Function Std_Err_wt_int4_N

! Elemental Function Std_Err_wt_int8_N(N,mu,w_mu,w_mu_sq,w,w_sq) Result(s_mu)
!     Use Kinds, Only: dp
!     Implicit None
!     Real(dp) :: s_mu
!     Integer(8), Intent(In) :: N  !number of trials
!     Real(dp), Intent(In) :: mu  !sum of N estimates
!     Real(dp), Intent(In) :: w_mu  !sum of weight*estimate products
!     Real(dp), Intent(In) :: w_mu_sq  !sum of squares of N estimates multiplied by weight
!     Real(dp), Intent(In) :: w  !sum of weights applied to N estimates
!     Real(dp), Intent(In) :: w_sq  !sum of squared weights applied to N estimates

!     s_mu = Std_Err_wt_real_N(Real(N,dp),mu,w_mu,w_mu_sq,w,w_sq)
! End Function Std_Err_wt_int8_N

Subroutine Do_stats(values,mu,sigma,CI_high,CI_low,normality_basic,normality_AD,RNG)
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Real(dp), Intent(In) :: values(:)
    Real(dp), Intent(Out) :: mu
    Real(dp), Intent(Out) :: sigma
    Real(dp), Intent(Out) :: CI_high(1:5),CI_low(1:5)
    Logical, Intent(Out) :: normality_basic,normality_AD(1:4)
    Type(RNG_Type), Intent(InOut) :: RNG
    Real(dp), Parameter :: confidence_for_interval(1:5) = (/ 0.99_dp , 0.95_dp , 0.9_dp, 0.8_dp , 0.5_dp /)
    Integer :: i
    
    mu = aMean(values)
    sigma = Std_Dev(values)
    Do i = 1,5
        Call Confidence_Interval_by_bootstrap(values,1._dp-confidence_for_interval(i),CI_high(i),CI_low(i),RNG)
    End Do
    normality_basic = Check_normal_BOE(values)
    Call Check_normal_AD(values,normality_AD)
End Subroutine Do_Stats

Function Check_normal_BOE(x) Result(is_normal)
    Use Kinds, Only: dp
    Implicit None
    Logical :: is_normal
    Real(dp), Intent(In) :: x(:)  !sample to be checked for normal distribution
    Real(dp) :: mu,sigma
    
    is_normal = .TRUE.
    mu = aMean(x)
    sigma = Std_Dev(x)
    If (Abs((MaxVal(x)-mu)) .GT. 3._dp*sigma) is_normal = .FALSE.
    If (Abs((MinVal(x)-mu)) .GT. 3._dp*sigma) is_normal = .FALSE.
End Function Check_normal_BOE

Subroutine Check_normal_AD(x,is_normal,AD_stat)
    !Tests for normality using the Anderson-Darling test
    Use Kinds, Only: dp
    Use Sorting, Only: Quick_Sort
    Implicit None
    Real(dp), Intent(In) :: x(:)  !sample to be checked for normal distribution
    Logical :: is_normal(1:4)  !normality flags for 10, 5, 2.5, 1 percent significance levels
    Real(dp), Intent(Out), Optional :: AD_stat
    Real(dp) :: x_bar,s  !sample mean and standard deviation
    Integer :: n,i
    Real(dp), Allocatable :: z(:)
    Real(dp) :: AD  !test statistic
    Real(dp), Parameter :: AD_crit(1:4) = (/ 0.631_dp, & !10%
                                           & 0.752_dp, & !5%
                                           & 0.873_dp, & !2.5%
                                           & 1.035_dp /) !1%

    is_normal = .TRUE.  !Null hypothesis
    !use sample mean and sample standard dev as estimates to the population statistics
    x_bar = aMean(x)
    s = Std_Dev(x)
    !convert x to z-scores and sort
    n = Size(x)
    Allocate(z(1:n))
    z = (x - x_bar) / s  !compute z-scores for each sample point
    Call Quick_Sort(z)  !sort ascending
    !Compute the test statistic
    AD = 0._dp
    Do i = 1,n
        AD = AD + Real(2*i - 1,dp)*(Log(Std_Normal_CDF(z(i))) + Log(1._dp - Std_Normal_CDF(z(n+1-i))))
    End Do
    AD = (-Real(n,dp) - (AD / Real(n,dp))) * (1._dp + 0.75_dp/Real(n,dp) + 2.25_dp/Real(n**2,dp))
    !compare to critical values
    Where (AD .GT. AD_crit) is_normal = .FALSE. !Reject null hypothesis
    If (Present(AD_stat)) AD_stat = AD
End Subroutine Check_normal_AD 

Elemental Function Std_Normal_CDF(z)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Std_Normal_CDF
    Real(dp), Intent(In) :: z
    Real(dp), Parameter :: sqrt2 = Sqrt(2._dp)

    Std_Normal_CDF = 0.5_dp * (1._dp + ERF(z / sqrt2))
End Function Std_Normal_CDF

Subroutine Check_exponential_AD(x,is_exponential,AD_stat)
    !Tests for normality using the Anderson-Darling test
    Use Kinds, Only: dp
    Use Sorting, Only: Quick_Sort
    Implicit None
    Real(dp), Intent(In) :: x(:)  !sample to be checked for normal distribution
    Logical :: is_exponential(1:4)  !normality flags for 10, 5, 2.5, 1 percent significance levels
    Real(dp), Intent(Out), Optional :: AD_stat
    Integer :: n,i
    Real(dp), Allocatable :: z(:)
    Real(dp) :: s  !estimated scale parameter of the exponential distribution
    Real(dp) :: AD  !test statistic
    Real(dp), Parameter :: AD_crit(1:4) = (/ 1.070_dp, & !10%
                                           & 1.326_dp, & !5%
                                           & 1.587_dp, & !2.5%
                                           & 1.943_dp /) !1%

    is_exponential = .TRUE.  !Null hypothesis
    ! Estimate the scale parameter of the distribution
    s = aMean(x)
    ! Create a sorted copy of the sample
    n = Size(x)
    Allocate(z(1:n))
    z = x
    Call Quick_Sort(z)
    !Compute the test statistic
    AD = 0._dp
    Do i = 1,n
        AD = AD + Real(2*i - 1,dp)*(Log(Exponential_CDF(z(i),s)) + Log(1._dp - Exponential_CDF(z(n+1-i),s)))
    End Do
    AD = (-Real(n,dp) - (AD / Real(n,dp))) * (1._dp + 0.75_dp/Real(n,dp) + 2.25_dp/Real(n**2,dp))
    !compare to critical values
    Where (AD .GT. AD_crit) is_exponential = .FALSE. !Reject null hypothesis
    If (Present(AD_stat)) AD_stat = AD
End Subroutine Check_exponential_AD 

Elemental Function Exponential_CDF(z,s)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: Exponential_CDF
    Real(dp), Intent(In) :: z
    Real(dp), Intent(In) :: s

    Exponential_CDF = 1._dp - Exp(-z*s)
End Function Exponential_CDF

Subroutine Check_uniform_AD(x,is_uniform,AD_stat)
    !Tests for normality using the Anderson-Darling test
    Use Kinds, Only: dp
    Use Sorting, Only: Quick_Sort
    Implicit None
    Real(dp), Intent(In) :: x(:)  !sample to be checked for uniform distribution
    Logical :: is_uniform(1:4)  !normality flags for 10, 5, 2.5, 1 percent significance levels
    Real(dp), Intent(Out), Optional :: AD_stat
    Integer :: n,i
    Real(dp), Allocatable :: z(:)
    Real(dp) :: AD  !test statistic
    Real(dp), Parameter :: AD_crit(1:4) = (/ 1.933_dp, & !10%
                                           & 2.492_dp, & !5%
                                           & 3.070_dp, & !2.5%
                                           & 3.853_dp /) !1%

    is_uniform = .TRUE.  !Null hypothesis
    ! Create a sorted copy of the sample
    n = Size(x)
    Allocate(z(1:n))
    z = x
    Call Quick_Sort(z)
    !Compute the test statistic
    AD = 0._dp
    Do i = 1,n
        AD = AD + Real(2*i - 1,dp)*(Log(z(i)) + Log(1._dp - z(n+1-i)))
    End Do
    AD = (-Real(n,dp) - (AD / Real(n,dp))) * (1._dp + 0.75_dp/Real(n,dp) + 2.25_dp/Real(n**2,dp))
    !compare to critical values
    Where (AD .GT. AD_crit) is_uniform = .FALSE. !Reject null hypothesis
    If (Present(AD_stat)) AD_stat = AD
End Subroutine Check_uniform_AD

Subroutine Confidence_Interval_by_bootstrap(x,a,CI_high,CI_low,RNG)
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Use Sorting, Only: Quick_Sort
    Implicit None
    Real(dp), Intent(In) :: x(:)  !sample to be bootstrapped for confdence interval
    Real(dp), Intent(In) :: a  !alpha for desired confidence
    Real(dp), Intent(Out) :: CI_high,CI_low
    Type(RNG_Type), Intent(InOut) :: RNG
    Integer :: n  !number of elements in x
    !TODO Check different numbers of bootstraps against execution time, or consider as a user input
    Integer, Parameter :: n_bootstraps = 2**20  !for now, see TODO above
    Real(dp) :: bootstrapped_means(1:n_bootstraps)
    Real(dp), Allocatable :: bootstrap(:)
    Integer :: i
    Integer :: CI_high_index,CI_low_index
    
    n = Size(x)
    Allocate(bootstrap(n))
    Do i = 1,n_bootstraps
        !create a bootstrapped sample
        bootstrap = x(Floor(Real(n,dp)*RNG%Get_Randoms(n)) + 1)
        bootstrapped_means(i) = aMean(bootstrap)
    End Do
    !Sort the computed means
    Call Quick_Sort(bootstrapped_means)
    !Select the appropriate means as bounds to theconfidence interval
    CI_high_index = Ceiling((1._dp - a/2._dp) * Real(n_bootstraps,dp))
    CI_low_index = Ceiling((a/2._dp) * Real(n_bootstraps,dp))
    CI_high = bootstrapped_means(CI_high_index)
    CI_low = bootstrapped_means(CI_low_index)
End Subroutine Confidence_Interval_by_bootstrap

Pure Function aMean(x) Result(x_bar)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x_bar
    Real(dp), Intent(In) :: x(:)
    
    If (Size(x) .GT. 0) Then
        x_bar = Sum(x) / Real(Size(x),dp)
    Else
        x_bar = 0._dp
    End If
End Function aMean

Pure Function gMean(x) Result(x_bar)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: x_bar
    Real(dp), Intent(In) :: x(:)
    
    x_bar = Exp(aMean(Log(x + 1._dp))) - 1._dp
End Function gMean

Pure Function Variance(x) Result(s_squared)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: s_squared
    Real(dp), Intent(In) :: x(:)
    Real(dp) :: x_bar
    
    x_bar = aMean(x)
    s_squared = Sum((x-x_bar)**2) / Real((Size(x)-1),dp)
End Function Variance

Pure Function Std_Dev(x) Result(s)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: s
    Real(dp), Intent(In) :: x(:)
    
    s = Sqrt(Variance(x))
End Function Std_Dev

End Module Statistics

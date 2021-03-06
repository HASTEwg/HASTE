    Include 'mkl_vsl.f90'
    
Module Random_Numbers
    
    Use Kinds, Only: dp
    Use MKL_VSL_TYPE, Only: VSL_STREAM_STATE
    Implicit None
    Private
    Public :: RNG_Type
    Public :: Setup_RNG
    
    Type :: RNG_Type
        Type(VSL_STREAM_STATE) :: stream  !stream state variable for RNG
        Real(dp), Allocatable :: q(:)  !Pre-filled array of random numbers generated by stream
        Integer :: q_size  !size of array q
        Integer :: q_index  !position of next random number to be used in q
        Integer :: seed  !value used to seed the RNG
    Contains
        Procedure, Pass :: Get_Random => Get_1_Random
        Procedure, Pass :: Get_Randoms => Get_n_Randoms
        Procedure, Pass :: Restart => Restart_RNG
        Procedure, Pass :: Initialize => Initialize_RNG
        Procedure, Pass :: Cleanup => Cleanup_RNG
    End Type
    
    !N2H Improve detail of RNG stat checking to include detailed error trapping rather than general stops in current implementation

Contains
    
Function Setup_RNG(setup_file_name,run_file_name) Result(RNG)
    Use OMP_LIB, Only: OMP_GET_NUM_THREADS,OMP_GET_THREAD_NUM
    Use FileIO_Utilities, Only: fSHARE
    Implicit None
    Type(RNG_Type) :: RNG
    Character(*), Intent(In) :: setup_file_name,run_file_name
    Logical :: RNG_seed_random
    Integer :: RNG_seed
    Integer :: t
    Integer :: setup_unit,stat
    
    NameList /RNGSetupList/ RNG_seed_random,RNG_seed
    
    !open setup file and read namelist
    Open(NEWUNIT = setup_unit , FILE = setup_file_name , STATUS = 'OLD' , ACTION = 'READ' , IOSTAT = stat, SHARE = fSHARE)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Random_Numbers: Setup_RNG:  File open error, '//setup_file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(setup_unit,NML = RNGSetupList)
    Close(setup_unit)
    If (RNG_seed_random) Then
        RNG_seed = Create_Random_Seed()
        RNG_seed_random = .TRUE.
    End If
    If (OMP_GET_NUM_THREADS().GT.1 .OR. num_images().GT.1) Then !setup is being called from inside a parallel region
        If (OMP_GET_NUM_THREADS() .GT. 1) Then  !use the OpenMP thread number to choose the RNG stream
            t = OMP_GET_THREAD_NUM()  !OpenMP threads are numbered starting at zero
        Else If (num_images() .GT. 1) Then  !use the coarray image number to choose the RNG stream
            t = this_image() - 1  !coarray images are numbered starting at 1
        Else
            Print *,'ERROR:  Random_Numbers: Setup_RNG:  Unable to resolve thread or image number.'
            ERROR STOP
        End If
        Call RNG%Initialize(seed = RNG_seed , thread = t)
    Else  !initialize for serial execution
        Call RNG%Initialize(seed = RNG_seed)
    End If
    If (this_image() .EQ. 1) Then
        Open(NEWUNIT = setup_unit , FILE = run_file_name , STATUS = 'OLD' , ACTION = 'WRITE' , POSITION = 'APPEND' , IOSTAT = stat)
        If (stat .NE. 0) Then
            Print *,'ERROR:  Random_Numbers: Setup_RNG:  File open error, '//run_file_name//', IOSTAT=',stat
            ERROR STOP
        End If
        Write(setup_unit,NML = RNGSetupList)
        Write(setup_unit,*)
        Close(setup_unit)
    End If
End Function Setup_RNG

Subroutine Initialize_RNG(RNG,seed,thread,size)  !Initializes a RNG and returns state variables for that stream
    Use MKL_VSL, Only: vslnewstream,VSL_BRNG_MT2203,VSL_BRNG_SFMT19937
    Use MKL_VSL, Only: VSL_ERROR_OK,VSL_STATUS_OK
    Use MKL_VSL, Only: vdrnguniform,VSL_RNG_METHOD_UNIFORM_STD
    Use Kinds, Only: dp
    Implicit None
    Class(RNG_Type),Intent(Out) :: RNG
    Integer, Intent(In), Optional :: seed  !RNG seed to initialize RNG stream, omission indicates a random seed
    Integer, Intent(In), Optional :: thread  !index of calling thread, omission indicates serial execution
    Integer, Intent(In), Optional :: size  !size of random number array to pre-fill, default 2**12
    Integer :: rng_stat  !status returns from MKL RNG operations
    
    If (Present(size)) Then
        RNG%q_size = size
    Else
        RNG%q_size = 2**12
    End If
    Allocate(RNG%q(1:RNG%q_size))
    If (Present(seed)) Then
        RNG%seed = seed
    Else  !Generate a random seed using intrinsic RNG seeded with default seed (date-and-time based)
        RNG%seed = Create_Random_Seed()  !sets seed to a value between [0,Huge) based on current date and time
    End If
    If (Present(thread)) Then  !Parallel execution, use an independent MT2203 stream for each thread
        rng_stat = vslnewstream(RNG%stream,VSL_BRNG_MT2203+thread,RNG%seed)
    Else  !serial execution, use SIMD-oriented fast MT19937
        !rng_stat = vslnewstream(RNG%stream,VSL_BRNG_MT2203+1,RNG%seed)
        rng_stat = vslnewstream(RNG%stream,VSL_BRNG_SFMT19937,RNG%seed)
    End If
    If (Not(rng_stat.EQ.VSL_ERROR_OK .OR. rng_stat.EQ.VSL_STATUS_OK)) Then
        Print *,'ERROR:  Random_Numbers: Initialize_RNG:  MKL RNG stream creation failed, STAT = ',rng_stat
        ERROR STOP
    End If
    rng_stat = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD,RNG%stream,RNG%q_size,RNG%q,0._dp,1._dp)
    If (Not(rng_stat.EQ.VSL_ERROR_OK .OR. rng_stat.EQ.VSL_STATUS_OK)) Then
        Print *,'ERROR:  Random_Numbers: Initialize_RNG:  MKL RNG stream initial fill q failed, STAT = ',rng_stat
        ERROR STOP
    End If
    RNG%q_index = 1
End Subroutine Initialize_RNG

Function Create_Random_Seed() Result(s)
    Use Kinds, Only: dp
    Implicit None
    Integer :: s
    Real(dp) :: r  !holder for random real used to generate random seed
    
    Call RANDOM_SEED()
    Call RANDOM_NUMBER(r)
    s = Floor(r*Real(Huge(s),dp))  !sets seed to a value between [0,Huge) based on current date and time
End Function Create_Random_Seed

Function Get_1_Random(RNG) Result(r)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: r
    Class(RNG_Type), Intent(InOut) :: RNG  !RNG stream state variable from which to get r
    
    r = RNG%q(RNG%q_index)  !get random number from the array
    If (RNG%q_index .EQ. RNG%q_size) Then !random array exhaused, need to refresh q
        Call Refresh_Random_Array(RNG)
    Else  !increment index
        RNG%q_index = RNG%q_index + 1
    End If
End Function Get_1_Random

Function Get_n_Randoms(RNG,n) Result(r)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: r(n)
    Class(RNG_Type), Intent(InOut) :: RNG
    Integer,Intent(In) :: n  !number of elements in r
    Integer :: m
    
    m = RNG%q_index + n - 1  !index of the final q needed to fill r
    If (m .LE. RNG%q_size) Then  !There are enough numbers left in th array for the request
        r = RNG%q(RNG%q_index:m)
        RNG%q_index = RNG%q_index + n
    Else  !There are not enough numbers in the array for the request
        m = RNG%q_size - RNG%q_index + 1  !q remaining in random array
        r(1:m) = RNG%q(RNG%q_index:RNG%q_size)  !exhaust the random array into the first m entries of r
        Call Refresh_Random_Array(RNG)  !Refresh the random array
        r(m+1:n) = RNG%q(1:n-m)  !fill the remaining n-m entries of r
        RNG%q_index = n - m + 1
    End If
    If (RNG%q_index .GE. RNG%q_size) Then !random array exhaused, need to refresh q
        Call Refresh_Random_Array(RNG)
    End If
End Function Get_n_Randoms

Subroutine Refresh_Random_Array(RNG)
    Use MKL_VSL, Only: VSL_ERROR_OK,VSL_STATUS_OK
    Use MKL_VSL, Only: vdrnguniform,VSL_RNG_METHOD_UNIFORM_STD
    Use Kinds, Only: dp
    Implicit None
    Type(RNG_Type), Intent(InOut) :: RNG
    Integer :: rng_stat  !status returns from MKL RNG operations
    
    rng_stat = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD,RNG%stream,RNG%q_size,RNG%q,0._dp,1._dp)
    If (Not(rng_stat.EQ.VSL_ERROR_OK .OR. rng_stat.EQ.VSL_STATUS_OK)) Then
        Print *,'ERROR:  Random_Numbers: Refresh_Random_Array:  MKL RNG q_fill failed, STAT = ',rng_stat
        ERROR STOP
    End If
    RNG%q_index = 1
End Subroutine Refresh_Random_Array

Subroutine Restart_RNG(RNG)
    Use OMP_LIB, Only: OMP_GET_NUM_THREADS,OMP_GET_THREAD_NUM
    Use MKL_VSL, Only: vsldeletestream,vslnewstream
    Use MKL_VSL, Only: VSL_BRNG_MT2203,VSL_BRNG_SFMT19937
    Use MKL_VSL, Only: VSL_ERROR_OK,VSL_STATUS_OK
    Use MKL_VSL, Only: vdrnguniform,VSL_RNG_METHOD_UNIFORM_STD
    Implicit None
    Class(RNG_Type), Intent(InOut) :: RNG
    Integer :: rng_stat  !status returns from MKL RNG operations
    Integer :: thread
    
    rng_stat = vsldeletestream(RNG%stream)
    If (OMP_GET_NUM_THREADS().GT.1 .OR. num_images().GT.1) Then !setup is being called from inside a parallel region
        If (OMP_GET_NUM_THREADS() .GT. 1) Then  !use the OpenMP thread number to choose the RNG stream
            thread = OMP_GET_THREAD_NUM()  !OpenMP threads are numbered starting at zero
        Else If (num_images() .GT. 1) Then  !use the coarray image number to choose the RNG stream
            thread = this_image() - 1  !coarray images are numbered starting at 1
        Else
            Print *,'ERROR:  Random_Numbers: Restart_RNG:  Unable to resolve thread or image number.'
            ERROR STOP
        End If
        rng_stat = vslnewstream(RNG%stream,VSL_BRNG_MT2203+thread,RNG%seed)
    Else  !initialize for serial execution
        rng_stat = vslnewstream(RNG%stream,VSL_BRNG_SFMT19937,RNG%seed)
    End If
    If (Not(rng_stat.EQ.VSL_ERROR_OK .OR. rng_stat.EQ.VSL_STATUS_OK)) Then
        Print *,'ERROR:  Random_Numbers: Restart_RNG:  MKL RNG stream creation failed, STAT = ',rng_stat
        ERROR STOP
    End If
    rng_stat = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD,RNG%stream,RNG%q_size,RNG%q,0._dp,1._dp)
    If (Not(rng_stat.EQ.VSL_ERROR_OK .OR. rng_stat.EQ.VSL_STATUS_OK)) Then
        Print *,'ERROR:  Random_Numbers: Restart_RNG:  MKL RNG stream initial fill q failed, STAT = ',rng_stat
        ERROR STOP
    End If
    RNG%q_index = 1
End Subroutine Restart_RNG

Subroutine Cleanup_RNG(RNG)
    Use MKL_VSL, Only: vsldeletestream
    Implicit None
    Class(RNG_Type), Intent(InOut) :: RNG
    Integer :: rng_stat  !status returns from MKL RNG operations
    
    rng_stat = vsldeletestream(RNG%stream)
    Deallocate(RNG%q)
    RNG%q_size = 0
    RNG%q_index = 0
    RNG%seed = 0
End Subroutine Cleanup_RNG

End Module Random_Numbers
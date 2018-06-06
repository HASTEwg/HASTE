Module Setups

    Implicit None
    Private
    Public :: Setup_HATS
    Public :: Paths_Files_Type
    Public :: Write_Setup_Information
    Public :: Create_Output_File_names
    !DIR$ IF DEFINED (COA)
        Public :: Setup_Info_to_disk
        Public :: Setup_Info_from_disk
        Public :: Cleanup_temp_files
    !DIR$ END IF
    
    Type :: Paths_Files_Type
        Character(:), Allocatable :: app_title  !name and version of program
        Character(:), Allocatable :: program_exe  !path and name of the running executable
        Character(:), Allocatable :: setup_file  !specifies setup file, default is 'HATS_Setup.txt' in the same directory as the executable
        Character(:), Allocatable :: log_file_name  !specifies log file, default is 'HATS_log.txt' in the default results directory
        Character(:), Allocatable :: run_file_name  !specifies run file, default is 'HATS-setup.txt' in the default directory
        Character(:), Allocatable :: resources_directory    !specifies resources directory, default assumes resources folder is in same directory as setup file
        Character(:), Allocatable :: cs_setup_file  !specifies setup file for cross sections, this file sets atmospheric constituents and what cross section data is available
        Character(:), Allocatable :: results_directory  !specifies results directory, default assumes results folder is in same directory as setup file
        Character(:), Allocatable :: file_suffix  !specifies suffix to be appended to file names
        Character(:), Allocatable :: output_folder  !specifies folder in results folder, this variable is an interim holder, thes value is appended to results path by HATS Setup
        Character(:), Allocatable :: TE_file_name
        Character(:), Allocatable :: t_file_name
        Character(:), Allocatable :: E_file_name
        Character(:), Allocatable :: f_file_name
        Character(:), Allocatable :: d_file_name
        Character(:), Allocatable :: m_file_name
        Character(:), Allocatable :: o_file_name
        Character(:), Allocatable :: s_file_name
    Contains
        Procedure, Pass :: Initialize => Initialize_Paths_Files
    End Type
    
Contains

Subroutine Setup_HATS(prompt_for_exit,screen_progress,paths_files,n_neutron_histories,absolute_n_histories,setup_file)
    !Reads in problem data, method data, physics data
    !Initializes processes and variables
    Use IFPORT, Only: $MAXPATH
    Use IFPORT, Only: GETDRIVEDIRQQ
    Use IFPORT, Only: FILE$CURDRIVE
    Use Kinds, Only: dp
    Use FileIO_Utilities, Only: slash
    Use FileIO_Utilities, Only: Get_Working_Directory
    Implicit None
    Logical, Intent(Out) :: prompt_for_exit
    Logical, Intent(Out) :: screen_progress
    Type(paths_files_type), Intent(InOut) :: paths_files
    Integer(8), Intent(Out) :: n_neutron_histories
    Logical, Intent(Out) :: absolute_n_histories
    Character(*), Intent(In), Optional :: setup_file  !if specified, overrides default setup file name and setup file specified by command line
    Character(:), Allocatable :: file_suffix  !specifies suffix to be appended to file names
    Character(:), Allocatable :: output_folder  !specifies folder in results folder, this variable is an interim holder, thes value is appended to results path by HATS Setup
    Logical :: force_overwrite
    Integer :: setup_unit,stat,num_cmd_args
    Integer :: pathlen, status
    Character($MAXPATH) :: path
    Logical :: interim_outputs
    Character(4) :: interim_unit
    Integer :: interim_period
    
    NameList /ProgramSetupList/   prompt_for_exit,screen_progress, &
                                & output_folder,file_suffix,force_overwrite, &
                                & interim_outputs,interim_unit,interim_period
    
    Call paths_files%Initialize()
    Allocate(Character($MAXPATH) :: output_folder)
    Allocate(Character($MAXPATH) :: file_suffix)
    !Get path and name of executable
    Call GET_COMMAND_ARGUMENT(0, path, pathlen, status)
    If (status .NE. 0) Then
        Print *,'ERROR:  Setups: Setup_HATS:  Read command line argument 0 failed'
        ERROR STOP
    End If
    paths_files%program_exe = Trim(path)
    !default values for files and directories
    Call Get_Working_Directory(path,slash)
    paths_files%setup_file = Trim(path)//'HATS_Setup.txt'
    paths_files%resources_directory = Trim(path)//'Resources'//slash
    paths_files%results_directory = Trim(path)//'Results'//slash
    If (Present(setup_file)) paths_files%setup_file = setup_file
    !open setup file and read namelist
    Open(NEWUNIT = setup_unit , FILE = paths_files%setup_file , STATUS = 'OLD' , ACTION = 'READ' , IOSTAT = stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Setups: Setup_HATS:  File open error, '//paths_files%setup_file//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(setup_unit,NML = ProgramSetupList)
    Close(setup_unit)
    !DIR$ IF DEFINED (MIC)
        prompt_for_exit = .FALSE.
    !DIR$ END IF
    !Trim output folder and file suffix character strings
    output_folder = Trim(output_folder)
    file_suffix = Trim(file_suffix)
    If (Len(Trim(output_folder)) .GT. 0) Then
        paths_files%output_folder = Trim(output_folder)//slash
    Else
        paths_files%output_folder = Trim(output_folder)
    End If
    paths_files%file_suffix = Trim(file_suffix)
    !Check if files exist, if they do overwrite or prompt for revised file/folders from user
    Call Check_files_exist(force_overwrite,num_cmd_args,paths_files)
    !Check for correct directories, create them if they don't exist
    Call Check_folders_exist(paths_files)
    !append output folder to results directory
    paths_files%results_directory = Trim(paths_files%results_directory//paths_files%output_folder)
    !Construct file names
    paths_files%cs_setup_file = ''
    paths_files%log_file_name = ''
    paths_files%run_file_name = ''
    paths_files%TE_file_name = ''
    paths_files%t_file_name = ''
    paths_files%E_file_name = ''
    paths_files%f_file_name = ''
    paths_files%d_file_name = ''
    paths_files%m_file_name = ''
    paths_files%o_file_name = ''
    paths_files%s_file_name = ''
    Call Create_Output_File_names(paths_files%results_directory,paths_files%file_suffix,paths_files%log_file_name,paths_files%TE_file_name,paths_files%t_file_name,paths_files%E_file_name,paths_files%f_file_name,paths_files%d_file_name,paths_files%m_file_name,paths_files%o_file_name,paths_files%s_file_name,paths_files%run_file_name)
    !Create backup setup file in results folder and write namelist
    If (this_image() .EQ. 1) Then
        !the backup setup file will not contain any continuation or study set configuration information
        Open(NEWUNIT = setup_unit , FILE = paths_files%run_file_name , STATUS = 'REPLACE' , ACTION = 'WRITE' , POSITION = 'APPEND' , IOSTAT = stat)
        If (stat .NE. 0) Then
            Print *,'ERROR:  Setups: Setup_HATS:  File open error, '//paths_files%run_file_name//', IOSTAT=',stat
            ERROR STOP
        End If
        Write(setup_unit,NML = ProgramSetupList)
        Write(setup_unit,*)
        Close(setup_unit)
    End If
    !Read in the next setup namelist
    Call Setup_Estimator(paths_files%setup_file,paths_files%run_file_name,n_neutron_histories,absolute_n_histories)
End Subroutine Setup_HATS

Subroutine Create_Output_File_names(dir,suff,log_name,TE_name,T_name,E_name,F_name,D_name,M_name,O_name,S_name,run_name)
    Implicit None
    Character(*), Intent(In) :: dir,suff
    Character(:), Allocatable, Intent(InOut) :: log_name,TE_name,T_name,E_name,F_name,D_name,M_name,O_name,S_name
    Character(:), Allocatable, Intent(InOut), Optional :: run_name
    
    !Construct file names
    !Construct log file name
    log_name = dir//'HATS-Log'//suff//'.txt'
    If (Present(run_name)) run_name = dir//'HATS-setup'//suff//'.txt'
    !Construct neutron output file names
    TE_name = dir//'TE_fluence'//suff//'.txt'
    T_name = dir//'T_fluence'//suff//'.txt'
    E_name = dir//'E_fluence'//suff//'.txt'
    F_name = dir//'tot_fluence'//suff//'.txt'
    D_name = dir//'ArrDirs'//suff//'.txt'
    M_name = dir//'ArrDirs_mu'//suff//'.txt'
    O_name = dir//'ArrDirs_omega'//suff//'.txt'
    S_name = dir//'SliceShape'//suff
End Subroutine Create_Output_File_names

Subroutine Check_files_exist(overwrite,n_cmd_args,paths_files)
    Use IFPORT, Only: $MAXPATH
    Use FileIO_Utilities, Only: slash
    Implicit None
    Logical, Intent(In) :: overwrite
    Integer, Intent(In) :: n_cmd_args
    Type(paths_files_type), Intent(InOut) :: paths_files
    Integer, Parameter :: n_names = 8
    Logical :: file_exists(1:n_names)
    Character($MAXPATH) :: file_name(1:n_names)
    Character(64) :: user_inp
    Integer :: i

    Do_CheckFiles: Do
        !construct file names to check if they already exist
        file_name(1) = paths_files%results_directory//paths_files%output_folder//'TE_Fluence'//paths_files%file_suffix//'.txt'
        file_name(2) = paths_files%results_directory//paths_files%output_folder//'E_Fluence'//paths_files%file_suffix//'.txt'
        file_name(3) = paths_files%results_directory//paths_files%output_folder//'t_Fluence'//paths_files%file_suffix//'.txt'
        file_name(4) = paths_files%results_directory//paths_files%output_folder//'tot_Fluence'//paths_files%file_suffix//'.txt'
        file_name(5) = paths_files%results_directory//paths_files%output_folder//'ArrDirs'//paths_files%file_suffix//'.txt'
        file_name(6) = paths_files%results_directory//paths_files%output_folder//'ArrDirs_mu'//paths_files%file_suffix//'.txt'
        file_name(7) = paths_files%results_directory//paths_files%output_folder//'ArrDirs_omega'//paths_files%file_suffix//'.txt'
        file_name(8) = paths_files%results_directory//paths_files%output_folder//'HATS-Log'//paths_files%file_suffix//'.txt'
        Do i = 1,n_names
            INQUIRE(FILE = file_name(i) , EXIST = file_exists(i))
        End Do
        If (Any(file_exists)) Then
            Print *,'WARNING:  One or more files already exist.'
            Print *,'    Output Folder: '//paths_files%results_directory//paths_files%output_folder
            Print *,'    File suffix:   '//paths_files%file_suffix
            If (overwrite) Then !file overwrite is forced by input settings
                Print *,'WARNING:  Automatic overwrite selected by setup inputs.'
                Exit Do_CheckFiles
            Else If (n_cmd_args .GT. 0) Then  !command line specified path cannot be changed, prompt for overwrite or abort
                Print *,'WARNING:  Command line specified results files already exist.'
                Do_cmd_line_overwrite: Do
                    Print *,'Press:  O to overwrite'
                    Print *,'        A to abort the simulation'
                    Read (*,*) user_inp
                    Select Case (user_inp)
                        Case ('O','o')
                            Exit Do_CheckFiles
                        Case ('A','a')
                            Print *,'  Confirm:  Abort simulation? (Y/N)'
                            Read (*,*) user_inp
                            Select Case (user_inp)
                                Case ('Y','y')
                                    Print *
                                    Print *,'Simulation aborted by user.'
                                    ERROR STOP
                            End Select
                        Case Default
                            Print *,'Invalid response.'
                    End Select
                End Do Do_cmd_line_overwrite
            End If
            Do_GetInput: Do
                Print *,'Press:  O to overwrite'
                Print *,'        D to enter a new directory for the new files'
                Print *,'        S to enter a different file suffix for the new files'
                Print *,'        A to abort the simulation'
                Read (*,*) user_inp
                Select Case (user_inp)
                    Case ('O','o')
                        Exit Do_CheckFiles
                    Case ('D','d')
                        Print *,'  Enter new directory (omit ''Results'//slash//''', omit trailing '''//slash//'''):'
                        Read (*,*) user_inp
                        paths_files%output_folder = Trim(user_inp)
                        Exit Do_GetInput
                    Case ('S','s')
                        Print *,'  Enter new file suffix (recommend 1st character delimiter (''_'','' '',''-'',etc):'
                        Read (*,*) user_inp
                        paths_files%file_suffix = Trim(user_inp)
                        Exit Do_GetInput
                    Case ('A','a')
                        Print *,'  Confirm:  Abort simulation? (Y/N)'
                        Read (*,*) user_inp
                        Select Case (user_inp)
                            Case ('Y','y')
                                Print *
                                Print *,'Simulation aborted by user.'
                                ERROR STOP
                        End Select
                    Case Default
                        Print *,'Invalid response.'
                End Select
            End Do Do_GetInput
        Else
            Exit Do_CheckFiles
        End If
    End Do Do_CheckFiles
End Subroutine Check_files_exist

Subroutine Check_folders_exist(paths_files)
    Use IFPORT, Only: MAKEDIRQQ
    Implicit None
    Type(paths_files_type), Intent(In) :: paths_files
    Logical :: folder_exists
    Logical :: success
    
    !Check if resources directories exist
    INQUIRE(DIRECTORY = paths_files%resources_directory , EXIST = folder_exists)
    If (.NOT. folder_exists) Then
        Print *,'ERROR:  Setups: Setup_HATS:  Resources directory not found: '//paths_files%resources_directory
        ERROR STOP
    End If
    !Check if results directories exist
    INQUIRE(DIRECTORY = paths_files%results_directory , EXIST = folder_exists)
    If (.NOT. folder_exists) Then  !create results folder
        success = MAKEDIRQQ(paths_files%results_directory)
        If (.NOT. success) Then
            Print *,'ERROR:  Setups: Setup_HATS:  Failed to create directory: '//paths_files%results_directory
            ERROR STOP
        End If
    End If
    If (paths_files%output_folder .NE. '')  Then !output folder is specified
        INQUIRE(DIRECTORY = paths_files%results_directory//paths_files%output_folder , EXIST = folder_exists)
        If (.NOT. folder_exists) Then  !create output folder
            success = MAKEDIRQQ(paths_files%results_directory//paths_files%output_folder)
            If (.NOT. success) Then
                Print *,'ERROR:  Setups: Setup_HATS:  Failed to create directory: '//paths_files%results_directory//paths_files%output_folder
                ERROR STOP
            End If
        End If
    End if
End Subroutine Check_folders_exist

Subroutine Setup_Estimator(setup_file_name,run_file_name,n_neutron_histories,absolute_n_histories)
    Implicit None
    Character(*), Intent(In) :: setup_file_name,run_file_name
    Integer(8), Intent(Out) :: n_neutron_histories
    Logical, Intent(Out) :: absolute_n_histories
    Integer :: setup_unit,stat
    
    NameList /EstimatorSetupList/ n_neutron_histories,absolute_n_histories
    
    !open setup file and read namelist
    Open(NEWUNIT = setup_unit , FILE = setup_file_name , STATUS = 'OLD' , ACTION = 'READ' , IOSTAT = stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Setups: Setup_Estimator:  File open error, '//setup_file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(setup_unit,NML = EstimatorSetupList)
    Close(setup_unit)
    Open(NEWUNIT = setup_unit , FILE = run_file_name , STATUS = 'OLD' , ACTION = 'WRITE' , POSITION = 'APPEND' , IOSTAT = stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Setups: Setup_Estimator:  File open error, '//run_file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(setup_unit,NML = EstimatorSetupList)
    Write(setup_unit,*)
    Close(setup_unit)
End Subroutine Setup_Estimator

!DIR$ IF DEFINED (COA)
Subroutine Setup_Info_to_disk(n_histories,abs_n_histories,prompt_for_exit,screen_progress,paths_files)
    Use IFPORT, Only: $MAXPATH
    Use IFPORT, Only: MAKEDIRQQ
    Use Kinds, Only: dp
    Use FileIO_Utilities, Only: slash
    Use FileIO_Utilities, Only: Get_Working_Directory
    Use FileIO_Utilities, Only: Var_to_File
    Implicit None
    Integer(8), Intent(In) :: n_histories
    Logical, Intent(In) :: abs_n_histories
    Logical, Intent(In) :: prompt_for_exit
    Logical, Intent(In) :: screen_progress
    Type(Paths_Files_Type), Intent(In) :: paths_files
    Logical :: folder_exists,success
    Character($MAXPATH) :: dir
    Character(:), Allocatable :: file_name,file_dir

    Call Get_Working_Directory(dir,slash)
    Allocate(Character($MAXPATH) :: file_dir)
    file_dir = Trim(dir)//'temp'//slash
    !Check if temp results directory exists
    INQUIRE(DIRECTORY = file_dir , EXIST = folder_exists)
    If (.NOT. folder_exists) Then  !create results folder
        success = MAKEDIRQQ(file_dir)
        If (.NOT. success) Then
            Print *,'ERROR:  Setups: Setup_Info_to_disk:  Failed to create directory: '//file_dir
            ERROR STOP
        End If
    End If
    Allocate(Character($MAXPATH) :: file_name)
    !write n_histories to file
    file_name = file_dir//'n.tmp'
    Call Var_to_File(n_histories,file_name)
    !write absolute_n_histories to file
    file_name = file_dir//'abs_n.tmp'
    Call Var_to_File(abs_n_histories,file_name)
    !write prompt_for_exit to file
    file_name = file_dir//'eb.tmp'
    Call Var_to_File(prompt_for_exit,file_name)
    !write screen_progress to file
    file_name = file_dir//'sp.tmp'
    Call Var_to_File(screen_progress,file_name)
    !write each element of paths_files to file
    file_name = file_dir//'pf_app_t.tmp'
    Call Var_to_File(paths_files%app_title,file_name)
    file_name = file_dir//'pf_prog_exe.tmp'
    Call Var_to_File(paths_files%program_exe,file_name)
    file_name = file_dir//'pf_set_f.tmp'
    Call Var_to_File(paths_files%setup_file,file_name)
    file_name = file_dir//'pf_log_f.tmp'
    Call Var_to_File(paths_files%log_file_name,file_name)
    file_name = file_dir//'pf_run_f.tmp'
    Call Var_to_File(paths_files%run_file_name,file_name)
    file_name = file_dir//'pf_resource_d.tmp'
    Call Var_to_File(paths_files%resources_directory,file_name)
    file_name = file_dir//'pf_cs_f.tmp'
    Call Var_to_File(paths_files%cs_setup_file,file_name)
    file_name = file_dir//'pf_result_d.tmp'
    Call Var_to_File(paths_files%results_directory,file_name)
    file_name = file_dir//'pf_f_suff.tmp'
    Call Var_to_File(paths_files%file_suffix,file_name)
    file_name = file_dir//'pf_out_fold.tmp'
    Call Var_to_File(paths_files%output_folder,file_name)
    file_name = file_dir//'pf_TE_f.tmp'
    Call Var_to_File(paths_files%TE_file_name,file_name)
    file_name = file_dir//'pf_t_f.tmp'
    Call Var_to_File(paths_files%t_file_name,file_name)
    file_name = file_dir//'pf_E_f.tmp'
    Call Var_to_File(paths_files%E_file_name,file_name)
    file_name = file_dir//'pf_f_f.tmp'
    Call Var_to_File(paths_files%f_file_name,file_name)
    file_name = file_dir//'pf_d_f.tmp'
    Call Var_to_File(paths_files%d_file_name,file_name)
    file_name = file_dir//'pf_m_f.tmp'
    Call Var_to_File(paths_files%m_file_name,file_name)
    file_name = file_dir//'pf_o_f.tmp'
    Call Var_to_File(paths_files%o_file_name,file_name)
    file_name = file_dir//'pf_s_f.tmp'
    Call Var_to_File(paths_files%s_file_name,file_name)
End Subroutine Setup_Info_to_disk
!DIR$ END IF

!DIR$ IF DEFINED (COA)
Subroutine Setup_Info_from_disk(n_histories,abs_n_histories,prompt_for_exit,screen_progress,paths_files)
    Use IFPORT, Only: $MAXPATH
    Use Kinds, Only: dp
    Use FileIO_Utilities, Only: slash
    Use FileIO_Utilities, Only: Get_Working_Directory
    Use FileIO_Utilities, Only: Var_from_File
    Implicit None
    Integer(8), Intent(Out) :: n_histories
    Logical, Intent(Out) :: abs_n_histories
    Logical, Intent(Out) :: prompt_for_exit
    Logical, Intent(Out) :: screen_progress
    Type(Paths_Files_Type), Intent(InOut) :: paths_files
    Character($MAXPATH) :: dir
    Character(:), Allocatable :: file_name,file_dir
    Character($MAXPATH) :: C_tmp
    
    Call Get_Working_Directory(dir)
    Allocate(Character($MAXPATH) :: file_dir)
    file_dir = Trim(dir)//slash//'temp'//slash
    Allocate(Character($MAXPATH) :: file_name)
    !read n_histories from file
    file_name = file_dir//'n.tmp'
    Call Var_from_File(n_histories,file_name)
    !read abs_n_histories from file
    file_name = file_dir//'abs_n.tmp'
    Call Var_from_File(abs_n_histories,file_name)
    !read prompt_for_exit from file
    file_name = file_dir//'eb.tmp'
    Call Var_from_File(prompt_for_exit,file_name)
    !read screen_progress from file
    file_name = file_dir//'sp.tmp'
    Call Var_from_File(screen_progress,file_name)
    !initialize paths_files character strings
    Call paths_files%Initialize()
    !read each element of paths_files from file
    file_name = file_dir//'pf_app_t.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%app_title = Trim(C_tmp)
    file_name = file_dir//'pf_prog_exe.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%program_exe = Trim(C_tmp)
    file_name = file_dir//'pf_set_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%setup_file = Trim(C_tmp)
    file_name = file_dir//'pf_log_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%log_file_name = Trim(C_tmp)
    file_name = file_dir//'pf_run_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%run_file_name = Trim(C_tmp)
    file_name = file_dir//'pf_resource_d.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%resources_directory = Trim(C_tmp)
    file_name = file_dir//'pf_cs_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%cs_setup_file = Trim(C_tmp)
    file_name = file_dir//'pf_result_d.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%results_directory = Trim(C_tmp)
    file_name = file_dir//'pf_f_suff.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%file_suffix = Trim(C_tmp)
    file_name = file_dir//'pf_out_fold.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%output_folder = Trim(C_tmp)
    file_name = file_dir//'pf_TE_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%TE_file_name = Trim(C_tmp)
    file_name = file_dir//'pf_t_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%t_file_name = Trim(C_tmp)
    file_name = file_dir//'pf_E_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%E_file_name = Trim(C_tmp)
    file_name = file_dir//'pf_f_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%f_file_name = Trim(C_tmp)
    file_name = file_dir//'pf_d_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%d_file_name = Trim(C_tmp)
    file_name = file_dir//'pf_m_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%m_file_name = Trim(C_tmp)
    file_name = file_dir//'pf_o_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%o_file_name = Trim(C_tmp)
    file_name = file_dir//'pf_s_f.tmp'
    Call Var_from_File(C_tmp,file_name)
    paths_files%s_file_name = Trim(C_tmp)
End Subroutine Setup_Info_from_disk
!DIR$ END IF

!DIR$ IF DEFINED (COA)
Subroutine Cleanup_temp_files()
    Use IFPORT, Only: $MAXPATH
    Use IFPORT, Only: DELFILESQQ
    Use IFPORT, Only: DELDIRQQ
    Use FileIO_Utilities, Only: slash
    Use FileIO_Utilities, Only: Get_Working_Directory
    Implicit None
    Integer :: n_del
    Logical :: success
    Character($MAXPATH) :: dir
    
    Call Get_Working_Directory(dir,slash)
    !delete all files in the directory
    n_del = DELFILESQQ(Trim(dir)//'temp'//slash//'*')
    !delete the temp directory
    success = DELDIRQQ(Trim(dir)//'temp'//slash)
End Subroutine Cleanup_temp_files
!DIR$ END IF

Subroutine Initialize_Paths_Files(paths_files)
    Use IFPORT, Only: $MAXPATH
    Implicit None
    Class(Paths_Files_Type), Intent(InOut) :: paths_files
    Character(9), Parameter :: empty_string = '<<EMPTY>>'

    !allocate character variables with an arbitrary length, each assignment statement then auto-reallocates them to the correct length
    Allocate(Character($MAXPATH) :: paths_files%app_title)
    paths_files%app_title = empty_string
    Allocate(Character($MAXPATH) :: paths_files%program_exe)
    paths_files%program_exe = empty_string
    Allocate(Character($MAXPATH) :: paths_files%setup_file)
    paths_files%setup_file = empty_string
    Allocate(Character($MAXPATH) :: paths_files%resources_directory)
    paths_files%resources_directory = empty_string
    Allocate(Character($MAXPATH) :: paths_files%results_directory)
    paths_files%results_directory = empty_string
    Allocate(Character($MAXPATH) :: paths_files%cs_setup_file)
    paths_files%cs_setup_file = empty_string
    Allocate(Character($MAXPATH) :: paths_files%output_folder)
    paths_files%output_folder = empty_string
    Allocate(Character($MAXPATH) :: paths_files%file_suffix)
    paths_files%file_suffix = empty_string
    Allocate(Character($MAXPATH) :: paths_files%log_file_name)
    paths_files%log_file_name = empty_string
    Allocate(Character($MAXPATH) :: paths_files%run_file_name)
    paths_files%run_file_name = empty_string
    Allocate(Character($MAXPATH) :: paths_files%TE_file_name)
    paths_files%TE_file_name = empty_string
    Allocate(Character($MAXPATH) :: paths_files%t_file_name)
    paths_files%t_file_name = empty_string
    Allocate(Character($MAXPATH) :: paths_files%E_file_name)
    paths_files%E_file_name = empty_string
    Allocate(Character($MAXPATH) :: paths_files%f_file_name)
    paths_files%f_file_name = empty_string
    Allocate(Character($MAXPATH) :: paths_files%d_file_name)
    paths_files%d_file_name = empty_string
    Allocate(Character($MAXPATH) :: paths_files%m_file_name)
    paths_files%m_file_name = empty_string
    Allocate(Character($MAXPATH) :: paths_files%o_file_name)
    paths_files%o_file_name = empty_string
    Allocate(Character($MAXPATH) :: paths_files%s_file_name)
    paths_files%s_file_name = empty_string
End Subroutine Initialize_Paths_Files

Subroutine Write_Setup_Information(n_img,t_process,t_elapsed_min,t_elapsed_max,n_h_hit,n_h_run,RNG,paths_files,file_name)
    Use IFPORT, Only: FDATE
    Use IFPORT, Only: HOSTNAM,MAX_HOSTNAM_LENGTH
    Use OMP_LIB, Only:  OMP_get_num_threads
    Use Kinds, Only: dp
    Use Random_Numbers, Only: RNG_Type
    Implicit None
    Integer, Intent(In) :: n_img
    Real(dp), Intent(In) :: t_process,t_elapsed_min,t_elapsed_max
    Integer(8), Intent(In) :: n_h_hit(1:n_img)
    Integer(8), Intent(In) :: n_h_run(1:n_img)
    Type(RNG_Type), Intent(In) :: RNG
    Type(Paths_Files_Type), Intent(In) :: paths_files
    Character(*), Intent(In) :: file_name
    Character(MAX_HOSTNAM_LENGTH+1) :: hostname
    Integer :: unit,stat
    Integer :: i
    
    Open(NEWUNIT = unit , FILE = file_name , STATUS = 'UNKNOWN' , ACTION = 'WRITE' , POSITION = 'APPEND' , IOSTAT = stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Setups: Write_Setup_Information:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit,'(A)') '------------------------------------------------------------------------'
    Write(unit,'(A)') paths_files%app_title
    Write(unit,'(A)') '------------------------------------------------------------------------'
    Write(unit,'(2A)') '  Run Complete: ',FDATE()
    Write(unit,'(A,f11.3,A)') '  Total Compute Time: ',t_process,' sec'
    Write(unit,'(A,f11.3,A)') '  Min Compute Time:   ',t_elapsed_min,' sec'
    Write(unit,'(A,f11.3,A)') '  Max Compute Time:   ',t_elapsed_max,' sec'
    Write(unit,'(A,f7.2,A)') '  Spin Fraction (approximate): ',100._dp * (t_process - Real(n_img,dp)*t_elapsed_min) / t_process,'%'
    stat = HOSTNAM(hostname)
    If (stat .NE. 0) hostname = 'UNKMOWN'
    If (n_img .GT. 1) Then
        Write(unit,'(3A,I4,A)') '  Host: ',Trim(hostname),', ',n_img,' coarray images'
    Else
        Write(unit,'(2A)') '  Host: ',Trim(hostname)
    End If
    Write(unit,*)
    Write(unit,*)
    Write(unit,'(A)') '--------------------------------------------------'
    Write(unit,'(A)') 'SETUP INFORMATION'
    Write(unit,'(A)') '--------------------------------------------------'
    Write(unit,'(A)') '  Paths & Files:'
    Write(unit,'(2A)') '    Executable:     ',paths_files%program_exe
    Write(unit,'(2A)') '    Setup File:     ',paths_files%setup_file
    Write(unit,'(2A)') '    Resources Dir:  ',paths_files%resources_directory
    Write(unit,'(2A)') '    Cross Sect set: ',paths_files%resources_directory//paths_files%cs_setup_file
    Write(unit,'(2A)') '    Results Dir:    ',paths_files%results_directory
    If (Trim(paths_files%file_suffix) .EQ. '') Then
        Write(unit,'(2A)') '    File Suffix:    ','<none>'
    Else
        Write(unit,'(2A)') '    File Suffix:    ',paths_files%file_suffix
    End If
    Write(unit,'(2A)') '    Log File:       ',paths_files%log_file_name
    Write(unit,'(2A)') '    Setup File cpy: ',paths_files%run_file_name
    Write(unit,'(2A)') '    TE file:        ',paths_files%TE_file_name
    Write(unit,'(2A)') '    t file:         ',paths_files%t_file_name
    Write(unit,'(2A)') '    E file:         ',paths_files%E_file_name
    Write(unit,'(2A)') '    F file:         ',paths_files%F_file_name
    Write(unit,'(2A)') '    Arr Dirs file:        ',paths_files%d_file_name
    Write(unit,'(2A)') '    Arr Dirs mu file:     ',paths_files%m_file_name
    Write(unit,'(2A)') '    Arr Dirs omega file:  ',paths_files%o_file_name
    Write(unit,'(2A)') '    Slice Shape files:    ',paths_files%s_file_name//'<...>.txt'
    Write(unit,*)
    Write(unit,'(A,I11)') '  RNG Seed: ',RNG%seed
    Write(unit,'(A,I15,A,I15,A,F6.2,A)') '  Number of Histories: ',Sum(n_h_hit),' contributing, ',Sum(n_h_run),' total run, (',100._dp*Real(Sum(n_h_hit),dp)/Real(Sum(n_h_run),dp),'% efficency)'
    Write(unit,'(A)') '  Histories per image/thread:'
    Write(unit,'(A11,2A17)') 'Image','Contributing','Total Run'
    Write(unit,'(A11,2A17)') '-----','---------------','---------------'
    Do i = 1,n_img
        Write(unit,'(I11,2I17,A,F6.2,A)') i,n_h_hit(i),n_h_run(i),' (',100._dp*Real(n_h_hit(i),dp)/Real(n_h_run(i),dp),'%)'
    End Do
    Write(unit,*)
    Write(unit,*)
    Close(unit)
End Subroutine Write_Setup_Information

End Module Setups

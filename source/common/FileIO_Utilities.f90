Module FileIO_Utilities

    Implicit None
    Public
!In general, the contents of this module are public, but the
!following procedures are support routines or are accessible via 
!generic interfaces and need not be explicitly public...
    Private :: Open_for_Var_to_File  !support routine for VAR_TO_FILE
    Private :: SP_to_file          !Public via VAR_TO_FILE
    Private :: SP_1Darray_to_file  !Public via VAR_TO_FILE
    Private :: SP_2Darray_to_file  !Public via VAR_TO_FILE
    Private :: DP_to_file          !Public via VAR_TO_FILE
    Private :: DP_1Darray_to_file  !Public via VAR_TO_FILE
    Private :: DP_2Darray_to_file  !Public via VAR_TO_FILE
    Private :: I4_to_file          !Public via VAR_TO_FILE
    Private :: I4_1Darray_to_file  !Public via VAR_TO_FILE
    Private :: I4_2Darray_to_file  !Public via VAR_TO_FILE
    Private :: I8_to_file          !Public via VAR_TO_FILE
    Private :: I8_1Darray_to_file  !Public via VAR_TO_FILE
    Private :: I8_2Darray_to_file  !Public via VAR_TO_FILE
    Private :: C_to_file           !Public via VAR_TO_FILE
    Private :: L_to_file           !Public via VAR_TO_FILE
    Private :: Open_for_Var_from_File  !support routine for VAR_FROM_FILE
    Private :: SP_from_file          !Public via VAR_FROM_FILE
    Private :: SP_1Darray_from_file  !Public via VAR_FROM_FILE
    Private :: SP_2Darray_from_file  !Public via VAR_FROM_FILE
    Private :: DP_from_file          !Public via VAR_FROM_FILE
    Private :: DP_1Darray_from_file  !Public via VAR_FROM_FILE
    Private :: DP_2Darray_from_file  !Public via VAR_FROM_FILE
    Private :: I4_from_file          !Public via VAR_FROM_FILE
    Private :: I4_1Darray_from_file  !Public via VAR_FROM_FILE
    Private :: I4_2Darray_from_file  !Public via VAR_FROM_FILE
    Private :: I8_from_file          !Public via VAR_FROM_FILE
    Private :: I8_1Darray_from_file  !Public via VAR_FROM_FILE
    Private :: I8_2Darray_from_file  !Public via VAR_FROM_FILE
    Private :: C_from_file           !Public via VAR_FROM_FILE
    Private :: L_from_file           !Public via VAR_FROM_FILE
    Private :: Output_Message_C     !Public via OUTPUT_MESSAGE
    Private :: Output_Message_CI4   !Public via OUTPUT_MESSAGE
    Private :: Output_Message_CI4C  !Public via OUTPUT_MESSAGE
    Private :: Output_Message_CI8   !Public via OUTPUT_MESSAGE
    Private :: Output_Message_CI8C  !Public via OUTPUT_MESSAGE
    Private :: Output_Message_CSP   !Public via OUTPUT_MESSAGE
    Private :: Output_Message_CSPC  !Public via OUTPUT_MESSAGE
    Private :: Output_Message_CDP   !Public via OUTPUT_MESSAGE
    Private :: Output_Message_CDPC  !Public via OUTPUT_MESSAGE
    Private :: Log_Message_C     !Public via LOG_MESSAGE
    Private :: Log_Message_CI4   !Public via LOG_MESSAGE
    Private :: Log_Message_CI4C  !Public via LOG_MESSAGE
    Private :: Log_Message_CI8   !Public via LOG_MESSAGE
    Private :: Log_Message_CI8C  !Public via LOG_MESSAGE
    Private :: Log_Message_CSP   !Public via LOG_MESSAGE
    Private :: Log_Message_CSPC  !Public via LOG_MESSAGE
    Private :: Log_Message_CDP   !Public via LOG_MESSAGE
    Private :: Log_Message_CDPC  !Public via LOG_MESSAGE
    
    Interface Var_to_file
        Module Procedure SP_to_file
        Module Procedure SP_1Darray_to_file
        Module Procedure SP_2Darray_to_file
        Module Procedure DP_to_file
        Module Procedure DP_1Darray_to_file
        Module Procedure DP_2Darray_to_file
        Module Procedure I4_to_file
        Module Procedure I4_1Darray_to_file
        Module Procedure I4_2Darray_to_file
        Module Procedure I8_to_file
        Module Procedure I8_1Darray_to_file
        Module Procedure I8_2Darray_to_file
        Module Procedure C_to_file
        Module Procedure L_to_file
    End Interface Var_to_File
    
    Interface Var_from_file
        Module Procedure SP_from_file
        Module Procedure SP_1Darray_from_file
        Module Procedure SP_2Darray_from_file
        Module Procedure DP_from_file
        Module Procedure DP_1Darray_from_file
        Module Procedure DP_2Darray_from_file
        Module Procedure I4_from_file
        Module Procedure I4_1Darray_from_file
        Module Procedure I4_2Darray_from_file
        Module Procedure I8_from_file
        Module Procedure I8_1Darray_from_file
        Module Procedure I8_2Darray_from_file
        Module Procedure C_from_file
        Module Procedure L_from_file
    End Interface Var_from_File
    
    Interface Output_Message
        Module Procedure Output_Message_C
        Module Procedure Output_Message_CI4
        Module Procedure Output_Message_CI4C
        Module Procedure Output_Message_CI8
        Module Procedure Output_Message_CI8C
        Module Procedure Output_Message_CSP
        Module Procedure Output_Message_CSPC
        Module Procedure Output_Message_CDP
        Module Procedure Output_Message_CDPC
    End Interface Output_Message

    Interface Log_Message
        Module Procedure Log_Message_C
        Module Procedure Log_Message_CI4
        Module Procedure Log_Message_CI4C
        Module Procedure Log_Message_CI8
        Module Procedure Log_Message_CI8C
        Module Procedure Log_Message_CSP
        Module Procedure Log_Message_CSPC
        Module Procedure Log_Message_CDP
        Module Procedure Log_Message_CDPC
    End Interface Log_Message

!  Character & I/O constants for LINUX vs Windows file systems
#   if LIN_OS
        Character(1), Parameter :: slash = '/'  !<--LINUX implementation
#   else
        Character(1), Parameter :: slash = '\'  !<--WINDOWS implementation
#   endif
    
!  Arbitrary maximum string lengths, for portability
    Integer, Parameter :: max_path_len = 255
    Integer, Parameter :: max_line_len = 80
!  Non-printing character constants for portability
    Character(1), Parameter :: creturn = achar(13)
    Character(1), Parameter :: newline = achar(10)
    Character(1), Parameter :: ding = achar(7)
!  Non-printing character constants for portability
    Character(80), Parameter :: full_dash_line = '--------------------------------------------------------------------------------'
    Character(60), Parameter :: long_dash_line = '------------------------------------------------------------'
    Character(40), Parameter :: half_dash_line = '----------------------------------------'
    Character(40), Parameter :: tiny_dash_line = '--------------------'
    
Contains

!!!!!!!!!!  VAR_TO_FILE ROUTINES  !!!!!!!!!!
Subroutine Open_for_Var_to_File(file_name,unit,stat)
    Implicit None
    Character(*), Intent(In) :: file_name
    Integer, Intent(Out) :: unit
    Integer, Intent(Out) :: stat
    
    Open(NEWUNIT = unit , FILE = file_name , STATUS = 'REPLACE' , ACTION = 'WRITE' , FORM = 'UNFORMATTED', IOSTAT = stat)
End Subroutine Open_for_Var_to_File

Subroutine SP_to_file(r,file_name)
    Use Kinds, Only: sp
    Implicit None
    Real(sp), Intent(In) :: r
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine SP_to_file

Subroutine SP_1Darray_to_file(r,file_name)
    Use Kinds, Only: sp
    Implicit None
    Real(sp), Intent(In) :: r(:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine SP_1Darray_to_file

Subroutine SP_2Darray_to_file(r,file_name)
    Use Kinds, Only: sp
    Implicit None
    Real(sp), Intent(In) :: r(:,:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine SP_2Darray_to_file

Subroutine DP_to_file(r,file_name)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: r
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine DP_to_file

Subroutine DP_1Darray_to_file(r,file_name)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: r(:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine DP_1Darray_to_file

Subroutine DP_2Darray_to_file(r,file_name)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: r(:,:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine DP_2Darray_to_file

Subroutine I4_to_file(i,file_name)
    Use Kinds, Only: il
    Implicit None
    Integer(il), Intent(In) :: i
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I4_to_file

Subroutine I4_1Darray_to_file(i,file_name)
    Use Kinds, Only: il
    Implicit None
    Integer(il), Intent(In) :: i(:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I4_1Darray_to_file

Subroutine I4_2Darray_to_file(i,file_name)
    Use Kinds, Only: il
    Implicit None
    Integer(il), Intent(In) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I4_2Darray_to_file

Subroutine I8_to_file(i,file_name)
    Use Kinds, Only: id
    Implicit None
    Integer(id), Intent(In) :: i
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I8_to_file

Subroutine I8_1Darray_to_file(i,file_name)
    Use Kinds, Only: id
    Implicit None
    Integer(id), Intent(In) :: i(:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I8_1Darray_to_file

Subroutine I8_2Darray_to_file(i,file_name)
    Use Kinds, Only: id
    Implicit None
    Integer(id), Intent(In) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I8_2Darray_to_file

Subroutine C_to_file(C,file_name)
    Implicit None
    Character(*), Intent(In) :: C
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    Character(max_path_len) :: Cmax
    
    If (Len(C) .GT. max_path_len) Call Output_Message('ERROR:  FileIO_Utilities: C_to_file:  Write string is longer than MAX_PATH_LEN',kill=.TRUE.)
    Cmax = C
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: C_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) Cmax
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: C_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine C_to_file

Subroutine L_to_file(L,file_name)
    Implicit None
    Logical, Intent(In) :: L
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat

    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: L_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) L
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: L_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine L_to_file

!!!!!!!!!!  VAR_FROM_FILE ROUTINES  !!!!!!!!!!
Subroutine Open_for_Var_from_File(file_name,unit,stat)
    Implicit None
    Character(*), Intent(In) :: file_name
    Integer, Intent(Out) :: unit
    Integer, Intent(Out) :: stat
    
    Open(NEWUNIT = unit , FILE = file_name , STATUS = 'OLD' , ACTION = 'READ' , FORM = 'UNFORMATTED' , IOSTAT = stat)
End Subroutine Open_for_Var_from_File

Subroutine SP_from_file(r,file_name,delete_file)
    Use Kinds, Only: sp
    Implicit None
    Real(sp), Intent(Out) :: r
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine SP_from_file

Subroutine SP_1Darray_from_file(r,file_name,delete_file)
    Use Kinds, Only: Sp
    Implicit None
    Real(sp), Intent(Out) :: r(:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine SP_1Darray_from_file

Subroutine SP_2Darray_from_file(r,file_name,delete_file)
    Use Kinds, Only: sp
    Implicit None
    Real(sp), Intent(Out) :: r(:,:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: SP_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine SP_2Darray_from_file

Subroutine DP_from_file(r,file_name,delete_file)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(Out) :: r
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine DP_from_file

Subroutine DP_1Darray_from_file(r,file_name,delete_file)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(Out) :: r(:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine DP_1Darray_from_file

Subroutine DP_2Darray_from_file(r,file_name,delete_file)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(Out) :: r(:,:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: DP_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine DP_2Darray_from_file

Subroutine I4_from_file(i,file_name,delete_file)
    Use Kinds, Only: il
    Implicit None
    Integer(il), Intent(Out) :: i
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit) i
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine I4_from_file

Subroutine I4_1Darray_from_file(i,file_name,delete_file)
    Use Kinds, Only: il
    Implicit None
    Integer(il), Intent(Out) :: i(:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine I4_1Darray_from_file

Subroutine I4_2Darray_from_file(i,file_name,delete_file)
    Use Kinds, Only: il
    Implicit None
    Integer(il), Intent(Out) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: I4_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine I4_2Darray_from_file

Subroutine I8_from_file(i,file_name,delete_file)
    Use Kinds, Only: id
    Implicit None
    Integer(id), Intent(Out) :: i
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit) i
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine I8_from_file

Subroutine I8_1Darray_from_file(i,file_name,delete_file)
    Use Kinds, Only: id
    Implicit None
    Integer(id), Intent(Out) :: i(:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine I8_1Darray_from_file

Subroutine I8_2Darray_from_file(i,file_name,delete_file)
    Use Kinds, Only: id
    Implicit None
    Integer(id), Intent(Out) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: I8_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine I8_2Darray_from_file

Subroutine C_from_file(C,file_name,delete_file)
    Implicit None
    Character(*), Intent(Out) :: C
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Character(max_path_len) :: Cmax
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: C_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) Cmax
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: C_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Len(Trim(Cmax)) .GT. Len(C)) Call Output_Message('ERROR:  FileIO_Utilities: C_from_file:  Read string is longer than requested string',kill=.TRUE.)
    C = Trim(Cmax)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine C_from_file

Subroutine L_from_file(L,file_name,delete_file)
    Implicit None
    Logical, Intent(Out) :: L
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_from_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  FileIO_Utilities: L_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) L
    If (stat .GT. 0) Call Output_Message('ERROR:  FileIO_Utilities: L_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Present(delete_file)) Then
        If (delete_file) Then
            Close(unit , STATUS = 'DELETE')
        Else
            Close(unit)
        End If
    Else
        Close(unit)
    End If
End Subroutine L_from_file

!!!!!!!!!!  FILE AND DIRECTORY MANAGEMENT ROUTINES  !!!!!!!!!!
Subroutine Working_Directory(GETdir,PUTdir,s)
    !Gets and/or sets the current working directory, appending a slash character if supplied
    !UNSTANDARD: GETCWD and CHDIR are extensions
#   if IFORT
        Use IFPORT, Only: GETCWD  !<--IFORT implementation
        Use IFPORT, Only: CHDIR  !<--IFORT implementation
#   endif
    Implicit None
    Character(max_path_len), Intent(Out), Optional :: GETdir
    Character(max_path_len), Intent(In), Optional :: PUTdir
    Character(1), Intent(In), Optional :: s
    Integer :: stat
    
    If (Present(GETdir)) Then
#       if IFORT
            stat = GETCWD(GETdir)  !<--IFORT implementation
#       else
            Call GETCWD(GETdir , stat)  !<--GFORT implementation
#       endif
        If (Present(s)) GETdir = Trim(GETdir)//s
    End If
    If (Present(PUTdir)) Then
        stat = CHDIR(PUTdir)
    End If
End Subroutine Working_Directory

Function Check_Directory(dirname) Result(exists)
    !returns TRUE if the directory exists in the current working directory
    !UNSTANDARD: DIRECTORY is an Intel extension
    Implicit None
    Logical :: exists
    Character(*), Intent(In) :: dirname
    
#   if IFORT
        INQUIRE(DIRECTORY = dirname , EXIST = exists)  !<--IFORT implementation
#   else
        INQUIRE(FILE = dirname , EXIST = exists)
#   endif
End Function Check_Directory

Subroutine Create_Directory(dirname)
    !Creates a new directory in the current working directory
    Implicit None
    Character(*), Intent(In) :: dirname

    If (.NOT.(Check_Directory(dirname))) Call EXECUTE_COMMAND_LINE('mkdir '//dirname)
End Subroutine Create_Directory

Subroutine Delete_Directory(dirname)
    !Deletes a directory (AND ALL CONTENTS) in the current working directory
    !UNPORTABLE The commands listed for EXECUTE_COMMAND_LINE may be implementation or host specific
    Implicit None
    Character(*), Intent(In) :: dirname

#   if LIN_OS
        Call EXECUTE_COMMAND_LINE('rm -f -r '//dirname)  !<--LINUX implementation
#   else
        Call EXECUTE_COMMAND_LINE('rmdir /S /Q '//dirname)  !<--WINDOWS implementation
#   endif
End Subroutine Delete_Directory

Subroutine Clean_Directory(dirname)
    !Deletes ALL CONTENTS in a directory (works by deleting the entire directory and creating a new directory with the same name)
    Implicit None
    Character(*), Intent(In) :: dirname

    Call Delete_Directory(dirname)
    Call Create_Directory(dirname)
End Subroutine Clean_Directory

!!!!!!!!!!  LOG_MESSAGE ROUTINES  !!!!!!!!!!
Subroutine Open_for_Log_Message(file_name,unit,stat)
    Implicit None
    Character(*), Intent(In) :: file_name
    Integer, Intent(Out) :: unit
    Integer, Intent(Out) :: stat
    Integer :: i
    
    i = 0
    Do
        Open(NEWUNIT = unit , FILE = file_name , STATUS = 'UNKNOWN' , ACTION = 'WRITE' , POSITION = 'APPEND' , IOSTAT = stat)
        If (stat .EQ. 0) Exit  !NORMAL EXIT
        i = i + 1
        If (i .GT. 1000) Exit  !FAILED EXIT
        Call Wait(100)  !wait 100 milliseconds before retrying
    End Do
End Subroutine Open_for_Log_Message

Subroutine Log_Stamp(s)
    Implicit None
    Character(max_line_len) :: s
    
    Write(s,'(A,I0)') Date_Time_stamp()//' Worker ',Worker_Index()
End Subroutine Log_Stamp

Subroutine Log_Message_C(message,logfile)
    Implicit None
    Character(*), Intent(In) :: message
    Character(*), Intent(In) :: logfile
    Integer :: unit,stat
    Character(max_line_len) :: stamp
    
    Call Log_Stamp(stamp)
    Call Open_for_Log_Message(logfile,unit,stat)
    If (stat .NE. 0) Then
        Call Output_Message('ERROR:  FileIO_Utilities: Log_Message_C:  File open error, '//logfile//', IOSTAT=',stat)
        Call Output_Message('MESSAGE NOT LOGGED.')
        Return
    End If
    Write(unit,'(A)') Trim(stamp)
    Write(unit,'(A)') message
    Write(unit,*)
    Close(unit)
End Subroutine Log_Message_C

Subroutine Log_Message_CI4(message,i,logfile)
    Implicit None
    Character(*), Intent(In) :: message
    Integer(4), Intent(In) :: i
    Character(*), Intent(In) :: logfile
    Integer :: unit,stat
    Character(max_line_len) :: stamp
    
    Call Log_Stamp(stamp)
    Call Open_for_Log_Message(logfile,unit,stat)
    If (stat .NE. 0) Then
        Call Output_Message('ERROR:  FileIO_Utilities: Log_Message_CI4:  File open error, '//logfile//', IOSTAT=',stat)
        Call Output_Message('MESSAGE NOT LOGGED.')
        Return
    End If
    Write(unit,'(A)') Trim(stamp)
    Write(unit,'(A,I0)') message,i
    Write(unit,*)
    Close(unit)
End Subroutine Log_Message_CI4

Subroutine Log_Message_CI4C(message1,i,message2,logfile)
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Integer(4), Intent(In) :: i
    Character(*), Intent(In) :: logfile
    Integer :: unit,stat
    Character(max_line_len) :: stamp
    
    Call Log_Stamp(stamp)
    Call Open_for_Log_Message(logfile,unit,stat)
    If (stat .NE. 0) Then
        Call Output_Message('ERROR:  FileIO_Utilities: Log_Message_CI4C:  File open error, '//logfile//', IOSTAT=',stat)
        Call Output_Message('MESSAGE NOT LOGGED.')
        Return
    End If
    Write(unit,'(A)') Trim(stamp)
    Write(unit,'(A,I0,A)') message1,i,message2
    Write(unit,*)
    Close(unit)
End Subroutine Log_Message_CI4C

Subroutine Log_Message_CI8(message,i,logfile)
    Use Kinds, Only: id
    Implicit None
    Character(*), Intent(In) :: message
    Integer(id), Intent(In) :: i
    Character(*), Intent(In) :: logfile
    Integer :: unit,stat
    Character(max_line_len) :: stamp
    
    Call Log_Stamp(stamp)
    Call Open_for_Log_Message(logfile,unit,stat)
    If (stat .NE. 0) Then
        Call Output_Message('ERROR:  FileIO_Utilities: Log_Message_CI8:  File open error, '//logfile//', IOSTAT=',stat)
        Call Output_Message('MESSAGE NOT LOGGED.')
        Return
    End If
    Write(unit,'(A)') Trim(stamp)
    Write(unit,'(A,I0)') message,i
    Write(unit,*)
    Close(unit)
End Subroutine Log_Message_CI8

Subroutine Log_Message_CI8C(message1,i,message2,logfile)
    Use Kinds, Only: id
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Integer(id), Intent(In) :: i
    Character(*), Intent(In) :: logfile
    Integer :: unit,stat
    Character(max_line_len) :: stamp
    
    Call Log_Stamp(stamp)
    Call Open_for_Log_Message(logfile,unit,stat)
    If (stat .NE. 0) Then
        Call Output_Message('ERROR:  FileIO_Utilities: Log_Message_CI8C:  File open error, '//logfile//', IOSTAT=',stat)
        Call Output_Message('MESSAGE NOT LOGGED.')
        Return
    End If
    Write(unit,'(A)') Trim(stamp)
    Write(unit,'(A,I0,A)') message1,i,message2
    Write(unit,*)
    Close(unit)
End Subroutine Log_Message_CI8C

Subroutine Log_Message_CSP(message,r,logfile)
    Use Kinds, Only: sp
    Implicit None
    Character(*), Intent(In) :: message
    Real(sp), Intent(In) :: r
    Character(*), Intent(In) :: logfile
    Integer :: unit,stat
    Character(max_line_len) :: stamp
    
    Call Log_Stamp(stamp)
    Call Open_for_Log_Message(logfile,unit,stat)
    If (stat .NE. 0) Then
        Call Output_Message('ERROR:  FileIO_Utilities: Log_Message_CSP:  File open error, '//logfile//', IOSTAT=',stat)
        Call Output_Message('MESSAGE NOT LOGGED.')
        Return
    End If
    Write(unit,'(A)') Trim(stamp)
    Write(unit,'(A,F0.8)') message,r
    Write(unit,*)
    Close(unit)
End Subroutine Log_Message_CSP

Subroutine Log_Message_CSPC(message1,r,message2,logfile)
    Use Kinds, Only: sp
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Real(sp), Intent(In) :: r
    Character(*), Intent(In) :: logfile
    Integer :: unit,stat
    Character(max_line_len) :: stamp
    
    Call Log_Stamp(stamp)
    Call Open_for_Log_Message(logfile,unit,stat)
    If (stat .NE. 0) Then
        Call Output_Message('ERROR:  FileIO_Utilities: Log_Message_CSPC:  File open error, '//logfile//', IOSTAT=',stat)
        Call Output_Message('MESSAGE NOT LOGGED.')
        Return
    End If
    Write(unit,'(A)') Trim(stamp)
    Write(unit,'(A,F0.8,A)') message1,r,message2
    Write(unit,*)
    Close(unit)
End Subroutine Log_Message_CSPC

Subroutine Log_Message_CDP(message,r,logfile)
    Use Kinds, Only: dp
    Implicit None
    Character(*), Intent(In) :: message
    Real(dp), Intent(In) :: r
    Character(*), Intent(In) :: logfile
    Integer :: unit,stat
    Character(max_line_len) :: stamp
    
    Call Log_Stamp(stamp)
    Call Open_for_Log_Message(logfile,unit,stat)
    If (stat .NE. 0) Then
        Call Output_Message('ERROR:  FileIO_Utilities: Log_Message_CDP:  File open error, '//logfile//', IOSTAT=',stat)
        Call Output_Message('MESSAGE NOT LOGGED.')
        Return
    End If
    Write(unit,'(A)') Trim(stamp)
    Write(unit,'(A,F0.16)') message,r
    Write(unit,*)
    Close(unit)
End Subroutine Log_Message_CDP

Subroutine Log_Message_CDPC(message1,r,message2,logfile)
    Use Kinds, Only: dp
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Real(dp), Intent(In) :: r
    Character(*), Intent(In) :: logfile
    Integer :: unit,stat
    Character(max_line_len) :: stamp
    
    Call Log_Stamp(stamp)
    Call Open_for_Log_Message(logfile,unit,stat)
    If (stat .NE. 0) Then
        Call Output_Message('ERROR:  FileIO_Utilities: Log_Message_CDPC:  File open error, '//logfile//', IOSTAT=',stat)
        Call Output_Message('MESSAGE NOT LOGGED.')
        Return
    End If
    Write(unit,'(A)') Trim(stamp)
    Write(unit,'(A,F0.16,A)') message1,r,message2
    Write(unit,*)
    Close(unit)
End Subroutine Log_Message_CDPC

!!!!!!!!!!  OUTPUT_MESSAGE ROUTINES !!!!!!!!!!
Subroutine Make_Boom()
    Implicit None
    
    Write(*,*)
    Write(*,'(A)') '              ____           '
    Write(*,'(A)') '      __,-~~/~    `---.      '
    Write(*,'(A)') '    _/_,---(      ,    )     '
    Write(*,'(A)') '   /        <    /   )  \    '
    Write(*,'(A)') '   \/  ~"~"~"~"~"~\~"~)~"/   '
    Write(*,'(A)') '   (_ (   \  (     >    \)   '
    Write(*,'(A)') '    \_( _ <         >_> /    '
    Write(*,'(A)') '         " |"|"|"| "         '
    Write(*,'(A)') '        .-=| : : |=-.        '
    Write(*,'(A)') '        `-=#$%&%$#=-`        '
    Write(*,'(A)') '           |:   :|           '
    Write(*,'(A)') '  _____.(~#%&$@%#&#~)._____  '//ding
End Subroutine Make_Boom

Subroutine Output_Message_C(message,kill)
    Implicit None
    Character(*), Intent(In) :: message
    Logical, Intent(In), Optional :: kill
    Integer :: n,m
    Integer :: i1,i2
    Integer, Parameter :: w = max_line_len
    
    Write(*,*) ding
    n = Len(message)
    If (n .LE. w) Then  !the message fits on one line
        Write(*,'(A)') message
    Else  !the message is multiple lines
        m = 1
        Do
            i1 = (m - 1) * w + 1
            i2 = Min(n,m * w)
            Write(*,'(A)') message(i1:i2)
            If (i2 .EQ. n) Exit
            m = m + 1
        End Do
    End If
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_C

Subroutine Output_Message_CI4(message,i,kill)
    Use Kinds, Only: il
    Implicit None
    Character(*), Intent(In) :: message
    Integer(il), Intent(In) :: i
    Logical, Intent(In), Optional :: kill
    Character(12) :: ichar
    
    Write(ichar,'(I0)') i
    Call Output_Message_C(message//Trim(ichar))
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CI4

Subroutine Output_Message_CI4C(message1,i,message2,kill)
    Use Kinds, Only: il
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Integer(il), Intent(In) :: i
    Logical, Intent(In), Optional :: kill
    Character(12) :: ichar
    
    Write(ichar,'(I0)') i
    Call Output_Message_C(message1//Trim(ichar)//message2)
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CI4C

Subroutine Output_Message_CI8(message,i,kill)
    Implicit None
    Character(*), Intent(In) :: message
    Integer(8), Intent(In) :: i
    Logical, Intent(In), Optional :: kill
    Character(17) :: ichar
    
    Write(ichar,'(I0)') i
    Call Output_Message_C(message//Trim(ichar))
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CI8

Subroutine Output_Message_CI8C(message1,i,message2,kill)
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Integer(8), Intent(In) :: i
    Logical, Intent(In), Optional :: kill
    Character(17) :: ichar
    
    Write(ichar,'(I0)') i
    Call Output_Message_C(message1//Trim(ichar)//message2)
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CI8C

Subroutine Output_Message_CSP(message,r,kill)
    Use Kinds, Only: sp
    Implicit None
    Character(*), Intent(In) :: message
    Real(sp), Intent(In) :: r
    Logical, Intent(In), Optional :: kill
    Character(36) :: rchar
    
    Write(rchar,'(F0.8)') r
    Call Output_Message_C(message//Trim(rchar))
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CSP

Subroutine Output_Message_CSPC(message1,r,message2,kill)
    Use Kinds, Only: sp
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Real(sp), Intent(In) :: r
    Logical, Intent(In), Optional :: kill
    Character(36) :: rchar
    
    Write(rchar,'(F0.8)') r
    Call Output_Message_C(message1//Trim(rchar)//message2)
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CSPC

Subroutine Output_Message_CDP(message,r,kill)
    Use Kinds, Only: dp
    Implicit None
    Character(*), Intent(In) :: message
    Real(dp), Intent(In) :: r
    Logical, Intent(In), Optional :: kill
    Character(48) :: rchar
    
    Write(rchar,'(F0.16)') r
    Call Output_Message_C(message//Trim(rchar))
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CDP

Subroutine Output_Message_CDPC(message1,r,message2,kill)
    Use Kinds, Only: dp
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Real(dp), Intent(In) :: r
    Logical, Intent(In), Optional :: kill
    Character(48) :: rchar
    
    Write(rchar,'(F0.16)') r
    Call Output_Message_C(message1//Trim(rchar)//message2)
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CDPC

!!!!!!!!!!  THREAD, IMAGE, and WORKER INDEXING ROUTINES  !!!!!!!!!!
Function Worker_Index(OMP_threaded,CAF_imaged) Result(i)
#   if CAF
        Use OMP_LIB, Only: OMP_GET_NUM_THREADS,OMP_GET_THREAD_NUM
#   endif
    Implicit None
    Integer :: i
    Logical, Intent(Out), Optional :: OMP_threaded
    Logical, Intent(Out), Optional :: CAF_imaged

    If (Present(OMP_threaded)) OMP_threaded = .FALSE.
    If (Present(CAF_imaged)) CAF_imaged = .FALSE.
#   if CAF
        If (n_Workers().GT.1) Then !Parallel threads or images are running
            If (num_images() .GT. 1) Then  !use the coarray image number to index the worker
                i = this_image()  !coarray images are numbered starting at 1
                If (Present(CAF_imaged)) CAF_imaged = .TRUE.
            Else If (OMP_GET_NUM_THREADS() .GT. 1) Then  !use the OpenMP thread number to index the worker
                i = OMP_GET_THREAD_NUM() + 1  !OpenMP threads are numbered starting at zero
                If (Present(OMP_threaded)) OMP_threaded = .TRUE.
            Else
                Call Output_Message('ERROR:  FileIO_Utilities: Worker_Index:  Unable to resolve thread or image number.',kill=.TRUE.)
            End If
        Else
            i = 1  !default value for single threaded/imaged applications
        End If
#   else
        i = 1  !default value for single threaded/imaged applications
#   endif
End Function Worker_Index

Function n_Workers() Result(n)
#   if CAF
        Use OMP_LIB, Only: OMP_GET_NUM_THREADS
#   endif
    Implicit None
    Integer :: n

#   if CAF
        If (OMP_GET_NUM_THREADS().GT.1 .OR. num_images().GT.1) Then !Parallel threads or images are running
            If (num_images() .GT. 1) Then
                n = num_images()
            Else If (OMP_GET_NUM_THREADS() .GT. 1) Then  !use the OpenMP thread number to index the thread
                n = OMP_GET_NUM_THREADS()
            Else
                Call Output_Message('ERROR:  FileIO_Utilities: n_Workers:  Unable to resolve number of threads or images.',kill=.TRUE.)
            End If
        Else
            n = 1  !default value for single threaded/imaged applications
        End If
#   else
        n = 1  !default value for single threaded/imaged applications
#   endif
End Function n_Workers

!!!!!!!!!!  TIMING ROUTINES  !!!!!!!!!!
Function Second_of_Month() Result(s)
    !Returns number of seconds since the beginning of the month
    !(to millisecond resolution, and according to the system clock)
    Use Kinds, Only: dp
    Implicit None
    Real(dp) :: s
    Integer :: v(8)
    Integer, Parameter :: days2sec = 24*60*60
    Integer, Parameter :: hrs2sec  = 60*60
    Integer, Parameter :: min2sec  = 60
    Real(dp), Parameter :: ms2sec  = 1.E-3_dp
    
    Call DATE_AND_TIME ( values = v )
    s = Real(   v(3)*days2sec & !day of the month
            & + v(5)*hrs2sec  & !hour of the day
            & + v(6)*min2sec  & !minute of the hour
            & + v(7)          & !seconds
                              & ,dp ) &
            & + Real(v(8),dp)*ms2sec !milliseconds
End Function Second_of_Month

Function Delta_Time(start_clock,clock_then,clock_now) Result(sec)
    !Returns number of seconds since an arbitrary start time (set by a previous call to this routine)
    !(to millisecond resolution, and according to the system clock)
    Use Kinds, Only: dp
    Use Kinds, Only: il
    Implicit None
    Real(dp) :: sec
    Integer(il), Intent(Out), Optional :: start_clock
    Integer(il), Intent(In), Optional :: clock_then
    Integer(il), Intent(Out), Optional :: clock_now
    Integer(il) :: clock,clock_delta
    Real(dp), Parameter :: ms2sec  = 1.E-3_dp
    
    sec = 0._dp  !default value
    Call SYSTEM_CLOCK(clock)
    If (Present(start_clock)) start_clock = clock
    If (Present(clock_then)) Then
        If (clock .LT. clock_then) Then
            clock_delta = clock + (HUGE(clock_then) - clock_then)
        Else
            clock_delta = clock - clock_then
        End If
        sec = Real(clock_delta,dp)*ms2sec
    End If
    If (Present(clock_now)) clock_now = clock
End Function Delta_Time

Function Date_Time_string() Result(s)
    Implicit None
    Character(20) :: s
    Integer :: v(8)
    Character(5), Parameter :: months(12) = (/ ' Jan ',' Feb ',' Mar ',' Apr ',' May ',' Jun ',' Jul ',' Aug ',' Sep ',' Oct ',' Nov ',' Dec ' /)
    
    Call DATE_AND_TIME(VALUES = v)
    Write(s,'(I2.2,A5,I4.4,A1,I2.2,A1,I2.2,A1,I2.2)') v(3),months(v(2)),v(1),' ',v(5),':',v(6),':',v(7)
End Function Date_Time_string

Function Date_Time_stamp() Result(s)
    Implicit None
    Character(20) :: s
    Character(8) :: d
    Character(10) :: t
    
    Call DATE_AND_TIME(DATE = d, TIME = t)
    s = d//' '//t//' '
End Function Date_Time_stamp

Function Get_Host_Name() Result(s)
    !UNSTANDARD: HOSTNAM (IFORT) and HOSTNM (GFORT) are extensions
#   if IFORT
        Use IFPORT, Only: HOSTNAM  !<--IFORT implementation
#   endif
    Implicit None
    Character(max_line_len) :: s
    Integer :: stat
    
#   if (GFORT || IFORT)
#       if GFORT
            Call HOSTNM(s,stat)  !<--GFORT implementation
            If (stat .NE. 0) s = '<<UNKNOWN>>'
#       endif
#       if IFORT
            stat = HOSTNAM(s)  !<--IFORT implementation
            If (stat .NE. 0) s = '<<UNKNOWN>>'
#       endif
#   else
        s = '<<UNKNOWN>>'
#   endif
End Function Get_Host_Name

Subroutine Wait(ms)
    Use Kinds, Only: il
    Implicit None
    Integer(il), Intent(In) :: ms !number of milliseconds to wait before returning
    Integer(il) :: t1,t2,u

    Call SYSTEM_CLOCK(count = t1)
    u = 0_il
    Do
        Call SYSTEM_CLOCK(count = t2)
        If (t2 - t1 + u .GT. ms) Then
            Return
        Else If (t2 .LT. t1) Then !clock has wrapped around
        !this is rare (about once per 25 days), but would cause the loop to stall
            u = HUGE(t1) - t1
            t1 = 0_il
        End If
    End Do
End Subroutine Wait

End Module FileIO_Utilities

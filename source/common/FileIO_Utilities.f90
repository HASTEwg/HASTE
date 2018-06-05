Module FileIO_Utilities

    Implicit None
    Public
!In general, the contents of this module are public, but the
!following procedures are support routines or are accessible via 
!generic interfaces and need not be explicitly public...
    Private :: Open_for_Var_to_File  !support routine for VAR_TO_FILE
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
    Private :: Output_Message_CL    !Public via OUTPUT_MESSAGE
    Private :: Output_Message_CLC   !Public via OUTPUT_MESSAGE
    
    Interface Var_to_file
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
        Module Procedure Output_Message_CL
        Module Procedure Output_Message_CLC
    End Interface Output_Message
    
!  Character & I/O constants for LINUX vs Windows file systems
    !DIR$ IF DEFINED (MIC)
        Character(1), Parameter :: slash = '/'
        Character(8), Parameter :: fSHARE = 'DENYNONE'
    !DIR$ ELSE
        Character(1), Parameter :: slash = '\'
        Character(6), Parameter :: fSHARE = 'DENYWR'
    !DIR$ END IF
    
!  Non-printing character constants for portability
    Character(1), Parameter :: creturn = achar(13)
    Character(1), Parameter :: newline = achar(10)
    Character(1), Parameter :: ding = achar(7)
    
Contains

Subroutine Open_for_Var_to_File(file_name,unit,stat)
    Implicit None
    Character(*), Intent(In) :: file_name
    Integer, Intent(Out) :: unit
    Integer, Intent(Out) :: stat
    
    Open(NEWUNIT = unit , FILE = file_name , STATUS = 'REPLACE' , ACTION = 'WRITE' , FORM = 'UNFORMATTED', IOSTAT = stat , SHARE = 'DENYRW')
End Subroutine Open_for_Var_to_File

Subroutine Open_for_Var_from_File(file_name,dont_share,unit,stat)
    Implicit None
    Character(*), Intent(In) :: file_name
    Logical, Intent(In) :: dont_share
    Integer, Intent(Out) :: unit
    Integer, Intent(Out) :: stat
    
    If (dont_share) Then
        Open(NEWUNIT = unit , FILE = file_name , STATUS = 'OLD' , ACTION = 'READ' , FORM = 'UNFORMATTED' , IOSTAT = stat , SHARE = 'DENYRW')
    Else
        Open(NEWUNIT = unit , FILE = file_name , STATUS = 'OLD' , ACTION = 'READ' , FORM = 'UNFORMATTED' , IOSTAT = stat , SHARE = fSHARE)
    End If
End Subroutine Open_for_Var_from_File

Subroutine DP_to_file(r,file_name)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(In) :: r
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: DP_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: DP_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
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
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: DP_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: DP_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
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
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: DP_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: DP_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine DP_2Darray_to_file

Subroutine I4_to_file(i,file_name)
    Implicit None
    Integer(4), Intent(In) :: i
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I4_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I4_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I4_to_file

Subroutine I4_1Darray_to_file(i,file_name)
    Implicit None
    Integer(4), Intent(In) :: i(:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I4_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I4_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I4_1Darray_to_file

Subroutine I4_2Darray_to_file(i,file_name)
    Implicit None
    Integer(4), Intent(In) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I4_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I4_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I4_2Darray_to_file

Subroutine I8_to_file(i,file_name)
    Implicit None
    Integer(8), Intent(In) :: i
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I8_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I8_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I8_to_file

Subroutine I8_1Darray_to_file(i,file_name)
    Implicit None
    Integer(8), Intent(In) :: i(:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I8_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I8_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I8_1Darray_to_file

Subroutine I8_2Darray_to_file(i,file_name)
    Implicit None
    Integer(8), Intent(In) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I8_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I8_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine I8_2Darray_to_file

Subroutine C_to_file(C,file_name)
    Use IFPORT, Only: $MAXPATH
    Implicit None
    Character(*), Intent(In) :: C
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    Character($MAXPATH) :: Cmax
    
    If (Len(C) .GT. $MAXPATH) Call Output_Message('ERROR:  Utilities: C_to_file:  Write string is longer than $MAXPATH',kill=.TRUE.)
    Cmax = C
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: C_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) Cmax
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: C_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine C_to_file

Subroutine L_to_file(L,file_name)
    Implicit None
    Logical, Intent(In) :: L
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat

    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: L_to_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Write(unit , IOSTAT = stat) L
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: L_to_file:  File write error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Close(unit)
End Subroutine L_to_file

Subroutine DP_from_file(r,file_name,delete_file)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(Out) :: r
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: DP_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: DP_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
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
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: DP_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: DP_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
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
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: DP_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: DP_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
    Else
        Close(unit)
    End If
End Subroutine DP_2Darray_from_file

Subroutine I4_from_file(i,file_name,delete_file)
    Implicit None
    Integer(4), Intent(Out) :: i
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I4_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit) i
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: I4_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
    Else
        Close(unit)
    End If
End Subroutine I4_from_file

Subroutine I4_1Darray_from_file(i,file_name,delete_file)
    Implicit None
    Integer(4), Intent(Out) :: i(:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I4_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: I4_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
    Else
        Close(unit)
    End If
End Subroutine I4_1Darray_from_file

Subroutine I4_2Darray_from_file(i,file_name,delete_file)
    Implicit None
    Integer(4), Intent(Out) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I4_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: I4_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
    Else
        Close(unit)
    End If
End Subroutine I4_2Darray_from_file

Subroutine I8_from_file(i,file_name,delete_file)
    Implicit None
    Integer(8), Intent(Out) :: i
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I8_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit) i
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: I8_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
    Else
        Close(unit)
    End If
End Subroutine I8_from_file

Subroutine I8_1Darray_from_file(i,file_name,delete_file)
    Implicit None
    Integer(8), Intent(Out) :: i(:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I8_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: I8_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
    Else
        Close(unit)
    End If
End Subroutine I8_1Darray_from_file

Subroutine I8_2Darray_from_file(i,file_name,delete_file)
    Implicit None
    Integer(8), Intent(Out) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: I8_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: I8_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
    Else
        Close(unit)
    End If
End Subroutine I8_2Darray_from_file

Subroutine C_from_file(C,file_name,delete_file)
    Use IFPORT, Only: $MAXPATH
    Implicit None
    Character(*), Intent(Out) :: C
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Character($MAXPATH) :: Cmax
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: C_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) Cmax
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: C_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (Len(Trim(Cmax)) .GT. Len(C)) Call Output_Message('ERROR:  Utilities: C_from_file:  Read string is longer than requested string',kill=.TRUE.)
    C = Trim(Cmax)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
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
    Logical :: del_f
    
    If (Present(delete_file)) Then
        del_f = delete_file
    Else
        del_f = .FALSE.
    End If
    Call Open_for_Var_from_File(file_name,del_f,unit,stat)
    If (stat .NE. 0) Call Output_Message('ERROR:  Utilities: L_from_file:  File open error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    Read(unit , IOSTAT = stat) L
    If (stat .GT. 0) Call Output_Message('ERROR:  Utilities: L_from_file:  File read error, '//file_name//', IOSTAT=',stat,kill=.TRUE.)
    If (del_f) Then
        Close(unit , STATUS = 'DELETE')
    Else
        Close(unit)
    End If
End Subroutine L_from_file

Subroutine Get_Working_Directory(dir,s)
    !Gets the current working directory, appending a slash character if supplied
    Use IFPORT, Only: $MAXPATH
    Use IFPORT, Only: GETDRIVEDIRQQ
    Use IFPORT, Only: FILE$CURDRIVE
    Implicit None
    Character($MAXPATH), Intent(Out) :: dir
    Character(1), Intent(In), Optional :: s
    Integer :: pathlen
    
    dir = FILE$CURDRIVE
    pathlen = GETDRIVEDIRQQ(dir)
    If (Present(s)) dir = Trim(dir)//s
End Subroutine Get_Working_Directory

Subroutine Make_Folder(fold)
    Use IFPORT, Only: MAKEDIRQQ
    Implicit None
    Character(*), Intent(In) :: fold
    Logical :: folder_exists

    INQUIRE(DIRECTORY = fold , EXIST = folder_exists)
    If (.NOT. folder_exists) Then  !create folder
        folder_exists = MAKEDIRQQ(fold)
        If (.NOT. folder_exists) Call Output_Message('ERROR:  Utilities: Make_Folder:  Failed to create directory: '//fold,kill=.TRUE.)
    End If
End Subroutine Make_Folder

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
    Write(*,'(A)') '  _____.(~#%&$@%#&#~)._____  '
    Write(*,*) ding
    Write(*,*)
End Subroutine Make_Boom

Subroutine Output_Message_C(message,kill)
    Implicit None
    Character(*), Intent(In) :: message
    Logical, Intent(In), Optional :: kill
    
    Write(*,'(A)') message
    Write(*,*) ding
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_C

Subroutine Output_Message_CI4(message,i,kill)
    Implicit None
    Character(*), Intent(In) :: message
    Integer(4), Intent(In) :: i
    Logical, Intent(In), Optional :: kill
    
    Write(*,'(A,I0)') message,i
    Write(*,*) ding
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CI4

Subroutine Output_Message_CI4C(message1,i,message2,kill)
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Integer(4), Intent(In) :: i
    Logical, Intent(In), Optional :: kill
    
    Write(*,'(A,I0,A)') message1,i,message2
    Write(*,*) ding
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CI4C

Subroutine Output_Message_CI8(message,i,kill)
    Implicit None
    Character(*), Intent(In) :: message
    Integer(8), Intent(In) :: i
    Logical, Intent(In), Optional :: kill
    
    Write(*,'(A,I0)') message,i
    Write(*,*) ding
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CI8

Subroutine Output_Message_CI8C(message1,i,message2,kill)
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Integer(8), Intent(In) :: i
    Logical, Intent(In), Optional :: kill
    
    Write(*,'(A,I0,A)') message1,i,message2
    Write(*,*) ding
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
    
    Write(*,'(A,F0.8)') message,r
    Write(*,*) ding
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
    
    Write(*,'(A,F0.8,A)') message1,r,message2
    Write(*,*) ding
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
    
    Write(*,'(A,F0.16)') message,r
    Write(*,*) ding
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CDSP

Subroutine Output_Message_CDPC(message1,r,message2,kill)
    Use Kinds, Only: dp
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Real(dp), Intent(In) :: r
    Logical, Intent(In), Optional :: kill
    
    Write(*,'(A,F0.16,A)') message1,r,message2
    Write(*,*) ding
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CDPC

Subroutine Output_Message_CL(message,l,kill)
    Implicit None
    Character(*), Intent(In) :: message
    Logical, Intent(In) :: l
    Logical, Intent(In), Optional :: kill
    
    Write(*,'(A,L)') message,l
    Write(*,*) ding
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CL

Subroutine Output_Message_CLC(message1,l,message2,kill)
    Implicit None
    Character(*), Intent(In) :: message1,message2
    Logical, Intent(In) :: l
    Logical, Intent(In), Optional :: kill
    
    Write(*,'(A,L,A)') message1,l,message2
    Write(*,*) ding
    If (Present(kill)) Then
        If (kill) ERROR STOP
    End If
End Subroutine Output_Message_CLC

End Module FileIO_Utilities

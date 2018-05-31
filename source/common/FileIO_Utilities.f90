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
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: DP_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: DP_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: DP_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: DP_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: DP_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) r
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: DP_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Close(unit)
End Subroutine DP_2Darray_to_file

Subroutine I4_to_file(i,file_name)
    Implicit None
    Integer(4), Intent(In) :: i
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I4_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I4_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Close(unit)
End Subroutine I4_to_file

Subroutine I4_1Darray_to_file(i,file_name)
    Implicit None
    Integer(4), Intent(In) :: i(:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I4_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I4_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Close(unit)
End Subroutine I4_1Darray_to_file

Subroutine I4_2Darray_to_file(i,file_name)
    Implicit None
    Integer(4), Intent(In) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I4_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I4_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Close(unit)
End Subroutine I4_2Darray_to_file

Subroutine I8_to_file(i,file_name)
    Implicit None
    Integer(8), Intent(In) :: i
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I8_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I8_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Close(unit)
End Subroutine I8_to_file

Subroutine I8_1Darray_to_file(i,file_name)
    Implicit None
    Integer(8), Intent(In) :: i(:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I8_1Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I8_1Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Close(unit)
End Subroutine I8_1Darray_to_file

Subroutine I8_2Darray_to_file(i,file_name)
    Implicit None
    Integer(8), Intent(In) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat
    
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I8_2Darray_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) i
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I8_2Darray_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    
    If (Len(C) .GT. $MAXPATH) Then
        Print *,'ERROR:  Utilities: C_to_file:  Write string is longer than $MAXPATH'
        ERROR STOP
    End If
    Cmax = C
    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: C_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) Cmax
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: C_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Close(unit)
End Subroutine C_to_file

Subroutine L_to_file(L,file_name)
    Implicit None
    Logical, Intent(In) :: L
    Character(*), Intent(In) :: file_name
    Integer :: unit
    Integer :: stat

    Call Open_for_Var_to_File(file_name,unit,stat)
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: L_to_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Write(unit , IOSTAT = stat) L
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: L_to_file:  File write error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: DP_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: DP_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: DP_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: DP_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: DP_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit , IOSTAT = stat) r
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: DP_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    Implicit None
    Integer(4), Intent(Out) :: i
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I4_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit) i
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: I4_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    Implicit None
    Integer(4), Intent(Out) :: i(:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I4_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: I4_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    Implicit None
    Integer(4), Intent(Out) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I4_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: I4_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    Implicit None
    Integer(8), Intent(Out) :: i
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I8_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit) i
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: I8_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    Implicit None
    Integer(8), Intent(Out) :: i(:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I8_1Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: I8_1Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    Implicit None
    Integer(8), Intent(Out) :: i(:,:)
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: I8_2Darray_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit , IOSTAT = stat) i
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: I8_2Darray_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
    Use IFPORT, Only: $MAXPATH
    Implicit None
    Character(*), Intent(Out) :: C
    Character(*), Intent(In) :: file_name
    Logical, Intent(In), Optional :: delete_file
    Integer :: unit
    Integer :: stat
    Character($MAXPATH) :: Cmax
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: C_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit , IOSTAT = stat) Cmax
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: C_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    If (Len(Trim(Cmax)) .GT. Len(C)) Then
        Print *,'ERROR:  Utilities: C_from_file:  Read string is longer than requested string'
        ERROR STOP
    End If
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
    
    If (Present(delete_file)) Then
        Call Open_for_Var_from_File(file_name,delete_file,unit,stat)
    Else
        Call Open_for_Var_from_File(file_name,.FALSE.,unit,stat)
    End If
    If (stat .NE. 0) Then
        Print *,'ERROR:  Utilities: L_from_file:  File open error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
    Read(unit , IOSTAT = stat) L
    If (stat .GT. 0) Then
        Print *,'ERROR:  Utilities: L_from_file:  File read error, '//file_name//', IOSTAT=',stat
        ERROR STOP
    End If
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
        If (.NOT. folder_exists) Then
            Print *,'ERROR:  Utilities: Make_Folder:  Failed to create directory: '//fold
            ERROR STOP
        End If
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
    Write(*,*)
End Subroutine Make_Boom
    
End Module FileIO_Utilities

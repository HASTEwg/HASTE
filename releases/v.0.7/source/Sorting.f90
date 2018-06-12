Module Sorting

    Implicit None
    Private
    Public:: Quick_Sort
    Public:: Union_Sort
    
    Interface Quick_Sort
        Module Procedure Quick_Sort_Direction
        Module Procedure Quick_Sort_Default
    End Interface Quick_Sort
        
Contains

Subroutine Union_Sort(A, iMax, aMin, aMax, Up)
        ! Creates the sorted union of A
        !   Optionally excluding values below aMin
        !   Optionally excluding values above aMax
        ! Default sort order is increasing,
        !   Optionally, Up specifies sort order
        ! After the call, A(LBound(A):iMax) contains
        !   the union of the accceptable values, 
        !   and A(iMax+1:UBound(A)) is meaningless
        ! Cost is dominated by quicksort, hence order of n*Log(n)    
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(InOut):: A(:)
    Integer, Intent(Out):: iMax
    Real(dp), Intent(In), Optional:: aMin, aMax
    Logical, Intent(In), Optional:: Up
    Integer:: iMin
    Real(dp):: a0, a1
    Integer:: i, j
    iMin = LBound(A,1)
    If (Present(aMin)) then
        a0 = aMin
    Else
        a0 = - Huge(A)  ! eliminate no entries for being too small
    End if
    If (Present(aMax)) then
        a1 = aMax
    Else
        a1 = Huge(A)    ! eliminate no entries for being too large
    End if
    Call Quick_Sort(A)
    i = iMin
    ! move past values that are too small
    If (Present(aMin)) then
        Do
            If (a(i) < aMin) then
                i = i + 1
            Else
                Exit
            End if
        End do
    End if
    ! now i is index of first desired entry
    ! copy it into the first element of A
    A(iMin) = A(i)
    ! Copy the acceptable values into place after it
    iMax = iMin
    Do j = i+1, UBound(A,1)
        If (A(j) > a1) then
            Exit                    ! remaining entries are too large, union is complete
        Else if (A(j) /= A(iMax)) then ! Copy it down into the union
            iMax = iMax + 1
            A(iMax) = A(j)       
        End if
    End do
    If (Present(Up)) then
        If (.not. up) then
            A(iMin:iMax) = A(iMax:iMin:-1)
        End if
    End if
End Subroutine Union_Sort

Subroutine Quick_Sort_Direction(A, Up)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(InOut):: A(:)
    Logical, Intent(In):: Up
    If (.Not. Up) A = -A
    Call Quick_Sort(A)
    If (.Not. Up) A = -A
End Subroutine Quick_Sort_Direction

Recursive Subroutine Quick_Sort_Default(A)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(InOut):: A(:)
    Integer :: iq
    If(Size(A) > 1) then
        Call Partition(A, iq)
        Call Quick_Sort(A(:iq-1))
        Call Quick_Sort(A(iq:))
    End if
End Subroutine Quick_Sort_Default

Subroutine Partition(A, marker)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(InOut) :: A(:)
    Integer, Intent(Out) :: marker
    Integer :: i, j
    Real(dp) :: x      ! pivot point
    x = A(1)
    i = 0
    j = size(A) + 1
    do
     j = j - 1
     do
        if ( A(j) <= x ) exit
        j = j - 1
     end do
     i = i + 1
     do
        if ( A(i) >= x ) exit
        i = i+1
     end do
     if (i < j) then
        Call Swap(A(i), A(j))
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do
End Subroutine Partition

Subroutine Swap(x,y)
    Use Kinds, Only: dp
    Implicit None
    Real(dp), Intent(InOut):: x, y
    Real(dp):: holder
    holder = x
    x = y
    y = holder
End Subroutine Swap

End Module Sorting


    
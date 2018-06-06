Module Particle_Piles

    Use Kinds, Only: dp
    Implicit None
    Private
    Public :: Pile_Type

    Type Pile_Particle_Type
        Real(dp) :: r(1:3)  !position
        Real(dp) :: OmegaHat(1:3)  !direction
        Real(dp) :: E  !energy
        Real(dp) :: w  !weight
        Real(dp) :: t  !time of emission
        Type(Pile_Particle_Type), Pointer :: up => Null()
        Type(Pile_Particle_Type), Pointer :: dn => Null()
    Contains
        Procedure, Pass :: fill => Fill_Pile_Particle
        Procedure, Pass :: empty => Empty_Pile_Particle
    End Type
    
    Type Pile_Type
        Type(Pile_Particle_Type), Pointer :: head => Null()
        Type(Pile_Particle_Type), Pointer :: tail => Null()
        Type(Pile_Particle_Type), Pointer :: ptr => Null()
        Integer :: num_piled = 0  !tracks the current number of piled particles
        Integer :: max_piled = 0  !tracks the maximum extent of the pile
        Logical :: First_Time = .TRUE.
    Contains
        Procedure, Pass :: Put => Put_on_Pile
        Procedure, Pass :: Get => Get_from_Pile
    End Type
      
Contains

Function Setup_Particle_Pile() Result(p)
    Implicit None
    Type(Pile_Type) :: p
    
    Allocate(p%head)
    Nullify(p%head%up)
    p%tail => p%head
    Nullify(p%tail%dn)
    Nullify(p%ptr)
    p%First_Time = .FALSE.
    p%num_piled = 0
    p%max_piled = 1
End Function Setup_Particle_Pile

Subroutine Put_On_Pile(pile,r,OmegaHat,E,w,t)
    Use Kinds, Only: dp
    Implicit None
    Class(Pile_Type), Intent(InOut) :: pile  !this variable is automatically passed and need not be explicit in subroutine calls
    Real(dp), Intent(In) :: r(1:3)  !position
    Real(dp), Intent(In) :: OmegaHat(1:3)  !direction
    Real(dp), Intent(In) :: E  !energy
    Real(dp), Intent(In) :: w  !weight
    Real(dp), Intent(In) :: t  !time of emission
    
    If (.NOT. Associated(pile%ptr)) Then  ! There are no particles on the pile
        ! Put the new particle in the ptr/head of the pile
        Call pile%head%fill(r,OmegaHat,E,w,t)
        pile%ptr => pile%head
        pile%num_piled = pile%num_piled + 1
        Return
    Else If (.NOT. Associated(pile%ptr%dn)) Then  ! There is not pile space allocated for new values
        ! Create new pile space
        Allocate(pile%ptr%dn)
        pile%tail    => pile%ptr%dn
        pile%tail%up => pile%ptr
        Nullify(pile%tail%dn)
        pile%max_piled = pile%max_piled + 1
    End If
    ! Point the pointer at the next empty pile space
    pile%ptr => pile%ptr%dn
    ! Put the new particle in that empty space
    Call pile%ptr%fill(r,OmegaHat,E,w,t)
    pile%num_piled = pile%num_piled + 1
End Subroutine Put_On_Pile

Subroutine Fill_Pile_Particle(p,r,OmegaHat,E,w,t)
    Use Kinds, Only: dp
    Implicit None
    Class(Pile_Particle_Type), Intent(InOut) :: p
    Real(dp), Intent(In) :: r(1:3)  !position
    Real(dp), Intent(In) :: OmegaHat(1:3)  !direction
    Real(dp), Intent(In) :: E  !energy
    Real(dp), Intent(In) :: w  !weight
    Real(dp), Intent(In) :: t  !time of emission

    p%r = r
    p%OmegaHat = OmegaHat
    p%E = E
    p%w = w
    p%t = t
End Subroutine Fill_Pile_Particle

Function Get_From_Pile(pile,r,OmegaHat,E,w,t) Result(particle_on_pile)
    Use Kinds, Only: dp
    Implicit None
    Logical :: particle_on_pile
    Class(Pile_Type), Intent(InOut) :: pile !this variable is automatically passed and need not be explicit in function calls
    Real(dp), Intent(Out) :: r(1:3)  !position
    Real(dp), Intent(Out) :: OmegaHat(1:3)  !direction
    Real(dp), Intent(Out) :: E  !energy
    Real(dp), Intent(Out) :: w  !weight
    Real(dp), Intent(Out) :: t  !time of emission

    If (Associated(pile%ptr)) Then  ! There is a particle on the pile, CONTINUE
        particle_on_pile = .TRUE.
    Else  ! The pile is empty, RETURN
        particle_on_pile = .FALSE.
        Return
    End If
    ! Get the particle from the bottom of the pile
    Call pile%ptr%empty(r,OmegaHat,E,w,t)
    ! Point ptr to the new pile bottom
    pile%ptr => pile%ptr%up
    pile%num_piled = pile%num_piled - 1
End Function Get_From_pile

Subroutine Empty_Pile_Particle(p,r,OmegaHat,E,w,t)
    Use Kinds, Only: dp
    Implicit None
    Class(Pile_Particle_Type), Intent(InOut) :: p
    Real(dp), Intent(Out) :: r(1:3)  !position
    Real(dp), Intent(Out) :: OmegaHat(1:3)  !direction
    Real(dp), Intent(Out) :: E  !energy
    Real(dp), Intent(Out) :: w  !weight
    Real(dp), Intent(Out) :: t  !time of emission

    !get values
    r = p%r
    OmegaHat = p%OmegaHat
    E = p%E
    w = p%w
    t = p%t
    !write nonsense vaues into the now empty particle
    p%r = 0._dp
    p%OmegaHat = 0._dp
    p%E = -1._dp
    p%w = 0._dp
    p%t = 0._dp
End Subroutine Empty_Pile_Particle

End Module Particle_Piles

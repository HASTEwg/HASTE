Program LPadjoint

Use Kinds, Only: dp
Use Satellite_Motion, Only: Satellite_Position_Type
Use Satellite_Motion, Only: Initialize_Satellite_Motion
Use Random_Numbers, Only: RNG_Type
Use Neutron_Utilities, Only: Neutron_Speed
Use Neutron_Utilities, Only: Neutron_Energy
Use Find_Trajectory, Only: Prev_Event_Trajectory
Use Diverge_Approx, Only: Div_Fact_by_shooting
Use Random_Directions, Only: Isotropic_Omega_Hat
Use Global, Only: Pi,TwoPi,halfPi
Use Global, Only: r2deg
Use Global, Only: Z_hat,X_hat,Y_hat
Use Global, Only: rot_moon
Use Utilities, Only: Unit_Vector
Use Utilities, Only: Vector_Length
Use Utilities, Only: Linear_spaces
Use Utilities, Only: Log_spaces
Use Statistics, Only: Std_Err
Use FileIO_Utilities, Only: cr => creturn
Use FileIO_Utilities, Only: Wait
Use FileIO_Utilities, Only: slash
Use FileIO_Utilities, Only: Read_List
Use Astro_Utilities, Only: Lambert_Gooding

Implicit None

Type(RNG_Type) :: RNG
Integer :: n_trials !this many tallies will be accumulated in the grid
! List of energies at which to create maps
Integer :: n_En = 15
Real(dp), Allocatable :: En_list(:) ![keV] neutron energy at arrival in satellite frame
Real(dp) :: En  ![keV] current detection energy of interest
! Surface grids
Integer :: n_bin_Cos
Integer :: n_bin_En
Real(dp), Allocatable :: Cos_bin_grid(:)
Real(dp), Allocatable :: En_bin_grid(:)
Type :: tally_box
    Real(dp), Allocatable :: f(:,:) !1:6,1:n_box_Cos
    Integer, Allocatable :: c(:) !1:n_box_Cos
End Type
Type :: Surface_box
    Logical :: hit
    Real(dp) :: f(1:4)
    Integer :: c
    Integer :: x_size
    Integer, Allocatable :: iE(:)
    Type(tally_box), Allocatable :: x(:)
End Type
! Tally grid
Integer, Parameter :: n_lat_bins = 36 , n_lon_bins = 72
Type(Surface_box) :: f(1:n_lat_bins,1:n_lon_bins)
Integer, Allocatable :: swap_iE(:)
Type(tally_box), Allocatable :: swap_x(:)
! Satellite data and variables
Type(Satellite_Position_Type) :: sat
Real(dp) :: r_sat(1:3),v_sat(1:3)  !position and velocity of the satellite at t2
Real(dp) :: Omega_hat2(1:3) !direction of neutron arrival in satellite frame
! Trajectory & Divergence variables
Real(dp) :: t2 !time of intercept
Logical, Parameter :: Gravity = .TRUE.  !flag to set gravity on or off
Logical :: Found  !flag for whether a trajectory was found
Real(dp) :: r1(1:3),v1(1:3),tof !position,velocity, time of flight defining flight from the surface of the central body
Real(dp) :: v2(1:3) !velocity at the conclusion of the trajectory defined byr1,v1,tof
Real(dp) :: DFact !divergence factor for the intercepting flight from emission to intercept
Real(dp) :: Ee,zeta  !emission energy and emission polar angle cosine
! 
Real(dp) :: DEC,HA ![rad] declination and hour angle (used in place of lon/lat)
Integer :: dec_bin,ha_bin !computed bins for dec and ha
Integer :: En_bin,zeta_bin,b !computed bins for emission energy and emission angle cosine
Integer :: map_unit,bound_unit  !file unit number
Integer :: h !counter for number of histories
Integer :: h_miss !counter for number of histories started but not propagated because a trajectory was not found
Integer :: e !counter for energy points, runs 1:n_En
Integer :: i,j,k,l,m  !counters
Character(2) :: e_char !character representation of energy index for file naming
Character(2) :: n_En_char !character representation of total number of energy points, for screen updates
Character(9) :: t2_char !character representation of time of intercept for file naming
Real(dp) :: Dfact_err,tof_err
Real(dp) :: lat,lon
Character(80) :: cmd1,cmd2
Logical :: screen_progress
Logical :: skip_next_cmd
Integer :: n_scratch
Integer :: En_bin_start
Real(dp), Allocatable :: scratch(:)
# if CAF
 Integer :: next_e[*]
 Character(80), Allocatable :: stat_lines(:)[:]
 Character(80) :: new_stat_line
 Logical :: En_finished(:)[:]
# endif

! Set default t2 and n_trials, defaults are for testing and can be overridden by command line
n_trials = 100000
!t2 = 45900._dp !time of intercept
t2 = 84420._dp !time of intercept
! Set default to send progress updates to screen
screen_progress = .TRUE.
!Get command line arguments
n_En = -1
skip_next_cmd = .FALSE.
b = COMMAND_ARGUMENT_COUNT()
If ( b .NE. 0 ) Then
    Do i = 1,b
        If (skip_next_cmd) Then !the i-th command was already read as the second element of an argument pair
            skip_next_cmd = .FALSE.
            Cycle
        End If
        Call GET_COMMAND_ARGUMENT(i,cmd1)
        Select Case (Trim(cmd1))
            Case ('t2','T2','t','T')
                Call GET_COMMAND_ARGUMENT(i+1,cmd2)
                Read(cmd2,*) t2
                skip_next_cmd = .TRUE.
            Case ('n','N','n_trials')
                Call GET_COMMAND_ARGUMENT(i+1,cmd2)
                Read(cmd2,*) n_trials
                skip_next_cmd = .TRUE.
            Case ('e','E','En','Ed')
                n_En = 1
                Allocate(En_list(1:1))
                Call GET_COMMAND_ARGUMENT(i+1,cmd2)
                Read(cmd2,*) En_list(1)
                skip_next_cmd = .TRUE.
            Case ('quiet','QUIET','Quiet','q','Q')
                screen_progress = .FALSE.
            Case Default
                Write (*,'(A)') 'Invalid command argument specified. Aborting.'
        End Select
    End Do
End If
! Read in detection energies
If (n_EN .LT. 1) Call Read_List('grid'//slash//'Ed.txt',n_En,En_list)
En_list = En_list * 1000._dp  !convert to keV
! Read in angle cosine bin boundaries
Call Read_List('grid'//slash//'cos_grid.txt',n_scratch,scratch)
n_bin_Cos = n_scratch - 1
Allocate(Cos_bin_grid(0:n_bin_Cos))
Cos_bin_grid(0:n_bin_Cos) = scratch(1:n_scratch)
Deallocate(scratch)
! Read in energy bin boundaries
Call Read_List('grid'//slash//'en_grid.txt',n_scratch,scratch)
n_bin_En = n_scratch - 1
Allocate(En_bin_grid(0:n_bin_En))
En_bin_grid(0:n_bin_En) = scratch(1:n_scratch)
Deallocate(scratch)
En_bin_grid = En_bin_grid * 1000._dp  !convert to keV
! Initialize satellite position & velocity
Call Initialize_Satellite_Motion('','Conic_tab ',sat)
Call sat%R_and_V(t2,r_sat,v_sat)
! Initialize Random Number Generator
# if CAF
 Call RNG%Initialize(seed = 7777777 , thread = this_image())
# else
 Call RNG%Initialize(seed = 7777777)
# endif
! Write detection energies and energy-angle grid boundaries to file
# if CAF
 If (this_image().EQ.1) Then
# endif
    !write detection energies
    Open(NEWUNIT = bound_unit , FILE = 'LPemissionMap_Ed_grid.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Do i = 1,n_En
        Write(bound_unit,'(I5,2ES25.16E3)') i,En_list(i)
    End Do
    Close(bound_unit)
    !write angle cosine grid boundaries
    Open(NEWUNIT = bound_unit , FILE = 'LPemissionMap_Cos_grid.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Do i = 1,n_bin_Cos
        Write(bound_unit,'(I5,2ES25.16E3)') i,Cos_bin_grid(i-1),Cos_bin_grid(i)
    End Do
    Close(bound_unit)
    !write energy grid boundaries
    Open(NEWUNIT = bound_unit , FILE = 'LPemissionMap_En_grid.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Do i = 1,n_bin_En
        Write(bound_unit,'(I5,2ES25.16E3)') i,En_bin_grid(i-1)/1000._dp,En_bin_grid(i)/1000._dp !MeV
    End Do
    Close(bound_unit)
    !write surface box boundaries
    Open(NEWUNIT = bound_unit , FILE = 'LPemissionMap_LatLon_grid.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Do i = 1,n_lat_bins
        Do j = 1,n_lon_bins
            !compute declination-hourangle for this box
            DEC = Real(2*i-1,dp) * halfPi / Real(n_lat_bins,dp)
            HA = Real(2*j-1,dp) * Pi / Real(n_lon_bins,dp)
            !compute lat-lon for this surface box
            lat = halfPi - DEC
            lon = -(HA - Pi) - halfPi
            If (Abs(lon) .GT. Pi) lon = lon + SIGN(TwoPi,-lon)
            !write each surface box boundaries to file
            Write(bound_unit,'(2(I5,2F8.1))') i , r2deg*lat-2.5_dp , r2deg*lat+2.5_dp , &
                                            & j , r2deg*lon-2.5_dp , r2deg*lon+2.5_dp
        End Do
    End Do
    Close(bound_unit)
# if CAF
 End If
# endif

Write(n_En_char,'(I2.2)') n_En
Write(t2_char,'(I9.9)') NINT(t2)
# if CAF
 Allocate(stat_lines(1:n_En)[*])
 Do e = 1,n_En
    Write(e_char,'(I2.2)') e
    stat_lines(e) = 'En '//e_char//'/'//n_En_char//'   *.**% (  *.**% hits) Lunar F: *.********E+***'
 End Do
 Allocate(En_finished(1:n_En)[*])
 En_finished = .FALSE.
 next_e = 1
 SYNC ALL
 Do
    CRITICAL
        e = next_e[1]
        next_e[1] = next_e[1] + 1
    END CRITICAL
    If (e .GT. n_En) Exit
# else
 Do e = 1,n_En
# endif
    En = En_list(e)
    En_bin_start = n_bin_En + 5
    Do i = 1,n_bin_En
        If (En.GE.En_bin_grid(i-1) .AND. En.LT.En_bin_grid(i)) Then
            En_bin_start = i
            Exit
        End If
    End Do
    !initialize tallies for this energy
    Do j = 1,n_lon_bins
        Do i = 1,n_lat_bins
            f(i,j)%hit = .FALSE.
            f(i,j)%f = 0._dp
            f(i,j)%c = 0
            f(i,j)%x_size = 0
            If ( Allocated(f(i,j)%iE) ) Deallocate(f(i,j)%iE)
            If ( Allocated(f(i,j)%x) ) Deallocate(f(i,j)%x)
        End Do
    End Do
    !initialize output files for this energy
    Write(e_char,'(I2.2)') e
    Open(NEWUNIT = map_unit , FILE = 'LPemissionMap_'//t2_char//'_e'//e_char//'.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Write( map_unit , '(2ES25.16E3)' , ADVANCE = 'NO' ) En/1000._dp , t2
    h = 0
    h_miss = 0
    Do
        !choose a random direction of arrival at the detector
        Omega_hat2 = Isotropic_Omega_Hat(RNG)
        !check if an emission is possible at the surface at this energy to result in this rendezvous
        Call Prev_Event_Trajectory(sat, Gravity, t2, -Omega_hat2*Neutron_Speed(En), Found, r1, v1, tof)
        If (found) Then
            h = h + 1
            !compute the declination and right-ascension indexes for this emission point
            DEC = ACOS( Dot_Product(Z_hat,Unit_Vector(r1)) )
            HA = Atan2( Dot_Product(Unit_Vector(r1),X_hat) , Dot_Product(Unit_Vector(r1),Y_hat) )
            HA = HA + tof*rot_moon  !correct for rotation of the lunar surface during tof
            If (HA .LT. 0._dp) HA = HA + TwoPi
            dec_bin = 1 + Floor(Real(n_lat_bins,dp) * DEC / Pi)
            ha_bin = 1 + Floor(Real(n_lon_bins,dp) * HA / TwoPi)
            !Compute divergence
            v2 = -Omega_hat2*Neutron_Speed(En) + v_sat
            DFact = Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)
            !Find emission energy bin
            Ee = Neutron_Energy(v1)
            En_bin = 0
            Do j = En_bin_start,n_bin_En
                If (Ee.GE.En_bin_grid(j-1) .AND. Ee.LT.En_bin_grid(j)) Then
                    En_bin = j
                    Exit
                End If
            End Do
            !Find emission direction bin
            zeta = Dot_Product(Unit_Vector(r1),Unit_Vector(v1))
            zeta_bin = 0
            Do j = 1,n_bin_Cos
                If (zeta.LE.Cos_bin_grid(j-1) .AND. zeta.GT.Cos_bin_grid(j)) Then
                    zeta_bin = j
                    Exit
                End If
            End Do
            !Check if this surface box has been initialized
            b = 0
            If (.NOT.f(dec_bin,ha_bin)%hit) Then !initialize the bin
                f(dec_bin,ha_bin)%hit = .TRUE.
                f(dec_bin,ha_bin)%x_size = 1
                Allocate(f(dec_bin,ha_bin)%x(1:1))
                Allocate(f(dec_bin,ha_bin)%iE(1:1))
                f(dec_bin,ha_bin)%iE = En_bin
                Allocate(f(dec_bin,ha_bin)%x(1)%f(1:6,1:n_bin_Cos))
                f(dec_bin,ha_bin)%x(1)%f = 0._dp
                Allocate(f(dec_bin,ha_bin)%x(1)%c(1:n_bin_Cos))
                f(dec_bin,ha_bin)%x(1)%c = 0
                b = 1
            Else !surface box is initialized, check that this energy is allocated, if not, initialize it
                If ( Any(f(dec_bin,ha_bin)%iE .EQ. En_bin) ) Then !E bin is already present, find its place in the list
                    Do k = 1,f(dec_bin,ha_bin)%x_size
                        If (En_bin .EQ. f(dec_bin,ha_bin)%iE(k)) Then
                            b = k
                            Exit
                        End If
                    End Do
                    !b holds the index of the correct bin
                Else !insert and initialize E bin
                    Do k = 1,f(dec_bin,ha_bin)%x_size
                        If (En_bin .LT. f(dec_bin,ha_bin)%iE(k)) Then
                            b = k
                            Exit
                        End If
                        If (k .EQ. f(dec_bin,ha_bin)%x_size) Then
                            b = k + 1
                            Exit
                        End If
                    End Do
                    !b holds the index at which to insert the new energy bin
                    !copy current lists into temporary storage
                    Allocate(swap_iE(1:f(dec_bin,ha_bin)%x_size))
                    swap_iE = f(dec_bin,ha_bin)%iE
                    Allocate(swap_x(1:f(dec_bin,ha_bin)%x_size))
                    Do l = 1,f(dec_bin,ha_bin)%x_size
                        Allocate(swap_x(l)%c(1:n_bin_Cos))
                        swap_x(l)%c = f(dec_bin,ha_bin)%x(l)%c
                        Allocate(swap_x(l)%f(1:6,1:n_bin_Cos))
                        swap_x(l)%f = f(dec_bin,ha_bin)%x(l)%f
                    End Do
                    !deallocate primary lists
                    Deallocate(f(dec_bin,ha_bin)%iE)
                    Deallocate(f(dec_bin,ha_bin)%x)
                    !reallocate one element longer and copy lists back, inserting the new value at k
                    Allocate(f(dec_bin,ha_bin)%iE(1:f(dec_bin,ha_bin)%x_size+1))
                    f(dec_bin,ha_bin)%iE(:) = -1
                    Allocate(f(dec_bin,ha_bin)%x(1:f(dec_bin,ha_bin)%x_size+1))
                    Allocate(f(dec_bin,ha_bin)%x(b)%c(1:n_bin_Cos))
                    f(dec_bin,ha_bin)%x(b)%c = 0
                    Allocate(f(dec_bin,ha_bin)%x(b)%f(1:6,1:n_bin_Cos))
                    f(dec_bin,ha_bin)%x(b)%f = 0._dp
                    m = 0
                    Do l = 1,f(dec_bin,ha_bin)%x_size+1
                        If (l .EQ. b) Then
                            f(dec_bin,ha_bin)%iE(l) = En_bin
                            f(dec_bin,ha_bin)%x(l)%c = 0
                            f(dec_bin,ha_bin)%x(l)%f = 0._dp
                            m = 1
                        Else
                            f(dec_bin,ha_bin)%iE(l) = swap_iE(l-m)
                            f(dec_bin,ha_bin)%x(l)%c = swap_x(l-m)%c
                            f(dec_bin,ha_bin)%x(l)%f = swap_x(l-m)%f
                        End If
                    End Do
                    !increment x_size and cleanup
                    f(dec_bin,ha_bin)%x_size = f(dec_bin,ha_bin)%x_size + 1
                    Deallocate(swap_iE)
                    Deallocate(swap_x)
                End If
            End If
            !Increment the tally counter for this surface box
            f(dec_bin,ha_bin)%f(:) = f(dec_bin,ha_bin)%f(:) + (/ Dfact , Dfact**2 , Dfact*tof , (Dfact*tof)**2 /)
            f(dec_bin,ha_bin)%c = f(dec_bin,ha_bin)%c + 1
            !tally counter, intensity (divergence factor), and tof (weighted by divergence factor)
            f(dec_bin,ha_bin)%x(b)%c(zeta_bin) = f(dec_bin,ha_bin)%x(b)%c(zeta_bin) + 1
            f(dec_bin,ha_bin)%x(b)%f(:,zeta_bin) = f(dec_bin,ha_bin)%x(b)%f(:,zeta_bin) + & 
                                                 & (/ Dfact , Dfact**2 , tof , tof**2 , Dfact*tof , Dfact*(tof**2) /)
        Else
            h_miss = h_miss + 1
        End If
        If (screen_progress) Then
#           if CAF
             If (MOD(h,10000).EQ.0) Then
                Write( new_stat_line,'(A,I2,A,F6.2,A,F6.2,A,ES15.8E3)') & 
                     & 'En ',e,'/'//n_En_char//' ',100._dp*Real(h,dp)/Real(n_trials,dp),'% (', &
                     & 100._dp*Real(h,dp)/Real(h+h_miss,dp),'% hits) Lunar F: ',Sum(f(:,:)%f(1))/Real(h+h_miss,dp)
                If (this_image() .EQ. 1) Then
                    stat_lines(e) = new_stat_line
                    Do j = 1,n_En
                        Write(*,'(A)') stat_lines(j)
                    End Do
                    Write(*,'(A)',ADVANCE='NO') ACHAR(27)//'['//n_En_char//'F'
                Else
                    stat_lines(e)[1] = new_stat_line
                End If
             End If
#           else
             If (MOD(h,10000).EQ.0) Then
                Write( * , '(A,I2,A,F6.2,A,F6.2,A,ES15.8E3,A)' , ADVANCE = 'NO' ) & 
                     & 'En ',e,'/'//n_En_char//' ',100._dp*Real(h,dp)/Real(n_trials,dp),'% (', &
                     & 100._dp*Real(h,dp)/Real(h+h_miss,dp),'% hits) Lunar F: ',Sum(f(:,:)%f(1))/Real(h+h_miss,dp),cr
             End If
#           endif
        End If
        If (h .GE. n_trials) Exit
    End Do
    Write(map_unit,'(2I12)') h,h_miss
    Do i = 1,n_lat_bins
        Do j = 1,n_lon_bins
            If (f(i,j)%hit) Then
                !compute position vector at the lat-lon center of this surface box
                DEC = Real(2*i-1,dp) * halfPi / Real(n_lat_bins,dp)
                HA = Real(2*j-1,dp) * Pi / Real(n_lon_bins,dp)
                r1 = Cos(DEC) * Z_hat + Sqrt(1._dp - Cos(DEC)**2) * (Cos(HA) * Y_hat + Sin(HA) * X_hat)
                !walk through the energy-angle lists for this surface box, writing each result to file
                Do k = 1,f(i,j)%x_size
                    Do l = 1,n_bin_Cos
                        If (f(i,j)%x(k)%c(l) .EQ. 0) Cycle
                        !intensity
                        Dfact = f(i,j)%x(k)%f(1,l) / Real(h+h_miss,dp)
                        Dfact_err = Std_err( h+h_miss , f(i,j)%x(k)%f(1,l) , f(i,j)%x(k)%f(2,l) )
                        !tof
                        tof = f(i,j)%x(k)%f(5,l) / f(i,j)%x(k)%f(1,l)
                        tof_err = Std_err( f(i,j)%x(k)%c(l) , f(i,j)%x(k)%f(3,l) , f(i,j)%x(k)%f(4,l) )
                        !Properties of a trajectory matching this tof and satellite position
                        Call Lambert_Gooding(r1,r_sat,tof,v1,v2)
                        !Write to file
                        Write( map_unit,'(4I5,I12,10ES25.16E3)') &
                             & i,j,f(i,j)%iE(k),l, & 
                             & f(i,j)%x(k)%c(l) , & 
                             & Dfact , Dfact_err , & 
                             & tof , tof_err , & 
                             & Neutron_Energy(v1) / 1000._dp , & !MeV
                             & Dot_Product(Unit_Vector(r1),Unit_Vector(v1)) , &
                             & Unit_Vector(v2 + v_sat) , &
                             & Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)
                    End Do
                End Do
            End If
        End Do
    End Do
    Close(map_unit)
    If (screen_Progress) Then
#       if CAF
         En_finished(e)[1] = .TRUE.
#       else
         Write(*,*)
#       endif
    End If
End Do
# if CAF
 If (screen_progress) Then
    If (this_image() .EQ. 1) Then
        Do
            If ( All(En_finished) ) Exit
            Call Wait(100)
            Do i = 1,n_En
            Write(*,'(A)') stat_lines(i)
            End Do
            Write(*,'(A)',ADVANCE='NO') ACHAR(27)//'['//n_En_char//'F'
        End Do
    End If
    SYNC ALL
    If (this_image() .EQ. 1) Then
        Do i = 1,n_En
            Write(*,'(A)') stat_lines(i)
        End Do
        Write(*,*)
    End If
 End If
# endif

End Program

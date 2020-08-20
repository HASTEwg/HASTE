Program LPadjoint

Use Kinds, Only: dp
Use Satellite_Motion, Only: Satellite_Position_Type
Use Satellite_Motion, Only: Initialize_Satellite_Motion
Use Random_Numbers, Only: RNG_Type
Use Detectors, Only: Grid_info_type
Use Detectors, Only: Define_Grid_Info
Use Neutron_Utilities, Only: Neutron_Speed
Use Neutron_Utilities, Only: Neutron_Energy
Use Find_Trajectory, Only: Prev_Event_Trajectory
Use Diverge_Approx, Only: Div_Fact_by_shooting
Use Neutron_Scatter, Only: Scattered_Angles
Use Random_Directions, Only: Isotropic_Omega_Hat
Use Global, Only: Pi,TwoPi,halfPi
Use Global, Only: r2deg
Use Global, Only: Z_hat,X_hat,Y_hat
Use Global, Only: n_life
Use Utilities, Only: Unit_Vector
Use Utilities, Only: Cross_Product
Use Utilities, Only: Vector_Length
Use Utilities, Only: Linear_spaces
Use Utilities, Only: Log_spaces
Use Statistics, Only: Std_Err
Use FileIO_Utilities, Only: cr => creturn
Use Astro_Utilities, Only: Lambert_Gooding

Implicit None

Type(RNG_Type) :: RNG
Integer, Parameter :: n_trials = 10000000 !this many tallies will be accumulated in the grid
! List of energies at which to create maps
Integer, Parameter :: n_En = 15
Real(dp), Parameter :: En(1:n_En) = & ![keV] neutron energy at arrival in satellite frame
                                    & 1000._dp * (/ 1.e-8_dp,   & 
                                                  & 2.5e-8_dp,  & 
                                                  & 1.e-7_dp,   & 
                                                  & 2.e-7_dp,   & 
                                                  & 3.e-7_dp,   & 
                                                  & 4.e-7_dp,   & 
                                                  & 5.e-7_dp,   & 
                                                  & 6.e-7_dp,   & 
                                                  & 8.e-7_dp,   & 
                                                  & 1.e-6_dp,   & 
                                                  & 3.16e-6_dp, & 
                                                  & 1.e-5_dp,   & 
                                                  & 1.e-4_dp,   & 
                                                  & 1.e-3_dp,   & 
                                                  & 1.e-2_dp    /)  ![keV]
! Surface grids
Integer, Parameter :: n_box_Cos = 12
Integer, Parameter :: n_box_En = 15
Real(dp) :: Cos_box_grid(0:n_box_Cos)
Real(dp) :: En_box_grid(0:n_En*n_box_En)
Type :: tally_box
    Real(dp) :: f(1:4,1:n_box_Cos)
    Integer :: c(1:n_box_Cos)
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
! Time of intercept/detection
Real(dp), Parameter :: t2 = 45900._dp !time of intercept
!Real(dp), Parameter :: t2 = 84420._dp !time of intercept
! Satellite data and variables
Type(Satellite_Position_Type) :: sat
Real(dp) :: r_sat(1:3),v_sat(1:3)  !position and velocity of the satellite at t2
Real(dp) :: Omega_hat2(1:3) !direction of neutron arrival in satellite frame
! Trajectory & Divergence variables
Logical, Parameter :: Gravity = .TRUE.  !flag to set gravity on or off
Logical :: Found  !flag for whether a trajectory was found
Real(dp) :: r1(1:3),v1(1:3),tof !position,velocity, time of flight defining flight from the surface of the central body
Real(dp) :: v2(1:3)
Real(dp) :: DFact !divergence factor for the intercepting flight from emission to intercept
Real(dp) :: Ee,zeta
! 
Real(dp) :: DEC,HA ![rad] declination and hour angle (used in place of lon/lat)
Integer :: dec_bin,ha_bin !computed bins for dec and ha
Integer :: En_bin,zeta_bin,b !computed bins for emission energy and emission angle cosine
Integer :: map_unit,bound_unit  !file unit number
Integer :: h !counter for number of histories (reused for declintion bin counter after history generation is complete)
Integer :: h_miss !counter for number of histories started but not propagated because a trajectory was not found
Integer :: e !counter for energy points, runs 1:n_En
Integer :: i,j,k,l,m  !counters
Character(2) :: e_char !character representation of energy index for file naming
Character(2) :: n_En_char !character representation of total number of energy points, for screen updates
Real(dp) :: Dfact_err,tof_err
Real(dp) :: lat,lon
# if CAF
 Integer :: next_e[*]
 Character(80) :: stat_lines(1:n_En)[*]
 Character(80) :: new_stat_line
# endif

!Satellite position & velocity
Call Initialize_Satellite_Motion('','Conic_tab ',sat)
Call sat%R_and_V(t2,r_sat,v_sat)
! Random Number Generator
# if CAF
 Call RNG%Initialize(seed = 7777777 , thread = this_image())
# else
 Call RNG%Initialize(seed = 7777777)
# endif
Write(n_En_char,'(I2)') n_En
! Compute angle cosine bin boundaries
Cos_box_grid = 2._dp
Call Linear_Spaces(1._dp,0._dp,Cos_box_grid(:))
! Compute energy bin boundaries
En_box_grid = -1._dp
j = 0
Do i = 1,n_En-1
    Call Log_Spaces(En(i),En(i+1),En_box_grid(j:j+n_box_En))
    j = j + n_box_En
End Do
Call Log_Spaces(En(n_En),10._dp*En(n_En),En_box_grid(j:j+n_box_En))
If (this_image().EQ.1) Then
    !write angle cosine grid boundaries
    Open(NEWUNIT = bound_unit , FILE = 'LPemissionMap_Cos_grid.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Do i = 0,n_box_Cos
        Write(bound_unit,'(I5,ES25.16E3)') i,Cos_box_grid(i)
    End Do
    Close(bound_unit)
    !write energy grid boundaries
    Open(NEWUNIT = bound_unit , FILE = 'LPemissionMap_En_grid.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Do i = 0,n_En*n_box_En
        Write(bound_unit,'(I5,ES25.16E3)',ADVANCE='NO') i,En_box_grid(i)/1000._dp !MeV
        If ( Any(En.EQ.En_box_grid(i)) ) Then
            Write(bound_unit,'(A)') ' *'
        Else
            Write(bound_unit,*)
        End If
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
End If        

# if CAF
 Do e = 1,n_En
    Write(e_char,'(I2.2)') e
    stat_lines(e) = 'En '//e_char//'/'//n_En_char//'   *.**% (  *.**% hits) Total F: *.********E+***'
 End Do
 next_e = 1
 Do
    CRITICAL
        e = next_e[1]
        next_e[1] = next_e[1] + 1
    END CRITICAL
    If (e .GT. n_En) Exit
# else
 Do e = 1,n_En
# endif
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
    Open(NEWUNIT = map_unit , FILE = 'LPemissionMap_e'//e_char//'.tst' , STATUS = 'REPLACE' , ACTION = 'WRITE')
    Write( map_unit , '(2ES25.16E3)' , ADVANCE = 'NO' ) En(e),t2
    h = 0
    h_miss = 0
    Do
        !choose a random direction of arrival at the detector
        Omega_hat2 = Isotropic_Omega_Hat(RNG)
        !check if an emission is possible at the surface at this energy to result in this rendezvous
        Call Prev_Event_Trajectory(sat, Gravity, t2, -Omega_hat2*Neutron_Speed(En(e)), Found, r1, v1, tof)
        If (found) Then
            h = h + 1
            !compute the declination and right-ascension indexes for this emission point
            DEC = ACOS( Dot_Product(Z_hat,Unit_Vector(r1)) )
            HA = Atan2( Dot_Product(Unit_Vector(r1),X_hat) , Dot_Product(Unit_Vector(r1),Y_hat) )
            If (HA .LT. 0._dp) HA = HA + TwoPi
            dec_bin = 1 + Floor(Real(n_lat_bins,dp) * DEC / Pi)
            ha_bin = 1 + Floor(Real(n_lon_bins,dp) * HA / TwoPi)
            !Compute divergence
            v2 = -Omega_hat2*Neutron_Speed(En(e)) + v_sat
            DFact = Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)
            !Compute emission energy bin, first by walking through En which is irregular, then by direct computation
            Ee = Neutron_Energy(v1)
            En_bin = 0
            If (e .LT. n_En) Then
                Do j = e+1,n_En
                    If (Ee .LT. En(j)) Then !this Ee fits in the segment of energy bins before En(j)
                        En_bin = (j-1)*n_box_En + Ceiling( Real(n_box_En,dp) * (Log(Ee)-Log(En(j-1))) / (Log(En(j))-Log(En(j-1))) )
                        Exit
                    End If
                End Do
            Else !special handling for the last batch of energies because they're above the end of the normal grid
                If (Ee .LT. 10._dp*En(n_En)) Then !this Ee fits in the segment of energy bins before En(j)
                    En_bin = n_En*n_box_En + & 
                           & Ceiling( Real(n_box_En,dp) * (Log(Ee)-Log(En(n_En))) / (Log(10._dp*En(n_En))-Log(En(n_En))) )
                Else
                    Print*,'ENERGY OUT OF BOUNDS:',Ee
                    STOP
                End If
            End If
            !compute emission direction bin
            zeta = Dot_Product(Unit_Vector(r1),Unit_Vector(v1))
            zeta_bin = Ceiling( (1._dp - zeta) * Real(n_box_Cos,dp) )
            !Check if this surface box has been initialized
            b = 0
            If (.NOT.f(dec_bin,ha_bin)%hit) Then !initialize the bin
                f(dec_bin,ha_bin)%hit = .TRUE.
                f(dec_bin,ha_bin)%x_size = 1
                Allocate(f(dec_bin,ha_bin)%x(1:1))
                Allocate(f(dec_bin,ha_bin)%iE(1:1))
                f(dec_bin,ha_bin)%iE = En_bin
                f(dec_bin,ha_bin)%x(1)%f = 0._dp
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
                        swap_x(l)%c = f(dec_bin,ha_bin)%x(l)%c
                        swap_x(l)%f = f(dec_bin,ha_bin)%x(l)%f
                    End Do
                    !deallocate primary lists
                    Deallocate(f(dec_bin,ha_bin)%iE)
                    Deallocate(f(dec_bin,ha_bin)%x)
                    !reallocate one element longer and copy lists back, inserting the new value at k
                    Allocate(f(dec_bin,ha_bin)%iE(1:f(dec_bin,ha_bin)%x_size+1))
                    f(dec_bin,ha_bin)%iE(:) = -1
                    Allocate(f(dec_bin,ha_bin)%x(1:f(dec_bin,ha_bin)%x_size+1))
                    f(dec_bin,ha_bin)%x(b)%c = 0
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
                                                 & (/ Dfact , Dfact**2 , Dfact*tof , Dfact*(tof**2) /)
        Else
            h_miss = h_miss + 1
        End If
#       if CAF
         If (MOD(h,1000).EQ.0) Then
            Write( new_stat_line,'(A,I2,A,F6.2,A,F6.2,A,ES16.8E3)' ) & 
                 & 'En ',e,'/'//n_En_char//' ',100._dp*Real(h,dp)/Real(n_trials,dp),'% (', &
                 & 100._dp*Real(h,dp)/Real(h+h_miss,dp),'% hits) Total F: ',Sum(f(:,:)%f(1))
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
#       else
         If (MOD(h,1000).EQ.0) Write( * , '(A,I2,A,F6.2,A,F6.2,A,ES16.8E3,A)' , ADVANCE = 'NO' ) & 
                                   & 'En ',e,'/'//n_En_char//' ',100._dp*Real(h,dp)/Real(n_trials,dp),'% (', &
                                   & 100._dp*Real(h,dp)/Real(h+h_miss,dp),'% hits) Total F: ',Sum(f(:,:)%f(1)),cr
#       endif
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
                    Do l = 1,n_box_Cos
                        If (f(i,j)%x(k)%c(l) .EQ. 0) Cycle
                        !intensity
                        Dfact = f(i,j)%x(k)%f(1,l) / Real(h+h_miss,dp)
                        Dfact_err = Std_err( h+h_miss , f(i,j)%x(k)%f(1,l) , f(i,j)%x(k)%f(2,l) )
                        !tof
                        tof = f(i,j)%x(k)%f(3,l) / (f(i,j)%x(k)%f(1,l) * Real(f(i,j)%x(k)%c(l),dp))
                        tof_err = Std_err( f(i,j)%x(k)%c(l) ,                        & 
                                         & f(i,j)%x(k)%f(3,l) / f(i,j)%x(k)%f(1,l) , & 
                                         & f(i,j)%x(k)%f(4,l) / f(i,j)%x(k)%f(1,l)  )
                        !Properties of a trajectory matching this tof and satellite position
                        Call Lambert_Gooding(r1,r_sat,tof,v1,v2)
                        !Write to file
                        Write( map_unit,'(4I5,I12,10ES25.16E3)') &
                             & i,j,k,l, & 
                             & f(i,j)%x(k)%c(l) , & 
                             & Dfact , Dfact_err , & 
                             & tof , tof_err , & 
                             & Neutron_Energy(v1) , & 
                             & Dot_Product(Unit_Vector(r1),Unit_Vector(v1)) , &
                             & Unit_Vector(v2 + v_sat) , &
                             & Div_Fact_by_shooting(r1,Unit_Vector(v1),Vector_Length(v1),(/0._dp,0._dp,0._dp/),tof,v_sat,v2)

                    End Do
                End Do
            End If
        End Do
    End Do
    Close(map_unit)
#   if CAF
#   else
     Write(*,*)
#   endif
End Do
# if CAF
 SYNC ALL
 If (this_image() .EQ. 1) Then
    Do i = 1,n_En+1
        Write(*,*)
    End Do
 End If
# endif

End Program

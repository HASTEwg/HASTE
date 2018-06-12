Program HATS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  High-Altitude Transport to Space for Neutrons (HATS-n)
!!  0.0     Initial version based on HASTE-n TE and HASTE-n
!!  0.1     Initial release candidate, debugged & ready for studies.  Ready for
!!          multithreading or coarray implementations.
!!  0.2     Initial coarray implementation.
!!  0.3     Revised coarray implementation for memory stability.  Revised next-
!!          event orbital trajectory selection and supporting astro routines.
!!  0.4     Converted from QuickWin to Console application.
!!  0.5     Speed improvements.
!!  0.6     Added direct (first-flight by sampling) computations. Revised cross 
!!          section implementation for speed.
!!  0.7     Revised EPL quadrature routines, Kepler Problem solver, & gravity 
!!          divergence approach.
!!  0.8     Opened development to HATSwg, migrated project to Github.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Use Kinds, Only: dp
Use Random_Numbers, Only: RNG_Type
Use Random_Numbers, Only: Setup_RNG
Use Setups, Only: Setup_HATS
Use Setups, Only: Paths_Files_Type
Use Setups, Only: Create_Output_File_names
Use Atmospheres, Only: Atmosphere_Type
Use Atmospheres, Only: Setup_Atmosphere
Use Sources, Only: Source_Type
Use Sources, Only: Setup_Source
Use Detectors, Only: Detector_Type
Use Detectors, Only: Setup_Detector
Use Detectors, Only: Close_Slice_Files
Use Neutron_Scatter, Only: Scatter_Model_Type
Use Neutron_Scatter, Only: Setup_Scatter_Model
Use MC_Neutron, Only: Do_Neutron
Use Tallies, Only: Contrib_array
Use Tallies, Only: Setup_Tallies
Use Results, Only: Write_Run_Summary
Use Results, Only: Write_Tally_Grids
Use Statistics, Only: gMean
Use Statistics, Only: Std_Err
Use FileIO_Utilities, Only: Make_Boom
Use FileIO_Utilities, Only: creturn
Use FileIO_Utilities, Only: Second_of_Month
# if CAF
    Use Setups, Only: Setup_info_to_disk
    Use Setups, Only: Setup_info_from_disk
    Use Setups, Only: Cleanup_temp_files
    Use Results, Only: Image_Result_to_Disk
    Use Results, Only: Image_Results_from_Disk
# endif

Implicit None

Character(80), Parameter :: title = 'High-Altitude Transport to Space for Neutrons (HATS-n) v0.8.02, 30 Jun 2018'
Integer(8) :: n_histories
Logical :: absolute_n_histories  !specifies whether number of histories is an absolute limit, or a goal to accumulate contributions on
Logical :: prompt_exit  !specifies whether the simulation will wait for user unput before exiting
Logical :: screen_progress  !determines if progress will be updated on screen during run
Type(Source_Type) :: source  !contains data defining neutron source
Type(Detector_Type) :: detector  !contains data defining detector
Type(Scatter_Model_Type) :: ScatterModel  !contains data defining the neutron scattering model
Type(Contrib_array) :: TE_Tallies  !list of contributions tallied to time-energy bins
Type(Contrib_array) :: Dir_Tallies  !list of contributions tallied to arrival direction bins
Type(Paths_Files_Type) :: paths_files  !contains path and file name character strings
Type(Atmosphere_Type) :: atmosphere  !contains data defining atmospheric representation
Type(RNG_Type) :: RNG  !contains data defining the random number generator
Real(dp) :: t_start,t_now,t_last,ETTC  !times (in seconds) for computing estimated time to completion and recording run time
Integer :: HH,MM,SS  !integer times for computing estimated time to completion
Integer(8) :: n_p,p  !loop counter for histories
Logical :: contributed  !flag indicating that at least one contribution was accumulated for the current history
Real(dp) :: t_tot,t_min,t_max
Integer :: n_img,i_img
Integer(8) :: n_done
Integer(8), Allocatable :: n_hist_run(:),n_hist_hit(:)

n_img = num_images()
i_img = this_image()
# if CAF
    If (i_img .EQ. 1) Then
# endif
    !UNSTANDARD Carriage control is no longer part of the Fortran standard, and its use here is specific to the implementation in Intel compilers ONLY.
    !Set carriage control to 'FORTRAN' so that console screen updates can be in-place
    !Open(6,CARRIAGECONTROL ='FORTRAN')
    Write(*,'(A)') '--------------------------------------------------------------------------------'
    Write(*,'(A)') title
    Write(*,'(A)') '--------------------------------------------------------------------------------'
    Write(*,'(A)') 'Setting up... '
    Call Setup_HATS(prompt_exit,screen_progress,paths_files,n_histories,absolute_n_histories)
    paths_files%app_title = title
#   if CAF
        Write(*,'(I6,A)') n_img,' images sharing histories'
        !Write processed setup info to disk for other images
        Call Setup_Info_to_disk(n_histories,absolute_n_histories,prompt_exit,screen_progress,paths_files)
#   endif
# if CAF
    End If
# endif
# if CAF
    !Sync so that setup info is available on disk for images other than 1
    SYNC ALL
    !Read setup info processed by image 1 from disk
    If (i_img .NE. 1) Then
        Call Setup_Info_from_disk(n_histories,absolute_n_histories,prompt_exit,screen_progress,paths_files)
        screen_progress = .FALSE.
    End If
# endif
RNG = Setup_RNG(paths_files%setup_file,paths_files%run_file_name)
atmosphere = Setup_Atmosphere(paths_files%setup_file,paths_files%resources_directory,paths_files%run_file_name,paths_files%cs_setup_file)
ScatterModel = Setup_Scatter_Model(paths_files%setup_file,paths_files%resources_directory,paths_files%cs_setup_file,paths_files%run_file_name)
source = Setup_Source(paths_files%setup_file,paths_files%run_file_name,atmosphere%R_top)
detector = Setup_Detector(paths_files%setup_file,paths_files%run_file_name,paths_files%s_file_name,atmosphere%R_top)
TE_Tallies = Setup_Tallies(detector%TE_grid(1)%n_bins,detector%TE_grid(2)%n_bins)
Dir_Tallies = Setup_Tallies(detector%Dir_grid(1)%n_bins,detector%Dir_grid(2)%n_bins)
If (continuation) Call Setup_Continuation(paths_files%results_directory,ScatterModel,TE_Tallies,Dir_Tallies)
# if CAF
    !divide n_histories equally among images
    n_p = n_histories / n_img  !integer divide
    If (i_img .EQ. n_img) n_p = n_p + Mod(n_histories,n_img)  !remainder added to last image
# else
    !all histories will be executed on a single image
    n_p = n_histories
# endif
!run a set of histories
If (i_img .EQ. 1) Then
    Write(*,'(A)') 'Running histories:  '
    Write(*,'(4A)') '  ',paths_files%results_directory,'...',paths_files%file_suffix
    If (screen_Progress) Then !prep screen for progress updates
        Write(*,'(A)') '    % Comp    ETTC     NE/min      H/min     gMean RSE   % Eff'
        Write(*,'(A)') '    ------  --------  ---------  ---------  -----------  ------'
        !Initialize progress to screen
        !Write(6,'(F9.2,A11,3ES11.3E2,A2,F7.2,A2)') 0._dp,'%  **:**:**',0._dp,0._dp,100._dp,' %',0._dp,' %'
        Write(*,'(F9.2,A11,3ES11.3E2,A2,F7.2,A2)') 0._dp,'%  **:**:**',0._dp,0._dp,100._dp,' %',0._dp,' %'
    End If
End If
p = 0
n_done = 0
!start a clock to track processing time
t_start = Second_of_Month()
If (screen_progress) Then  !run histories, periodically updating progress to the display
    t_last = t_start
    Do
        !update progress on screen
        t_now = Second_of_Month()
        If (t_now-t_last .GT. 1._dp) Then  !only update ETTC if elapsed time is more than a second
            t_last = t_now
            If (p .GT. 0) Then
                ETTC = (t_now - t_start) / Real(p,dp) * Real(n_p-p,dp)  !time per particle so far multiplied by particles remaining
                HH = Floor(ETTC/3600._dp)
                MM = Floor((ETTC - Real(3600*HH,dp))/60._dp)
                SS = Floor(ETTC - Real(3600*HH,dp) - Real(60*MM,dp))
                !Write(6,'("+",F9.2,A2,I3.2,A,I2.2,A,I2.2,3ES11.3E2,A2,F7.2,A2)') 100._dp * Real(p,dp) / Real(n_p,dp),'% ', & !Percent Complete
                !                                                                 & HH,':',MM,':',SS, & !ETTC
                !                                                                 & Real(n_img*ScatterModel%next_events(1),dp) / ((t_now-t_start) / 60._dp), & !Next-Events per minute
                !                                                                 & Real(n_img*p,dp) / ((t_now-t_start) / 60._dp), & !Histories per minute
                !                                                                 & 100._dp * gMean(Std_Err(p,TE_tallies%contribs(1:TE_tallies%index)%f,TE_tallies%contribs(1:TE_tallies%index)%f_sq) / TE_tallies%contribs(1:TE_tallies%index)%f),'% ', & !Geometric Mean Relative Standard Error
                !                                                                 & 100._dp * Real(p,dp) / Real(n_done,dp),'% ' !Percent efficency
                Write(*,'(A,F9.2,A2,I3.2,A,I2.2,A,I2.2,3ES11.3E2,A2,F7.2,A2)') creturn, & 
                                                                               & 100._dp * Real(p,dp) / Real(n_p,dp),'% ', & !Percent Complete
                                                                               & HH,':',MM,':',SS, & !ETTC
                                                                               & Real(n_img*ScatterModel%next_events(1),dp) / ((t_now-t_start) / 60._dp), & !Next-Events per minute
                                                                               & Real(n_img*p,dp) / ((t_now-t_start) / 60._dp), & !Histories per minute
                                                                               & 100._dp * gMean(Std_Err(p,TE_tallies%contribs(1:TE_tallies%index)%f,TE_tallies%contribs(1:TE_tallies%index)%f_sq) / TE_tallies%contribs(1:TE_tallies%index)%f),'% ', & !Geometric Mean Relative Standard Error
                                                                               & 100._dp * Real(p,dp) / Real(n_done,dp),'% ' !Percent efficency
            End If 
        End If
        !run a history
        Call Do_Neutron(source,detector,atmosphere,ScatterModel,RNG,contributed)
        n_done = n_done + 1
        If (contributed .OR. absolute_n_histories) Then
            p = p + 1
            !add the contributions from this history to the main time-energy and arrival direction contribution lists
            Call TE_Tallies%Tally_History(detector%TE_contrib_index,detector%TE_contribs_this_history(1:detector%TE_contrib_index),detector%TE_grid(1)%n_bins,detector%TE_contribs_t,detector%TE_grid(2)%n_bins,detector%TE_contribs_E)
            Call Dir_Tallies%Tally_History(detector%Dir_contrib_index,detector%Dir_contribs_this_history(1:detector%Dir_contrib_index),detector%Dir_grid(1)%n_bins,detector%Dir_contribs_mu,detector%Dir_grid(2)%n_bins,detector%Dir_contribs_omega)
        End If
        If (p .GE. n_p) Exit
    End Do
Else  !run histories, WITHOUT updating progress to the display
    Do
        !run a history
        Call Do_Neutron(source,detector,atmosphere,ScatterModel,RNG,contributed)
        n_done = n_done + 1
        If (contributed .OR. absolute_n_histories) Then
            p = p + 1
            !add the contributions from this history to the main time-energy and arrival direction contribution lists
            Call TE_Tallies%Tally_History(detector%TE_contrib_index,detector%TE_contribs_this_history(1:detector%TE_contrib_index),detector%TE_grid(1)%n_bins,detector%TE_contribs_t,detector%TE_grid(2)%n_bins,detector%TE_contribs_E)
            Call Dir_Tallies%Tally_History(detector%Dir_contrib_index,detector%Dir_contribs_this_history(1:detector%Dir_contrib_index),detector%Dir_grid(1)%n_bins,detector%Dir_contribs_mu,detector%Dir_grid(2)%n_bins,detector%Dir_contribs_omega)
        End If
        If (p .GE. n_p) Exit
    End Do
End If
!record final completion time
t_now = Second_of_Month()
t_tot = t_now - t_start
If (i_img.EQ.1 .AND. detector%shape_data) Call Close_Slice_Files(detector%n_slices,detector%TE_grid(1)%collect_shape,detector%TE_grid(1)%slice_unit,detector%TE_grid(2)%collect_shape,detector%TE_grid(2)%slice_unit)
# if CAF
    !Write image results to disk
    Call Image_Result_to_Disk(t_tot,n_p,n_done,TE_Tallies,Dir_Tallies,ScatterModel%n_kills,ScatterModel%next_events,ScatterModel%n_no_tally,ScatterModel%n_uncounted)
    SYNC ALL
    If (i_img .EQ. 1) Then
        !Gather image results from temporary files to image 1
        Call Image_Results_from_Disk(detector%TE_grid(1)%n_bins,detector%TE_grid(2)%n_bins,detector%Dir_grid(1)%n_bins,detector%Dir_grid(2)%n_bins,t_tot,t_min,t_max,n_hist_run,n_hist_hit,TE_Tallies,Dir_Tallies,ScatterModel%n_kills,ScatterModel%next_events,ScatterModel%n_no_tally,ScatterModel%n_uncounted)
    End If
# else
    Allocate(n_hist_run(1:1))
    n_hist_run = n_done + ScatterModel%n_uncounted  !includes implicity leaked histories from exatmospheric sources
    Allocate(n_hist_hit(1:1))
    n_hist_hit = n_p
    t_min = t_tot
    t_max = t_tot
# endif
If (i_img .EQ. 1) Then
    If (screen_progress) Then !finalize progress to screen
        !Write(6,'("+",F9.2,A2,I3.2,A,I2.2,A,I2.2,3ES11.3E2,A2,F7.2,A2)') 100._dp,'% ', & !Percent Complete
        !                                                                 & 0,':',0,':',0, & !ETTC
        !                                                                 & Real(ScatterModel%next_events(1),dp) / (t_tot / 60._dp), & !Next-Events per minute
        !                                                                 & Real(Sum(n_hist_run),dp) / (t_tot / 60._dp), & !Histories per minute
        !                                                                 & 100._dp * gMean(Std_Err(Sum(n_hist_run),TE_tallies%contribs(1:TE_tallies%index)%f,TE_tallies%contribs(1:TE_tallies%index)%f_sq) / TE_tallies%contribs(1:TE_tallies%index)%f),' %', & !Geometric Mean Relative Standard Error
        !                                                                 & 100._dp * Real(Sum(n_hist_hit),dp) / Real(Sum(n_hist_run),dp),'% ' !Percent efficency
        Write(*,'(A,F9.2,A2,I3.2,A,I2.2,A,I2.2,3ES11.3E2,A2,F7.2,A2)') creturn, & 
                                                                       & 100._dp,'% ', & !Percent Complete
                                                                       & 0,':',0,':',0, & !ETTC
                                                                       & Real(ScatterModel%next_events(1),dp) / (t_tot / 60._dp), & !Next-Events per minute
                                                                       & Real(Sum(n_hist_run),dp) / (t_tot / 60._dp), & !Histories per minute
                                                                       & 100._dp * gMean(Std_Err(Sum(n_hist_run),TE_tallies%contribs(1:TE_tallies%index)%f,TE_tallies%contribs(1:TE_tallies%index)%f_sq) / TE_tallies%contribs(1:TE_tallies%index)%f),' %', & !Geometric Mean Relative Standard Error
                                                                       & 100._dp * Real(Sum(n_hist_hit),dp) / Real(Sum(n_hist_run),dp),'% ' !Percent efficency
    End If
    !Write results
    Write(*,'(A)', ADVANCE = 'NO') 'Writing output... '
    Call Write_Run_Summary(n_img,t_tot,t_min,t_max,n_hist_hit,n_hist_run,RNG,paths_files,atmosphere,ScatterModel,source,detector,TE_Tallies,Dir_Tallies,paths_files%log_file_name)
    Call Write_Tally_Grids(TE_Tallies,Dir_Tallies,detector,Sum(n_hist_run),paths_files%F_file_name,paths_files%TE_file_name,paths_files%t_file_name,paths_files%E_file_name,paths_files%d_file_name,paths_files%m_file_name,paths_files%o_file_name)
#   if CAF
        !cleanup temp files
        Call Cleanup_temp_files()
#   endif
    Write(*,'(A)') 'Done.'
    Write(*,*)
End If
# if LIN_OS
    If (i_img .EQ. 1) Then
        Call Make_Boom()
        Write(*,'(A)') '--------------------------------------------------------------------------------'
        Write(*,*)
        Write(*,*)
    End If
# else
    If (i_img .EQ. 1) Then
        Call Make_Boom()
        Write(*,'(A)') '--------------------------------------------------------------------------------'
        If (prompt_exit) Pause 'Finished.  Press RETURN to exit...'
    End If
# endif

End Program HATS

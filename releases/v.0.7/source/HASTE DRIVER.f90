Program HASTE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Featured Air to Space Transport Estimator for Neutrons (FASTEN)
!!  0.0     Initial version based on HASTE-n TE
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
!!
!!  CONDITIONS FOR INCREMENT TO v1.0 (not yet met: as items are addressed,
!                                     they are removed from this list)
!!  -Cross Sections:  Change to raw ENDF "tape" as input instead of file 
!!                    collections, remove patch for resonance cross sections 
!!                    and replace with apprpriate routines to reconstruct 
!!                    resonances in-place
!!  -Pathlengths:  Switch Kepler EPLs to p-xi formulation
!!  -Find_Trajectory:  Add handling and searching for multiple roots to the 
!!                     rendezvous problem, also add convergence monitoring to 
!!                     trajectory solver in gravity cases
!!  -MC_Neutron:  Add weight adjustment in presence of gravity for 
!!                exoatmospheric source, add direct (first flight)
!!                contributions across detector grid without sampling (this 
!!                will include modifications to Detectors to add a first flight
!!                time-energy-direction grid which will not have variance 
!!                estimates)
!!  -Sources:  Create book-keeping and output of sampled source distributions
!!             (use direction & time-energy grids defined for detector to store 
!!             tallies for plotting the time-energy-direction distribution of 
!!             source neutrons), record and output information on biased 
!!             sourcing, correct biased sourcing for exoatmospheric sources
!!  -EVERYWHERE:  Change use of 'PRINT' statements to 'WRITE' statements for 
!!                more consistent formatting, create a log file to duplicate 
!!                and catch screen outputs so errors are correctly logged on 
!!                non-standard program stoppages
!!   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Use IFPORT, Only: DCLOCK
Use IFPORT, Only: $MAXPATH
Use Kinds, Only: dp
Use Random_Numbers, Only: RNG_Type
Use Random_Numbers, Only: Setup_RNG
Use Setups, Only: Setup_HASTE
Use Setups, Only: Paths_Files_Type
Use Setups, Only: Create_Output_File_names
Use Atmospheres, Only: Atmosphere_Type
Use Atmospheres, Only: Setup_Atmosphere
Use Sources, Only: Source_Type
Use Sources, Only: Setup_Source
Use Sources, Only: Celest_to_XYZ
Use Sources, Only: Check_Exo_Source
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
!DIR$ IF DEFINED (COA)
    Use Setups, Only: Setup_info_to_disk
    Use Setups, Only: Setup_info_from_disk
    Use Setups, Only: Cleanup_temp_files
    Use Results, Only: Image_Result_to_Disk
    Use Results, Only: Image_Results_from_Disk
!DIR$ END IF

Implicit None

Character(72), Parameter :: title = 'Featured Air to Space Transport Estimator for Neutrons (FASTEN) v0.7.04 '
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
If (i_img .EQ. 1) Then
    !Set carriage control to 'FORTRAN' so that console screen updates can be in-place
    Open(6,CARRIAGECONTROL ='FORTRAN')
    Write(*,'(A)') '------------------------------------------------------------------------'
    Write(*,'(A)') title
    Write(*,'(A)') '------------------------------------------------------------------------'
    Write(*,'(A)') 'Setting up... '
    Call Setup_HASTE(prompt_exit,screen_progress,paths_files,n_histories,absolute_n_histories)
    paths_files%app_title = title
    !DIR$ IF DEFINED (COA)
        Write(*,'(I6,A)') n_img,' images sharing histories'
        !Write processed setup info to disk for other images
        Call Setup_Info_to_disk(n_histories,absolute_n_histories,prompt_exit,screen_progress,paths_files)
    !DIR$ END IF
End If
!DIR$ IF DEFINED (COA)
    !Sync so that setup info is available on disk for images other than 1
    SYNC ALL
    !Read setup info processed by image 1 from disk
    If (i_img .NE. 1) Then
        Call Setup_Info_from_disk(n_histories,absolute_n_histories,prompt_exit,screen_progress,paths_files)
        screen_progress = .FALSE.
    End If
!DIR$ END IF
RNG = Setup_RNG(paths_files%setup_file,paths_files%run_file_name)
atmosphere = Setup_Atmosphere(paths_files%setup_file,paths_files%resources_directory,paths_files%run_file_name,paths_files%cs_setup_file)
ScatterModel = Setup_Scatter_Model(paths_files%setup_file,paths_files%resources_directory,paths_files%cs_setup_file,paths_files%run_file_name)
source = Setup_Source(paths_files%setup_file,paths_files%run_file_name,atmosphere%R_top)
detector = Setup_Detector(paths_files%setup_file,paths_files%run_file_name,paths_files%s_file_name,atmosphere%R_top)
TE_Tallies = Setup_Tallies(detector%TE_grid(1)%n_bins,detector%TE_grid(2)%n_bins)
Dir_Tallies = Setup_Tallies(detector%Dir_grid(1)%n_bins,detector%Dir_grid(2)%n_bins)
!DIR$ IF DEFINED (COA)
    !divide n_histories equally among images
    n_p = n_histories / n_img  !integer divide
    If (i_img .EQ. n_img) n_p = n_p + Mod(n_histories,n_img)  !remainder added to last image
!DIR$ ELSE
    !all histories will be executed on a single image
    n_p = n_histories
!DIR$ END IF
!run a set of histories
If (i_img .EQ. 1) Then
    Write(*,'(A)') 'Running histories:  '
    Write(*,'(4A)') '  ',paths_files%results_directory,'...',paths_files%file_suffix
    If (screen_Progress) Then !prep screen for progress updates
        Write(*,'(A)') '    % Comp    ETTC     NE/min      H/min     gMean RSE   % Eff'
        Write(*,'(A)') '    ------  --------  ---------  ---------  -----------  ------'
        !Initialize progress to screen
        Write(6,'(F9.2,A11,3ES11.3E2,A2,F7.2,A2)') 0._dp,'%  **:**:**',0._dp,0._dp,100._dp,' %',0._dp,' %'
    End If
End If
p = 0
n_done = 0
!start a clock to track processing time
t_start = Real(Floor(DCLOCK()*1.E3_dp),dp) * 1.E-3_dp  !truncate at millisecond
If (screen_progress) Then  !run histories, periodically updating progress to the display
    t_last = t_start
    Do
        !update progress on screen
        t_now = Real(Floor(DCLOCK()*1.E3_dp),dp) * 1.E-3_dp  !truncate at millisecond
        If (t_now-t_last .GT. 1._dp) Then  !only update ETTC if elapsed time is more than a second
            t_last = t_now
            If (p .GT. 0) Then
                ETTC = (t_now - t_start) / Real(p,dp) * Real(n_p-p,dp)  !time per particle so far multiplied by particles remaining
                HH = Floor(ETTC/3600._dp)
                MM = Floor((ETTC - Real(3600*HH,dp))/60._dp)
                SS = Floor(ETTC - Real(3600*HH,dp) - Real(60*MM,dp))
                Write(6,'("+",F9.2,A2,I3.2,A,I2.2,A,I2.2,3ES11.3E2,A2,F7.2,A2)') 100._dp * Real(p,dp) / Real(n_p,dp),'% ', & !Percent Complete
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
t_now = Real(Floor(DCLOCK()*1.E3_dp),dp) * 1.E-3_dp  !truncate at millisecond
t_tot = t_now - t_start
If (i_img.EQ.1 .AND. detector%shape_data) Call Close_Slice_Files(detector%n_slices,detector%TE_grid(1)%collect_shape,detector%TE_grid(1)%slice_unit,detector%TE_grid(2)%collect_shape,detector%TE_grid(2)%slice_unit)
!DIR$ IF DEFINED (COA)
    !Write image results to disk
    Call Image_Result_to_Disk(t_tot,n_p,n_done,TE_Tallies,Dir_Tallies,ScatterModel%n_kills,ScatterModel%next_events,ScatterModel%n_no_tally)
    SYNC ALL
    If (i_img .EQ. 1) Then
        !Gather image results from temporary files to image 1
        Call Image_Results_from_Disk(detector%TE_grid(1)%n_bins,detector%TE_grid(2)%n_bins,detector%Dir_grid(1)%n_bins,detector%Dir_grid(2)%n_bins,t_tot,t_min,t_max,n_hist_run,n_hist_hit,TE_Tallies,Dir_Tallies,ScatterModel%n_kills,ScatterModel%next_events,ScatterModel%n_no_tally)
    End If
!DIR$ ELSE
    Allocate(n_hist_run(1:1))
    n_hist_run = n_done
    Allocate(n_hist_hit(1:1))
    n_hist_hit = n_p
    t_min = t_tot
    t_max = t_tot
!DIR$ END IF
If (i_img .EQ. 1) Then
    If (screen_progress) Then !finalize progress to screen
        Write(6,'("+",F9.2,A2,I3.2,A,I2.2,A,I2.2,3ES11.3E2,A2,F7.2,A2)') 100._dp,'% ', & !Percent Complete
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
    !DIR$ IF DEFINED (COA)
        !cleanup temp files
        Call Cleanup_temp_files()
    !DIR$ END IF
    Write(*,'(A)') 'Done.'
    Write(*,*)
End If
!DIR$ IF DEFINED(MIC)
    If (i_img .EQ. 1) Then
        Call Make_Boom()
        Write(*,'(A)') '------------------------------------------------------------------------'
        Write(*,*)
        Write(*,*)
    End If
!DIR$ ELSE
    If (i_img .EQ. 1) Then
        Call Make_Boom()
        Write(*,'(A)') '------------------------------------------------------------------------'
        If (prompt_exit) Pause 'Finished.  Press RETURN to exit...'
    End If
!DIR$ END IF

End Program HASTE
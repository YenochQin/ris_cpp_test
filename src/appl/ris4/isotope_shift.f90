!===============================================================================
!> @brief Modern RIS (Relativistic Isotope Shift) calculation program
!> 
!> Entry routine for RIS calculations with modern Fortran 2023 features.
!> Controls the entire isotope shift computation using descriptive variable names,
!> comprehensive error handling, and modern programming practices.
!> 
!> This program calculates:
!> - Mass shift parameters
!> - Field shift parameters  
!> - Electronic density at the nucleus
!> - Radial expectation values for isotope shift analysis
!> 
!> @author Per Jonsson (original), Gediminas Gaigalas (translation)
!> @author J. Ekman, C. Nazé (modifications)
!> @date Last revision: Modernized for Fortran 2023
!===============================================================================
program isotope_shift
   use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
   use precision_definitions, only: working_precision_kind
   use default_C
   use iounit_C  
   use debug_C
   
   ! Use existing interfaces directly with modern naming conventions
   use getyn_I, only: getyn
   use setdbg_I, only: setdbg
   use getmixblock_I, only: getmixblock
   use setcon_I, only: setcon
   use setsum_I, only: setsum
   use strsum_I, only: strsum
   use factt_I, only: factt
   use ris_cal_I, only: ris_cal
   
   implicit none
   
   ! Local variables with descriptive names
   real(working_precision_kind) :: mass_ratio_squared
   logical :: user_wants_calculation
   character(len=24) :: atomic_system_name
   integer :: loop_counter, num_configurations, unused_core_count, parity_flag
   
   ! Error handling variables (Fortran 2023 feature)
   integer :: io_status, calculation_status
   character(len=256) :: error_message
   
   !---------------------------------------------------------------------------
   ! Initialize program settings and thresholds
   !---------------------------------------------------------------------------
   
   ! Set matrix elements cutoff threshold for numerical stability
   ! Matrix elements smaller than this value are not accumulated
   CUTOFF = 1.0e-10_working_precision_kind
   
   write(output_unit, '(A)') '==============================================================================='
   write(output_unit, '(A)') '  RELATIVISTIC ISOTOPE SHIFT CALCULATION PROGRAM (RIS4)'
   write(output_unit, '(A)') '  Modern Fortran 2023 Implementation'
   write(output_unit, '(A)') '==============================================================================='
   
   !---------------------------------------------------------------------------
   ! Main calculation setup and execution
   !---------------------------------------------------------------------------
   
   ! Initialize factorial table for angular momentum calculations
   call factt()
   
   ! Get atomic system name from user with error checking
   write(output_unit, '(A)', advance='no') 'Enter the name of the atomic system: '
   read(*, '(A)', iostat=io_status, iomsg=error_message) atomic_system_name
   if (io_status /= 0) then
      write(error_unit, '(A,I0,A,A)') 'ERROR: Failed to read system name (status=', &
         io_status, '): ', trim(error_message)
      error stop 'Fatal error reading atomic system name'
   end if
   
   ! Ask user if they want to proceed with calculation
   user_wants_calculation = getyn()
   
   if (user_wants_calculation) then
      ! Initialize debug settings
      call setdbg()
      
      ! Setup configuration space
      call setcon()
      
      ! Setup mixing coefficients (requires name and configuration count)
      call getmixblock(atomic_system_name, 1)
      
      ! Setup calculation summary (requires name and configuration count)
      call setsum(atomic_system_name, 1)
      
      ! Perform main isotope shift calculation
      call ris_cal(atomic_system_name)
      
      write(output_unit, '(A)') '==============================================================================='
      write(output_unit, '(A)') '  ISOTOPE SHIFT CALCULATION COMPLETED SUCCESSFULLY'
      write(output_unit, '(A)') '==============================================================================='
   else
      write(output_unit, '(A)') 'Calculation cancelled by user.'
   end if
   
end program isotope_shift
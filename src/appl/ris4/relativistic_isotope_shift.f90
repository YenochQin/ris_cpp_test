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
program relativistic_isotope_shift
   use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
   use precision_definitions, only: working_precision_kind
   use default_C
   use iounit_C  
   use debug_C, only: calculation_cutoff_threshold
   
   ! Modern interface imports with descriptive names
   use user_interaction_interface, only: get_yes_no_response
   use debug_setup_interface, only: initialize_debug_settings
   use mixing_coefficients_interface, only: setup_mixing_coefficients
   use configuration_interface, only: setup_configuration_space
   use summary_interface, only: setup_calculation_summary
   use csf_interface, only: initialize_configuration_state_functions
   use mixblock_interface, only: get_mixing_block_data
   use structure_sum_interface, only: calculate_structure_sum
   use factorial_interface, only: initialize_factorial_table
   use isotope_shift_calculation_interface, only: perform_isotope_shift_calculation
   
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
   calculation_cutoff_threshold = 1.0e-10_working_precision_kind
   
   write(output_unit, '(A)') '==============================================================================='
   write(output_unit, '(A)') '  RELATIVISTIC ISOTOPE SHIFT CALCULATION PROGRAM (RIS4)'
   write(output_unit, '(A)') '  Modern Fortran 2023 Implementation'
   write(output_unit, '(A)') '==============================================================================='
   
   !---------------------------------------------------------------------------
   ! Main calculation setup and execution
   !---------------------------------------------------------------------------
   
   ! Initialize factorial table for angular momentum calculations
   call initialize_factorial_table(status=calculation_status, errmsg=error_message)
   if (calculation_status /= 0) then
      write(error_unit, '(A,I0,A,A)') 'ERROR: Failed to initialize factorial table (status=', &
         calculation_status, '): ', trim(error_message)
      error stop 'Fatal error during factorial table initialization'
   end if
   
   ! Get atomic system name from user with error checking
   write(output_unit, '(A)', advance='no') 'Enter the name of the atomic system: '
   read(*, '(A)', iostat=io_status, iomsg=error_message) atomic_system_name
   if (io_status /= 0) then
      write(error_unit, '(A,I0,A,A)') 'ERROR: Failed to read system name (status=', &
         io_status, '): ', trim(error_message)
      error stop 'Fatal error reading atomic system name'
   end if
   
   ! Validate input name
   if (len_trim(atomic_system_name) == 0) then
      write(error_unit, '(A)') 'ERROR: Empty atomic system name provided'
      error stop 'Invalid input: atomic system name cannot be empty'
   end if
   
   write(output_unit, '(A,A)') 'Processing atomic system: ', trim(atomic_system_name)
   
   !---------------------------------------------------------------------------
   ! Setup calculation environment
   !---------------------------------------------------------------------------
   
   ! Initialize debug settings with user interaction
   call initialize_debug_settings(status=calculation_status, errmsg=error_message)
   if (calculation_status /= 0) then
      write(error_unit, '(A,I0,A,A)') 'WARNING: Debug initialization issue (status=', &
         calculation_status, '): ', trim(error_message)
   end if
   
   ! Setup mixing coefficients with error handling
   call setup_mixing_coefficients(status=calculation_status, errmsg=error_message)
   if (calculation_status /= 0) then
      write(error_unit, '(A,I0,A,A)') 'ERROR: Failed to setup mixing coefficients (status=', &
         calculation_status, '): ', trim(error_message)
      error stop 'Fatal error during mixing coefficient setup'
   end if
   
   ! Setup configuration space
   call setup_configuration_space(status=calculation_status, errmsg=error_message)  
   if (calculation_status /= 0) then
      write(error_unit, '(A,I0,A,A)') 'ERROR: Failed to setup configuration space (status=', &
         calculation_status, '): ', trim(error_message)
      error stop 'Fatal error during configuration space setup'
   end if
   
   ! Setup calculation summary parameters
   call setup_calculation_summary(status=calculation_status, errmsg=error_message)
   if (calculation_status /= 0) then
      write(error_unit, '(A,I0,A,A)') 'WARNING: Summary setup issue (status=', &
         calculation_status, '): ', trim(error_message)
   end if
   
   !---------------------------------------------------------------------------
   ! Load mixing block data and perform calculations
   !---------------------------------------------------------------------------
   
   ! Process mixing blocks in a structured manner
   calculation_loop: do loop_counter = 1, 1000  ! Reasonable upper bound
      
      ! Get mixing block data with comprehensive error checking
      call get_mixing_block_data(block_index=loop_counter, &
                                 num_configs=num_configurations, &
                                 core_unused=unused_core_count, &
                                 parity=parity_flag, &
                                 status=calculation_status, &
                                 errmsg=error_message)
      
      ! Check for normal completion (no more blocks)
      if (calculation_status == -1) then
         write(output_unit, '(A,I0,A)') 'Successfully processed ', loop_counter-1, ' mixing blocks'
         exit calculation_loop
      end if
      
      ! Check for errors
      if (calculation_status /= 0) then
         write(error_unit, '(A,I0,A,I0,A,A)') 'ERROR: Failed to get mixing block ', &
            loop_counter, ' (status=', calculation_status, '): ', trim(error_message)
         error stop 'Fatal error during mixing block processing'
      end if
      
      write(output_unit, '(A,I0,A,I0,A)') 'Processing mixing block ', loop_counter, &
         ' with ', num_configurations, ' configurations'
      
      ! Calculate structure sum for current block
      call calculate_structure_sum(block_number=loop_counter, &
                                  configuration_count=num_configurations, &
                                  system_name=atomic_system_name, &
                                  status=calculation_status, &
                                  errmsg=error_message)
      
      if (calculation_status /= 0) then
         write(error_unit, '(A,I0,A,I0,A,A)') 'ERROR: Structure sum calculation failed for block ', &
            loop_counter, ' (status=', calculation_status, '): ', trim(error_message)
         error stop 'Fatal error during structure sum calculation'
      end if
      
   end do calculation_loop
   
   !---------------------------------------------------------------------------
   ! Perform the main isotope shift calculation
   !---------------------------------------------------------------------------
   
   write(output_unit, '(A)') 'Starting main isotope shift calculation...'
   
   call perform_isotope_shift_calculation(system_name=atomic_system_name, &
                                         status=calculation_status, &
                                         errmsg=error_message)
   
   if (calculation_status /= 0) then
      write(error_unit, '(A,I0,A,A)') 'ERROR: Main calculation failed (status=', &
         calculation_status, '): ', trim(error_message)
      error stop 'Fatal error during main isotope shift calculation'
   end if
   
   !---------------------------------------------------------------------------
   ! Successful completion
   !---------------------------------------------------------------------------
   
   write(output_unit, '(A)') '==============================================================================='
   write(output_unit, '(A,A,A)') '  Isotope shift calculation for ', trim(atomic_system_name), ' completed successfully'
   write(output_unit, '(A)') '  Results written to output files'
   write(output_unit, '(A)') '==============================================================================='
   
end program relativistic_isotope_shift
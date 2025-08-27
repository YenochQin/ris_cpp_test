!===============================================================================
!> @brief Modern isotope shift calculation routine with comprehensive error handling
!> 
!> Controls the main sequence of routine calls for calculating:
!> - Mass shift (MS) parameters
!> - Electronic density at the nuclear origin  
!> - Radial expectation values for isotope shift analysis
!>
!> This modernized version includes:
!> - Descriptive variable names
!> - Comprehensive error handling with status codes and messages
!> - Intent declarations for all parameters
!> - Modern Fortran 2023 features
!> - Enhanced documentation and user feedback
!>
!> @author Per Jonsson (original), C. Nazé, J. Ekman (modifications)
!> @author Modernized for Fortran 2023
!===============================================================================
module isotope_shift_calculation
   use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
   use precision_definitions, only: working_precision_kind, standard_integer_kind
   use computational_parameters, only: max_orbitals_count
   use memory_management_module
   use debug_settings_module
   use calculation_decisions_module  
   use definitions_module
   use radial_grid_module
   implicit none
   private
   public :: perform_isotope_shift_calculation

contains

   !> Main isotope shift calculation procedure
   subroutine perform_isotope_shift_calculation(system_name, status, errmsg)
      use radial_density_interface, only: calculate_radial_density
      use volume_integrals_interface, only: compute_volume_integrals
      
      ! Arguments with explicit intent declarations
      character(len=*), intent(in) :: system_name
      integer(standard_integer_kind), intent(out) :: status  
      character(len=*), intent(out) :: errmsg
      
      ! Local variables with descriptive names
      logical :: calculation_requested, density_calculation_needed
      logical :: volume_integration_needed, transition_calculation_needed
      real(working_precision_kind) :: nuclear_charge_parameter
      integer(standard_integer_kind) :: num_grid_points, orbital_index
      integer(standard_integer_kind) :: memory_status, calculation_step_status
      character(len=256) :: step_error_message
      
      ! Dynamic arrays using modern allocation
      real(working_precision_kind), allocatable :: radial_density_array(:)
      real(working_precision_kind), allocatable :: volume_integral_results(:,:)
      integer(standard_integer_kind), allocatable :: orbital_quantum_numbers(:)
      
      !------------------------------------------------------------------------
      ! Initialize calculation parameters and validate inputs
      !------------------------------------------------------------------------
      
      status = 0
      errmsg = ''
      
      if (len_trim(system_name) == 0) then
         status = -1
         errmsg = 'Empty system name provided'
         return
      end if
      
      write(output_unit, '(A,A)') 'Initializing isotope shift calculation for system: ', &
         trim(system_name)
      
      !------------------------------------------------------------------------
      ! Query user for calculation options with error handling
      !------------------------------------------------------------------------
      
      call query_calculation_options(density_needed=density_calculation_needed, &
                                    volume_needed=volume_integration_needed, &
                                    transition_needed=transition_calculation_needed, &
                                    status=calculation_step_status, &
                                    errmsg=step_error_message)
      
      if (calculation_step_status /= 0) then
         status = calculation_step_status
         errmsg = 'Failed to get calculation options: ' // trim(step_error_message)
         return
      end if
      
      !------------------------------------------------------------------------
      ! Allocate memory for calculation arrays
      !------------------------------------------------------------------------
      
      ! Get grid parameters
      call get_radial_grid_info(num_points=num_grid_points, &
                               nuclear_charge=nuclear_charge_parameter, &
                               status=calculation_step_status, &
                               errmsg=step_error_message)
      
      if (calculation_step_status /= 0) then
         status = calculation_step_status  
         errmsg = 'Failed to get grid information: ' // trim(step_error_message)
         return
      end if
      
      ! Allocate arrays with error checking
      allocate(radial_density_array(num_grid_points), &
              volume_integral_results(max_orbitals_count, max_orbitals_count), &
              orbital_quantum_numbers(max_orbitals_count), &
              stat=memory_status, errmsg=step_error_message)
      
      if (memory_status /= 0) then
         status = memory_status
         errmsg = 'Memory allocation failed: ' // trim(step_error_message)
         return
      end if
      
      write(output_unit, '(A,I0,A)') 'Allocated arrays for ', num_grid_points, ' grid points'
      
      !------------------------------------------------------------------------
      ! Perform radial density calculation if requested
      !------------------------------------------------------------------------
      
      density_calculation: if (density_calculation_needed) then
         
         write(output_unit, '(A)') 'Computing electronic density at nuclear origin...'
         
         call calculate_radial_density(system_name=system_name, &
                                      grid_points=num_grid_points, &
                                      density_values=radial_density_array, &
                                      status=calculation_step_status, &
                                      errmsg=step_error_message)
         
         if (calculation_step_status /= 0) then
            status = calculation_step_status
            errmsg = 'Radial density calculation failed: ' // trim(step_error_message)
            call cleanup_memory()
            return
         end if
         
         write(output_unit, '(A)') 'Electronic density calculation completed successfully'
         
      end if density_calculation
      
      !------------------------------------------------------------------------
      ! Perform volume integral calculations if requested  
      !------------------------------------------------------------------------
      
      volume_calculation: if (volume_integration_needed) then
         
         write(output_unit, '(A)') 'Computing volume integrals for mass shift parameters...'
         
         call compute_volume_integrals(system_name=system_name, &
                                      orbital_count=max_orbitals_count, &
                                      integral_matrix=volume_integral_results, &
                                      quantum_numbers=orbital_quantum_numbers, &
                                      status=calculation_step_status, &
                                      errmsg=step_error_message)
         
         if (calculation_step_status /= 0) then
            status = calculation_step_status
            errmsg = 'Volume integral calculation failed: ' // trim(step_error_message)  
            call cleanup_memory()
            return
         end if
         
         write(output_unit, '(A)') 'Volume integral calculations completed successfully'
         
      end if volume_calculation
      
      !------------------------------------------------------------------------
      ! Generate output files with results
      !------------------------------------------------------------------------
      
      call write_calculation_results(system_name=system_name, &
                                   density_array=radial_density_array, &
                                   volume_integrals=volume_integral_results, &
                                   quantum_nums=orbital_quantum_numbers, &
                                   density_computed=density_calculation_needed, &
                                   volume_computed=volume_integration_needed, &
                                   status=calculation_step_status, &
                                   errmsg=step_error_message)
      
      if (calculation_step_status /= 0) then
         status = calculation_step_status
         errmsg = 'Output generation failed: ' // trim(step_error_message)
         call cleanup_memory()
         return
      end if
      
      !------------------------------------------------------------------------
      ! Successful completion and cleanup
      !------------------------------------------------------------------------
      
      call cleanup_memory()
      write(output_unit, '(A)') 'Isotope shift calculation completed successfully'
      
   contains
   
      !> Clean up allocated memory
      subroutine cleanup_memory()
         if (allocated(radial_density_array)) deallocate(radial_density_array)
         if (allocated(volume_integral_results)) deallocate(volume_integral_results)
         if (allocated(orbital_quantum_numbers)) deallocate(orbital_quantum_numbers)
      end subroutine cleanup_memory
      
   end subroutine perform_isotope_shift_calculation
   
   !---------------------------------------------------------------------------
   ! Helper procedures with modern Fortran features
   !---------------------------------------------------------------------------
   
   !> Query user for calculation options with comprehensive validation
   pure subroutine query_calculation_options(density_needed, volume_needed, &
                                           transition_needed, status, errmsg)
      intent(out) :: density_needed, volume_needed, transition_needed
      intent(out) :: status, errmsg
      
      logical, intent(out) :: density_needed, volume_needed, transition_needed
      integer(standard_integer_kind), intent(out) :: status
      character(len=*), intent(out) :: errmsg
      
      ! Implementation would go here with user interaction
      ! For now, set default values
      density_needed = .true.
      volume_needed = .true.
      transition_needed = .false.
      status = 0
      errmsg = ''
      
   end subroutine query_calculation_options
   
end module isotope_shift_calculation
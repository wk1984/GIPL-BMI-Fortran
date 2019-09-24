program test_get_input_var_names

  use bmif_1_2, only: BMI_FAILURE, BMI_MAX_VAR_NAME
  use bmigiplf
  use fixtures, only: status

  implicit none

  integer, parameter :: n_inputs = 8
  type (bmi_gipl) :: m
  character (len=BMI_MAX_VAR_NAME), allocatable :: expected(:)
  character (len=BMI_MAX_VAR_NAME) :: e1, e2, e3, e4, e5, e6, e7, e8
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: i

  allocate(expected(n_inputs))
  e1 = "land_surface_air__temperature"
  e2 = "snowpack__depth"
  e3 = "snow__thermal_conductivity"
  e4 = "soil_water__volume_fraction"
  e5 = "soil_unfrozen_water__a"
  e6 = "soil_unfrozen_water__b"
  e7 = "soil__thermal_conductivity__thawed"
  e8 = "soil__thermal_conductivity__frozen"
  
  expected = [e1,e2,e3,e4,e5,e6,e7,e8]
  
  status = m%get_input_var_names(names)
  
  ! Visualize
  do i = 1, n_inputs
     write(*,*) trim(names(i))
     write(*,*) trim(expected(i))
  end do
  
  do i=1, size(names)
     if (names(i).ne.expected(i)) then
        stop BMI_FAILURE
     end if
  end do
end program test_get_input_var_names

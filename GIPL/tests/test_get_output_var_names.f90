program test_get_output_var_names

  use bmif_1_2, only: BMI_FAILURE, BMI_MAX_VAR_NAME
  use bmigiplf
  use fixtures, only: status

  implicit none

  integer, parameter :: n_outputs = 2
  character (len=BMI_MAX_VAR_NAME), allocatable :: expected(:)
  character (len=BMI_MAX_VAR_NAME) :: e1, e2

  type (bmi_gipl) :: m
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: i

  allocate(expected(n_outputs))
  e1 = "soil__temperature"
  e2 = "model_soil_layer__count"
  expected = [e1,e2]
  
  status = m%get_output_var_names(names)

  ! Visualize
  write(*,*) names
  write(*,*) expected
  
  do i=1, size(names)
     if (names(i).ne.expected(i)) then
        stop BMI_FAILURE
     end if
  end do
end program test_get_output_var_names

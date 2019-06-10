program test_get_time_step

  use bmif_1_2, only: BMI_FAILURE
  use bmigiplf
  use fixtures, only: config_file, status

  implicit none

  real*8, parameter :: expected_time_step = 1.d0

  type (bmi_gipl) :: m
  real*8 :: time_step
  
  status = m%initialize(config_file)
  status = m%get_time_step(time_step)
  status = m%finalize()

  if (time_step.ne.expected_time_step) then
     stop BMI_FAILURE
  end if
end program test_get_time_step

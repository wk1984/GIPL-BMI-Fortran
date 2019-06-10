program test_update_frac

  use bmif_1_2, only: BMI_FAILURE
  use bmigiplf
  use fixtures, only: config_file, status

  implicit none

  double precision, parameter :: expected_time = 0.5d0

  type (bmi_gipl) :: m
  double precision :: time

  status = m%initialize(config_file)
  status = m%update_frac(expected_time)
  status = m%get_current_time(time)
  status = m%finalize()
  
  print*, expected_time, time

  if (time .ne. 1.5) then
     stop BMI_FAILURE
  end if
end program test_update_frac

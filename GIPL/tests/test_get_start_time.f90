program test_get_start_time

  use bmif_1_2, only: BMI_FAILURE
  use bmigiplf
  use fixtures, only: config_file, status

  implicit none

  double precision, parameter :: expected_time = 0.d0

  type (bmi_gipl) :: m
  double precision :: start_time
  
  status = m%initialize(config_file)
  status = m%get_start_time(start_time)
  status = m%finalize()
  
  if (status .eq. 0) then
  
  print*, start_time, expected_time
	
  if (start_time .ne. expected_time) then
     stop BMI_FAILURE
  end if
  
  else
  
  stop BMI_FAILURE
  
  endif

  
end program test_get_start_time

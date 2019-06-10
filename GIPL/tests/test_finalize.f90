program test_finalize

  use bmif_1_2, only: BMI_SUCCESS, BMI_FAILURE
  use bmigiplf
  use fixtures, only: status, config_file

  implicit none

  type (bmi_gipl) :: m
  integer :: status1

  status = m%initialize(config_file)
  status1 = m%finalize()
  if (status1.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if
end program test_finalize

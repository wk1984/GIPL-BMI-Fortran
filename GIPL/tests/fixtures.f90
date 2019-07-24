module fixtures
  
  use bmif_1_2, only: BMI_MAX_VAR_NAME
  implicit none
  
  character (len=BMI_MAX_VAR_NAME), parameter :: config_file = "test.cfg"

  integer :: status

contains

  subroutine print_array(array, dims)
    integer :: dims(2)
    real, dimension(product(dims)) :: array
    integer :: i, j

    do j = 1, dims(1)
       do i = 1, dims(2)
          write (*,"(f6.1)", advance="no") array(j + dims(1)*(i-1))
       end do
       write (*,*)
    end do
  end subroutine print_array

end module fixtures

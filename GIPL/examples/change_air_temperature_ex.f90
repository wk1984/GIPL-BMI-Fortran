! See the effect of changing diffusivity on plate temperature.
program change_air_temperature_ex

  use bmigiplf
  use testing_helpers, only: print_array
  implicit none

  character (len=*), parameter :: config_file = "test.cfg"
  character (len=*), parameter :: &
       dname = "land_surface_air__temperature"
  character (len=*), parameter :: &
       tname = "soil__temperature"
  double precision, parameter :: end_time = 365.d0

  type (bmi_gipl) :: m
  integer :: tgrid_id
  integer, dimension(2) :: tdims
  real :: t_adjust(1), temp(176), temp_new(176)
  integer :: status, i
  
  ! Run model to the end with alpha=1.0 (from cfg file).
  status = m%initialize(config_file)

  status = m%get_value(dname, t_adjust)
  write(*,"(a)") "Run 1"
  write(*,"(a, f5.2)") "tair =", t_adjust
  
  do i = 1, 182
  status = m%update()
  status = m%get_value(tname, temp)
  enddo
  
  status = m%finalize()
    
  ! Run model to the end with air temperature = 0.8.( 180 through 182 time step)
  
  status = m%initialize(config_file)
  t_adjust = 0.8

  write(*,"(a)") "Run 2"
  write(*,"(a, f5.2)") "tair =", t_adjust
  do i = 1, 182
  
  if ((i .ge. 180) .and. (i .le. 182)) then  
  status = m%set_value(dname, [t_adjust])
  endif
  status = m%update()
  status = m%get_value(tname, temp_new)
  enddo
  status = m%finalize()
  
  write(*,'(A8,A8)') 'Run 1', 'Run 2'
  do i =1,50
  write(*,'(F8.1,F8.1)') temp(i), temp_new(i)
  enddo
    
end program change_air_temperature_ex

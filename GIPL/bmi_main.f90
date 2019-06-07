! Run the heat model through its BMI.
program bmi_main

  use bmigiplf
  implicit none

  character (len=*), parameter :: var_name1 = "land_surface_air__temperature"
  character (len=*), parameter :: var_name2 = "snowpack__depth"
  character (len=*), parameter :: var_name3 = "snow__thermal_conductivity"
  
  character (len=*), parameter :: var_name5 = "precipitation_mass_flux_adjust_factor"
  character (len=*), parameter :: var_name6 = "snow_class"
  character (len=*), parameter :: var_name7 = "open_area_or_not"
  character (len=*), parameter :: var_name8 = "snowpack__initial_depth"
  character (len=*), parameter :: var_name9 = "initial_snow_density"
  
  integer, parameter :: ndims = 2
  integer:: s, grid_id1, grid_size1, grid_id2, grid_size2, i, iii
  integer:: grid_id3, grid_size3, grid_id4, grid_size4
  integer:: grid_id5, grid_size5, grid_id6, grid_size6
  integer:: grid_id7, grid_size7, grid_id8, grid_size8
  integer:: grid_id9, grid_size9
  
  real:: x
  
  character(len=10) var_unit1, var_unit2, var_unit3
  character(len=10) var_unit4, unit5, unit6
  character(len=10) unit7, unit8, unit9
    
  double precision :: current_time, end_time
  real, allocatable :: temperature(:)
  real, allocatable :: precipitation(:)
  real, allocatable :: snow_depth(:)
  real, allocatable :: snow_density(:)
  real, allocatable :: precipitation_adjust_factor(:)
  integer, allocatable :: snow_class(:)
  integer, allocatable :: open_area(:)
  real, allocatable :: initial_snow_depth(:)
  real, allocatable :: initial_snow_density(:)
  
  type (bmi_gipl) :: model
  
  character*256 fconfig 
      
  IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
  fconfig = '../data/gipl_config.cfg'
  ELSE
  CALL GET_COMMAND_ARGUMENT(1, fconfig)
  ENDIF
  
  do iii =  1, 3
  
  s = model%initialize(fconfig)
  
  write(*,"(a)") "Initialized"
    
  s = model%get_current_time(current_time)
  s = model%get_end_time(end_time)
  
  s = model%get_var_grid(var_name1, grid_id1)
  s = model%get_grid_size(grid_id1, grid_size1)
  s = model%get_var_units(var_name1, var_unit1)
    
!   if (iii == 3) then
!   
!   s = model%set_value(var_name7, [0])
!   s = model%set_value(var_name6, [2])
!   
!   endif
  
  write(*, '("Initial Time  = ",f0.1)') current_time     
  write(*, '("Final Time    = ",f0.1)') end_time
        
  allocate(temperature(grid_size1))
  
  s = model%get_value(var_name1, temperature)
   
!   write(*, '("Snow Class       = ",I0)') snow_class
!   write(*, '("Open Area or Not = ",I0)') open_area
!   write(*, '("P Adjust         = ",f0.2)') precipitation_adjust_factor

  print *, 'Updating'
  
!   do i = 1, end_time
!   
! !   s = model%set_value(var_name1, [-5.0]) ! set air temperature
! !   s = model%set_value(var_name2, [0.0])  ! set precipitation
! !   s = model%set_value(var_name8, [10.0]) ! set initial snow depth
! !   s = model%set_value(var_name9, [100.0]) ! set initial snow depth
!   
! !   s = model%set_value(var_name5, [0.5])  ! set snow class
! 
!   s = model%get_current_time(current_time)
!   s = model%get_value(var_name1, temperature)
!   s = model%get_value(var_name2, precipitation)
!    
!   s = model%update()
!   
!   s = model%get_value(var_name3, snow_depth)
!   s = model%get_value(var_name4, snow_density)
! 
!   enddo
! 
!   WRITE(*, '(F0.1, F8.2, A ,F8.2, A,F8.2, A, F6.1, A)') &
!   current_time, temperature, trim(var_unit1), &
!   precipitation, trim(var_unit2), &
!   snow_depth, trim(var_unit3), &
!   snow_density, trim(var_unit4)

deallocate(temperature)
    
  s = model%finalize()
  
  enddo

end program bmi_main

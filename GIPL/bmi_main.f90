! Run the heat model through its BMI.
program bmi_main

    use bmigiplf
    implicit none

    character (len = *), parameter :: var_name1 = "land_surface_air__temperature"
    character (len = *), parameter :: var_name2 = "snowpack__depth"
    character (len = *), parameter :: var_name3 = "snow__thermal_conductivity"

    character (len = *), parameter :: var_name4 = "soil_water__volume_fraction" ! VWC

    character (len = *), parameter :: var_name5 = "precipitation_mass_flux_adjust_factor"
    character (len = *), parameter :: var_name6 = "snow_class"

    character (len = *), parameter :: var_name8 = "snowpack__initial_depth"
    character (len = *), parameter :: var_name9 = "initial_snow_density"

    character (len = *), parameter :: out_name2 = 'model_soil_layer__count'
    character (len = *), parameter :: out_name1 = "soil__temperature"

    integer, parameter :: ndims = 2
    integer :: s, grid_id1, grid_size1, grid_id2, grid_size2, i, iii
    integer :: grid_id3, grid_size3, grid_id4, grid_size4
    integer :: grid_id5, grid_size5, grid_id6, grid_size6
    integer :: grid_id7, grid_size7, grid_id8, grid_size8
    integer :: grid_id9, grid_size9

    integer :: out_grid_id1, out_grid_size1, out_grid_rank1
    integer :: soil_nodes_number

    real :: x

    character(len = 10) var_unit1, var_unit2, var_unit3
    character(len = 10) var_unit4, unit5, unit6
    character(len = 10) unit7, unit8, unit9

    character(len = 10) out_var_unit1

    double precision :: current_time, end_time

    real, allocatable :: temperature(:)
    real, allocatable :: snow_depth(:)
    real, allocatable :: snow_conductivity(:)
    !    real, dimension(10) :: vwc, uwc_a, uwc_b, hc_a, hc_b, kt, kf

    real, allocatable :: soil_temperature(:)
    real, allocatable :: depth(:)

    type (bmi_gipl) :: model

    character*256 fconfig

    integer, dimension (3) :: out_grid_shape1

    IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
        fconfig = 'gipl_config.cfg'
    ELSE
        CALL GET_COMMAND_ARGUMENT(1, fconfig)
    ENDIF

    s = model%initialize(fconfig)

    write(*, "(a)") "Initialized"

    ! Get time variables:
    s = model%get_current_time(current_time)
    s = model%get_end_time(end_time)

    ! Get soil depth variables:
    s = model%get_var_itemsize(out_name2, soil_nodes_number)
    print*, 'Total soil nodes:', soil_nodes_number

    allocate(soil_temperature(soil_nodes_number))

    ! Get air temperature
    s = model%get_var_grid(var_name1, grid_id1)
    s = model%get_grid_size(grid_id1, grid_size1)
    s = model%get_var_units(var_name1, var_unit1)

    ! Get snow depth
    s = model%get_var_grid(var_name2, grid_id2)
    s = model%get_grid_size(grid_id2, grid_size2)
    s = model%get_var_units(var_name2, var_unit2)

    ! Get snow thermal conductivity
    s = model%get_var_grid(var_name3, grid_id3)
    s = model%get_grid_size(grid_id3, grid_size3)
    s = model%get_var_units(var_name3, var_unit3)

    ! Get snow thermal conductivity
    s = model%get_var_grid(var_name4, grid_id4)
    s = model%get_grid_size(grid_id4, grid_size4)
    s = model%get_var_units(var_name4, var_unit4)

    print*, grid_id4

    ! Get soil temperatures
    s = model%get_var_grid(out_name1, out_grid_id1)
    s = model%get_grid_rank(out_grid_id1, out_grid_rank1)
    s = model%get_grid_size(out_grid_id1, out_grid_size1)
    s = model%get_var_units(out_name1, out_var_unit1)
    s = model%get_grid_shape(out_grid_id1, out_grid_shape1)

    print*, 'Soil Temperature [size]', grid_size3
    print*, 'Soil Temperature [rank]', out_grid_rank1
    print*, 'Soil Temperature [shape]', out_grid_shape1

    write(*, '("Initial Time  = ",f0.1)') current_time
    write(*, '("Final Time    = ",f0.1)') end_time

    allocate(temperature(grid_size1))
    allocate(snow_depth(grid_size2))
    allocate(snow_conductivity(grid_size3))

    write(*, '(A5, 1x, A8, 1X, A8, 1X,A8, 1X,A8, 1X,A8)') 'T', 'Tair', 'Snow', 'Surface', 'Tg 0.3m', 'Tg 1m'

    do i = 1, int(end_time)

        if (i .eq. 5) then ! change air temperature at the 5th time step.

            s = model%set_value('land_surface_air__temperature', [-5.0])

        end if

        s = model%update()

        s = model%get_value(var_name1, temperature)
        s = model%get_value(var_name2, snow_depth)
        s = model%get_value(var_name3, snow_conductivity)
        s = model%get_value(out_name1, soil_temperature)

        if ((i .le. 190) .and. (i .ge. 180)) then

            write(*, '(I5, 1x, F8.3, 1X, F8.3, 1X,F8.3, 1X,F8.3, 1X,F8.3)'), i, temperature, snow_depth, soil_temperature(39), soil_temperature(59), soil_temperature(94)

        end if
    enddo

    s = model%finalize()

end program bmi_main

! Run the heat model through its BMI.
program bmi_main

    use bmigiplf
    implicit none

    character (len = *), parameter :: var_name1 = "land_surface_air__temperature"
    character (len = *), parameter :: var_name2 = "snowpack__depth"
    character (len = *), parameter :: var_name3 = "snow__thermal_conductivity"

    !-------------------------------
    ! These parameters could be set by using "set_value_at_indices"
    ! Known issues are related to grid type, grid_id, grid_size, etc.
    ! Because these variables are defined at few layers rather than each soil node.

    character (len = *), parameter :: var_name4 = "soil_water__volume_fraction" ! VWC
    character (len = *), parameter :: var_name5 = "soil_unfrozen_water__a" ! UWC_a
    character (len = *), parameter :: var_name6 = "soil_unfrozen_water__b" ! UWC_b

    !-------------------------------

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

    real :: x, obs1, obs2, obs3, obs4, obs5, obs6

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
    double precision, allocatable :: depth(:)
    double precision, allocatable :: dz(:)

    type (bmi_gipl) :: model

    integer :: arg_count = 0
    character*256 fconfig

    integer, dimension (1) :: out_grid_shape1

    do while (arg_count <= 1)
       call get_command_argument(arg_count, fconfig)
       arg_count = arg_count + 1
    end do

    if (len_trim(fconfig) == 0 .or. trim(fconfig) == '-h' &
         .or. trim(fconfig) == '--help') then
       write(*,"(a)") "Usage: run_bmigipl_model CONFIGURATION_FILE"
       write(*,"(a)")
       write(*,"(a)") "Run GIPL through its BMI with a configuration file."
       return
    end if

    s = model%initialize(fconfig)

    if (s .eq. 1) then

        write(*, "(a)") "Error"

    else

        write(*, "(a)") "Initialized"

        s = model%set_value('write_or_not', [1])

        ! Get time variables:
        s = model%get_current_time(current_time)
        s = model%get_end_time(end_time)

        ! Get soil depth variables:
        s = model%get_var_grid(out_name1, out_grid_id1)
        s = model%get_grid_size(out_grid_id1, soil_nodes_number)
        print*, 'Total soil nodes:', soil_nodes_number

        allocate(soil_temperature(soil_nodes_number))
        allocate(depth(soil_nodes_number))

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

        s = model%get_grid_z(out_grid_id1, depth) ! get depths

        do i = 1, soil_nodes_number
            write(*, '(I5, F8.3)') i, depth(i)
        end do

        allocate(temperature(grid_size1))
        allocate(snow_depth(grid_size2))
        allocate(snow_conductivity(grid_size3))

        write(*, '(A5, 1x, A8, 1X, A8, 1X,A8, 1X,A8, 1X,A8, 1X,A8)') &
                'T', 'Tair', 'Snow', &
                '0.001m', 'Tg 0.08m', 'Tg 0.12m', &
                'Tg 0.20m'

        do i = 1, int(end_time)

            if (i .eq. 85) then

                ! change VWC in first soil layer in 'geo.txt' file.

                !                s = model%set_value_at_indices(var_name4, [1], [0.1])

            end if

            if (i .eq. 92) then

                ! change UWC_a in first soil layer in 'geo.txt' file.

                !                s = model%set_value_at_indices(var_name5, [1], [0.1])

            end if

            if (i .eq. 92) then

                ! change UWC_b in first soil layer in 'geo.txt' file.

                !                s = model%set_value_at_indices(var_name6, [1], [-0.5])

            end if

            if (i .eq. 95) then ! change air temperature at the 5th time step.

                !                 s = model%set_value('land_surface_air__temperature', [-10.0])

            end if

            if (i .eq. 100) then ! change snow depth at the 100th time step.

                !                            s = model%set_value('snowpack__depth', [0.1])

            end if

            if (i .eq. 110) then ! change snow thermal conductivity at the 110th time step.

                !                             s = model%set_value('snow__thermal_conductivity', [0.01])

            end if

            s = model%get_value(var_name1, temperature)
            s = model%get_value(var_name2, snow_depth)
            s = model%get_value(var_name3, snow_conductivity)

            s = model%update()

            s = model%get_value(out_name1, soil_temperature)

            if ((i .le. 130) .and. (i .ge. 91)) then

                write(*, '(I5, 1x, F8.3, 1X, F8.3, 1X,F8.3, 1X,F8.3, 1X,F8.3, 1X,F8.3)') &
                        i, temperature, snow_depth, &
                        soil_temperature(40), &
                        soil_temperature(48), &
                        soil_temperature(50), &
                        soil_temperature(54)

            end if
        enddo

        s = model%finalize()

    endif

end program bmi_main

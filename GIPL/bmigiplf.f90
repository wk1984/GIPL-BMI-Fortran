module bmigiplf

    use gipl_model
    use bmif_1_2
    use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_f_pointer
    implicit none

    type, extends (bmi) :: bmi_gipl
        private
        type (gipl_model_type) :: model
    contains
        procedure :: get_component_name => gipl_component_name
        procedure :: get_input_var_names => gipl_input_var_names
        procedure :: get_output_var_names => gipl_output_var_names
        procedure :: initialize => gipl_initialize
        procedure :: finalize => gipl_finalize
        procedure :: get_start_time => gipl_start_time
        procedure :: get_end_time => gipl_end_time
        procedure :: get_current_time => gipl_current_time
        procedure :: get_time_step => gipl_time_step
        procedure :: get_time_units => gipl_time_units
        procedure :: update => gipl_update
        procedure :: update_frac => gipl_update_frac
        procedure :: update_until => gipl_update_until
        procedure :: get_var_grid => gipl_var_grid
        procedure :: get_grid_type => gipl_grid_type
        procedure :: get_grid_rank => gipl_grid_rank
        procedure :: get_grid_shape => gipl_grid_shape
        procedure :: get_grid_size => gipl_grid_size
        procedure :: get_grid_spacing => gipl_grid_spacing
        procedure :: get_grid_origin => gipl_grid_origin
        procedure :: get_grid_x => gipl_grid_x
        procedure :: get_grid_y => gipl_grid_y
        procedure :: get_grid_z => gipl_grid_z
        procedure :: get_grid_connectivity => gipl_grid_connectivity
        procedure :: get_grid_offset => gipl_grid_offset
        procedure :: get_var_type => gipl_var_type
        procedure :: get_var_units => gipl_var_units
        procedure :: get_var_itemsize => gipl_var_itemsize
        procedure :: get_var_nbytes => gipl_var_nbytes
        procedure :: get_value_int => gipl_get_int
        procedure :: get_value_float => gipl_get_float
        procedure :: get_value_double => gipl_get_double
        generic :: get_value => &
                get_value_int, &
                get_value_float, &
                get_value_double
        procedure :: get_value_ptr_int => gipl_get_ptr_int
        procedure :: get_value_ptr_float => gipl_get_ptr_float
        procedure :: get_value_ptr_double => gipl_get_ptr_double
        generic :: get_value_ptr => &
                get_value_ptr_int, &
                get_value_ptr_float, &
                get_value_ptr_double
        procedure :: get_value_at_indices_int => gipl_get_at_indices_int
        procedure :: get_value_at_indices_float => gipl_get_at_indices_float
        procedure :: get_value_at_indices_double => gipl_get_at_indices_double
        generic :: get_value_at_indices => &
                get_value_at_indices_int, &
                get_value_at_indices_float, &
                get_value_at_indices_double
        procedure :: set_value_int => gipl_set_int
        procedure :: set_value_float => gipl_set_float
        procedure :: set_value_double => gipl_set_double
        generic :: set_value => &
                set_value_int, &
                set_value_float, &
                set_value_double
        procedure :: set_value_at_indices_int => gipl_set_at_indices_int
        procedure :: set_value_at_indices_float => gipl_set_at_indices_float
        procedure :: set_value_at_indices_double => gipl_set_at_indices_double
        generic :: set_value_at_indices => &
                set_value_at_indices_int, &
                set_value_at_indices_float, &
                set_value_at_indices_double
        procedure :: print_model_info
    end type bmi_gipl

    private
    public :: bmi_gipl

    character (len = BMI_MAX_COMPONENT_NAME), target :: &
            component_name = "The 1D GIPL Model"

    ! Exchange items
    integer, parameter :: input_item_count = 6
    integer, parameter :: output_item_count = 2
    character (len = BMI_MAX_VAR_NAME), target, &
            dimension(input_item_count) :: input_items
    character (len = BMI_MAX_VAR_NAME), target, &
            dimension(output_item_count) :: output_items

contains

    ! Get the name of the model.
    function gipl_component_name(self, name) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), pointer, intent(out) :: name
        integer :: bmi_status

        name => component_name
        bmi_status = BMI_SUCCESS
    end function gipl_component_name

    ! List input variables.
    function gipl_input_var_names(self, names) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (*), pointer, intent(out) :: names(:)
        integer :: bmi_status

        input_items(1) = 'land_surface_air__temperature'
        input_items(2) = 'snowpack__depth'
        input_items(3) = 'snow__thermal_conductivity'
        input_items(4) = 'soil_water__volume_fraction'
        input_items(5) = 'soil_unfrozen_water__a'
        input_items(6) = 'soil_unfrozen_water__b'

        names => input_items
        bmi_status = BMI_SUCCESS

    end function gipl_input_var_names

    ! List output variables.
    function gipl_output_var_names(self, names) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (*), pointer, intent(out) :: names(:)
        integer :: bmi_status

        output_items(1) = 'soil__temperature'
        output_items(2) = 'model_soil_layer__count'

        names => output_items
        bmi_status = BMI_SUCCESS
    end function gipl_output_var_names

    ! BMI initializer.
    function gipl_initialize(self, config_file) result (bmi_status)
        class (bmi_gipl), intent(out) :: self
        character (len = *), intent(in) :: config_file
        integer :: bmi_status

        if (len(config_file) > 0) then
            call initialize(self%model, config_file)

            if (self%model%initialize_status .eq. 1) then
                bmi_status = BMI_SUCCESS
            else
                bmi_status = BMI_FAILURE
            endif
        else
            bmi_status = BMI_FAILURE
            stop
        end if

    end function gipl_initialize

    ! BMI finalizer.
    function gipl_finalize(self) result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        integer :: bmi_status

        call finalize(self%model)
        bmi_status = BMI_SUCCESS
    end function gipl_finalize

    ! Model start time.
    function gipl_start_time(self, time) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        double precision, intent(out) :: time
        integer :: bmi_status

        time = dble(self%model%time_beg)
        bmi_status = BMI_SUCCESS
    end function gipl_start_time

    ! Model end time.
    function gipl_end_time(self, time) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        double precision, intent(out) :: time
        integer :: bmi_status

        time = dble(self%model%time_end)
        bmi_status = BMI_SUCCESS
    end function gipl_end_time

    ! Model current time.
    function gipl_current_time(self, time) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        double precision, intent(out) :: time
        integer :: bmi_status

        time = dble(self%model%top_run_time)
        bmi_status = BMI_SUCCESS
    end function gipl_current_time

    ! Model time step.
    function gipl_time_step(self, time_step) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        double precision, intent(out) :: time_step
        integer :: bmi_status

        time_step = dble(self%model%time_step)
        bmi_status = BMI_SUCCESS
    end function gipl_time_step

    ! Model time units.
    function gipl_time_units(self, time_units) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(out) :: time_units
        integer :: bmi_status

        time_units = "second"

        if (self%model%n_sec_day .eq. dble(86400.)) then
            time_units = "day"
        endif
        if(self%model%n_sec_day .eq. dble(2628000.)) then
            time_units = "month"
        endif
        if(self%model%n_sec_day .eq. dble(3600.)) then
            time_units = "hour"
        endif

        bmi_status = BMI_SUCCESS
    end function gipl_time_units

    ! Advance model by one time step.
    function gipl_update(self) result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        integer :: bmi_status

        call update(self%model)
        bmi_status = BMI_SUCCESS
    end function gipl_update

    ! Advance the model by a fraction of a time step.
    function gipl_update_frac(self, time_frac) result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        double precision, intent(in) :: time_frac
        integer :: bmi_status
        real :: time_step

        if (time_frac > 0.0) then
            time_step = self%model%time_step
            self%model%time_step = time_step * real(time_frac)
            call update(self%model)
            self%model%time_step = time_step
        end if
        bmi_status = BMI_SUCCESS
    end function gipl_update_frac

    ! Advance the model until the given time.
    function gipl_update_until(self, time) result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        double precision, intent(in) :: time
        integer :: bmi_status
        double precision :: n_steps_real
        integer :: n_steps, i, s

        if (time > self%model%top_run_time) then
            n_steps_real = (time - self%model%top_run_time) / self%model%dt
            n_steps = floor(n_steps_real)
            do i = 1, n_steps
                s = self%update()
            end do
            s = self%update_frac(n_steps_real - dble(n_steps))
        end if
        bmi_status = BMI_SUCCESS
    end function gipl_update_until

    ! Get the grid id for a particular variable.
    function gipl_var_grid(self, var_name, grid_id) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        integer, intent(out) :: grid_id
        integer :: bmi_status

        select case(var_name)
        case('land_surface_air__temperature')
            grid_id = 0
            bmi_status = BMI_SUCCESS
        case('snow__thermal_conductivity')
            grid_id = 0
            bmi_status = BMI_SUCCESS
        case('snowpack__depth')
            grid_id = 0
            bmi_status = BMI_SUCCESS
        case('model_soil_layer__count')
            grid_id = 0
            bmi_status = BMI_SUCCESS
        case('soil_water__volume_fraction')
            grid_id = 2
            bmi_status = BMI_SUCCESS
        case('soil_unfrozen_water__a')
            grid_id = 2
        case('soil_unfrozen_water__b')
            grid_id = 2
        case('soil__temperature')
            grid_id = 2
            bmi_status = BMI_SUCCESS
        case default
            grid_id = -1
            bmi_status = BMI_FAILURE
        end select
    end function gipl_var_grid

    ! The type of a variable's grid.
    function gipl_grid_type(self, grid_id, grid_type) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        character (len = *), intent(out) :: grid_type
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_type = "rectilinear"
            bmi_status = BMI_SUCCESS
        case(1)
            grid_type = "scalar"
            bmi_status = BMI_SUCCESS
        case(2)
            grid_type = "three_dims"
            bmi_status = BMI_SUCCESS
        case default
            grid_type = "-"
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_type

    ! The number of dimensions of a grid.
    function gipl_grid_rank(self, grid_id, grid_rank) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        integer, intent(out) :: grid_rank
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_rank = 2
            bmi_status = BMI_SUCCESS
        case(1)
            grid_rank = 0
            bmi_status = BMI_SUCCESS
        case(2)
            grid_rank = 3
        case default
            grid_rank = -1
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_rank

    ! The dimensions of a grid.
    function gipl_grid_shape(self, grid_id, grid_shape) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        integer, dimension(:), intent(out) :: grid_shape
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_shape = [self%model%n_y, self%model%n_x]
            bmi_status = BMI_SUCCESS
        case(1)
            grid_shape = [1]
            bmi_status = BMI_SUCCESS
        case(2)
            grid_shape = [self%model%n_y, self%model%n_x, self%model%n_z]
            bmi_status = BMI_SUCCESS
        case default
            grid_shape = [-1]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_shape

    ! The total number of elements in a grid.
    function gipl_grid_size(self, grid_id, grid_size) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        integer, intent(out) :: grid_size
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_size = self%model%n_y * self%model%n_x
            bmi_status = BMI_SUCCESS
        case(1)
            grid_size = 1
            bmi_status = BMI_SUCCESS
        case(2)
            grid_size = self%model%n_y * self%model%n_x * self%model%n_z
            bmi_status = BMI_SUCCESS
        case default
            grid_size = -1
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_size

    ! The distance between nodes of a grid.
    function gipl_grid_spacing(self, grid_id, grid_spacing) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        double precision, dimension(:), intent(out) :: grid_spacing
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_spacing = [self%model%dy, self%model%dx]
            bmi_status = BMI_SUCCESS
        case(1)
            grid_spacing = [self%model%dx]
            bmi_status = BMI_SUCCESS
        case(2)
            grid_spacing = [self%model%dy, self%model%dx, self%model%dz]
            bmi_status = BMI_SUCCESS
        case default
            grid_spacing = [-1.d0]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_spacing

    ! Coordinates of grid origin.
    function gipl_grid_origin(self, grid_id, grid_origin) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        double precision, dimension(:), intent(out) :: grid_origin
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_origin = [0.d0, 0.d0]
            bmi_status = BMI_SUCCESS
        case(1)
            grid_origin = [0.d0]
            bmi_status = BMI_SUCCESS
        case(2)
            grid_origin = [0.d0, 0.d0, 0.d0]
            bmi_status = BMI_SUCCESS
        case default
            grid_origin = [-1.d0]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_origin

    ! X-coordinates of grid nodes.
    function gipl_grid_x(self, grid_id, grid_x) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        double precision, dimension(:), intent(out) :: grid_x
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_x = [0.d0]
            bmi_status = BMI_SUCCESS
        case(1)
            grid_x = [0.d0]
            bmi_status = BMI_SUCCESS
        case(2)
            grid_x = [0.d0]
            bmi_status = BMI_SUCCESS
        case default
            grid_x = [-1.d0]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_x

    ! Y-coordinates of grid nodes.
    function gipl_grid_y(self, grid_id, grid_y) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        double precision, dimension(:), intent(out) :: grid_y
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_y = [0.d0]
            bmi_status = BMI_SUCCESS
        case(1)
            grid_y = [0.d0]
            bmi_status = BMI_SUCCESS
        case(2)
            grid_y = [0.d0]
            bmi_status = BMI_SUCCESS
        case default
            grid_y = [-1.d0]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_y

    ! Z-coordinates of grid nodes.
    function gipl_grid_z(self, grid_id, grid_z) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        double precision, dimension(:), intent(out) :: grid_z
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_z = [0.d0]
            bmi_status = BMI_SUCCESS
        case(1)
            grid_z = [0.d0]
            bmi_status = BMI_SUCCESS
        case(2)
            grid_z = [self%model%zdepth]
            bmi_status = BMI_SUCCESS
        case default
            grid_z = [-1.d0]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_z

    ! Connectivity array of unstructured grid nodes.
    function gipl_grid_connectivity(self, grid_id, grid_conn) &
            result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        integer, dimension(:), intent(out) :: grid_conn
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_conn = [0, 0]
            bmi_status = BMI_SUCCESS
        case(1)
            grid_conn = [0]
            bmi_status = BMI_SUCCESS
        case(2)
            grid_conn = [0, 0, 0]
            bmi_status = BMI_SUCCESS
        case default
            grid_conn = [-1]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_connectivity

    ! Offsets of unstructured grid nodes.
    function gipl_grid_offset(self, grid_id, grid_offset) &
            result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        integer, intent(in) :: grid_id
        integer, dimension(:), intent(out) :: grid_offset
        integer :: bmi_status

        select case(grid_id)
        case(0)
            grid_offset = [0, 0]
            bmi_status = BMI_SUCCESS
        case(1)
            grid_offset = [0]
            bmi_status = BMI_SUCCESS
        case(2)
            grid_offset = [0, 0, 0]
            bmi_status = BMI_SUCCESS
        case default
            grid_offset = [-1]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_grid_offset

    ! The data type of the variable, as a string.
    function gipl_var_type(self, var_name, var_type) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        character (len = *), intent(out) :: var_type
        integer :: bmi_status

        select case(var_name)
        case("land_surface_air__temperature")
            var_type = "real"
            bmi_status = BMI_SUCCESS
        case("snowpack__depth")
            var_type = "real"
            bmi_status = BMI_SUCCESS
        case("snow__thermal_conductivity")
            var_type = "real"
            bmi_status = BMI_SUCCESS
        case("model_soil_layer__count")
            var_type = "integer"
            bmi_status = BMI_SUCCESS
        case("soil_water__volume_fraction")
            var_type = "real"
            bmi_status = BMI_SUCCESS
        case("soil__temperature")
            var_type = "real"
            bmi_status = BMI_SUCCESS
        case("soil_unfrozen_water__a")
            var_type = "real"
            bmi_status = BMI_SUCCESS
        case("soil_unfrozen_water__b")
            var_type = "real"
            bmi_status = BMI_SUCCESS
        case("write_or_not")
            var_type = "integer"
            bmi_status = BMI_SUCCESS
        case default
            var_type = "-"
            bmi_status = BMI_FAILURE
        end select
    end function gipl_var_type

    ! The units of the given variable.
    function gipl_var_units(self, var_name, var_units) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        character (len = *), intent(out) :: var_units
        integer :: bmi_status

        select case(var_name)
        case("snowpack__depth")
            var_units = "m"
            bmi_status = BMI_SUCCESS
        case("land_surface_air__temperature")
            var_units = "C"
            bmi_status = BMI_SUCCESS
        case("snow__thermal_conductivity")
            var_units = "W m-1 K-1"
            bmi_status = BMI_SUCCESS
        case("soil__temperature")
            var_units = 'C'
        case("soil_water__volume_fraction")
            var_units = 'm3 m-3'
        case default
            var_units = "-"
            bmi_status = BMI_FAILURE
        end select
    end function gipl_var_units

    ! Memory use per array element.
    function gipl_var_itemsize(self, var_name, var_size) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        integer, intent(out) :: var_size
        integer :: bmi_status

        select case(var_name)
        case("land_surface_air__temperature")
            var_size = 1  ! 'sizeof' in gcc & ifort
            bmi_status = BMI_SUCCESS
        case("snowpack__depth")
            var_size = 1        ! 'sizeof' in gcc & ifort
            bmi_status = BMI_SUCCESS
        case("snow__thermal_conductivity")
            var_size = 1        ! 'sizeof' in gcc & ifort
            bmi_status = BMI_SUCCESS
        case("model_soil_layer__count")
            var_size = 1
        case("soil__temperature")
            var_size = (self%model%n_z)
        case("soil_water__volume_fraction")
            var_size = (self%model%n_lay)
        case("soil_unfrozen_water__a")
            var_size = (self%model%n_lay)
        case("soil_unfrozen_water__b")
            var_size = (self%model%n_lay)
        case default
            var_size = -1
            bmi_status = BMI_FAILURE
        end select
    end function gipl_var_itemsize

    ! The size of the given variable.
    function gipl_var_nbytes(self, var_name, var_nbytes) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        integer, intent(out) :: var_nbytes
        integer :: bmi_status
        integer :: s1, s2, s3, grid_id, grid_size, item_size

        s1 = self%get_var_grid(var_name, grid_id)
        s2 = self%get_grid_size(grid_id, grid_size)
        s3 = self%get_var_itemsize(var_name, item_size)

        if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
            var_nbytes = item_size * grid_size
            bmi_status = BMI_SUCCESS
        else
            var_nbytes = -1
            bmi_status = BMI_FAILURE
        end if
    end function gipl_var_nbytes

    ! Get a copy of a integer variable's values, flattened.
    function gipl_get_int(self, var_name, dest) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        integer, intent(inout) :: dest(:)
        integer :: bmi_status
        select case(var_name)
        case("write_or_not")
            dest = [self%model%write_outputs_or_not]
            bmi_status = BMI_SUCCESS
        case("model_soil_layer__count")
            dest = [self%model%n_z]
            bmi_status = BMI_SUCCESS
        case default
            dest = [-1]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_get_int

    ! Get a copy of a real variable's values, flattened.
    function gipl_get_float(self, var_name, dest) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        real, intent(inout) :: dest(:)
        integer :: bmi_status

        select case(var_name)
        case("land_surface_air__temperature")
            dest = [self%model%utemp(int(self%model % top_run_time), 1)]
            bmi_status = BMI_SUCCESS
        case("snowpack__depth")
            dest = [self%model%snd(int(self%model % top_run_time), 1)]
            bmi_status = BMI_SUCCESS
        case("snow__thermal_conductivity")
            dest = [self%model%stcon(int(self%model % top_run_time), 1)]
            bmi_status = BMI_SUCCESS
        case("soil__temperature")
            dest = [self%model%temp ]
            bmi_status = BMI_SUCCESS
        case('soil_water__volume_fraction')
            dest = [self%model%vwc]
            bmi_status = BMI_SUCCESS
        case('soil_unfrozen_water__a')
            dest = [self%model%a_coef]
            bmi_status = BMI_SUCCESS
        case('soil_unfrozen_water__b')
            dest = [self%model%b_coef]
            bmi_status = BMI_SUCCESS
        case default
            dest = [-1.0]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_get_float

    ! Get a copy of a double variable's values, flattened.
    function gipl_get_double(self, var_name, dest) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        double precision, intent(inout) :: dest(:)
        integer :: bmi_status

        select case(var_name)

        case default
            dest = [-1.d0]
            bmi_status = BMI_FAILURE
        end select
    end function gipl_get_double

    ! Get a reference to an integer-valued variable, flattened.
    function gipl_get_ptr_int(self, var_name, dest) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        integer, pointer, intent(inout) :: dest(:)
        integer :: bmi_status
        type (c_ptr) :: src
        integer :: n_elements

        select case(var_name)
        case default
            bmi_status = BMI_FAILURE
        end select
    end function gipl_get_ptr_int

    ! Get a reference to a real-valued variable, flattened.
    function gipl_get_ptr_float(self, var_name, dest) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        real, pointer, intent(inout) :: dest(:)
        integer :: bmi_status
        type (c_ptr) :: src
        integer :: n_elements

        return
    end function gipl_get_ptr_float

    ! Get a reference to an double-valued variable, flattened.
    function gipl_get_ptr_double(self, var_name, dest) result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        double precision, pointer, intent(inout) :: dest(:)
        integer :: bmi_status
        type (c_ptr) :: src
        integer :: n_elements

        select case(var_name)
        case default
            bmi_status = BMI_FAILURE
        end select
    end function gipl_get_ptr_double

    ! Get values of an integer variable at the given locations.
    function gipl_get_at_indices_int(self, var_name, dest, indices) &
            result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        integer, intent(inout) :: dest(:)
        integer, intent(in) :: indices(:)
        integer :: bmi_status
        type (c_ptr) src
        integer, pointer :: src_flattened(:)
        integer :: i, n_elements

        select case(var_name)
        case default
            bmi_status = BMI_FAILURE
        end select
    end function gipl_get_at_indices_int

    ! Get values of a real variable at the given locations.
    function gipl_get_at_indices_float(self, var_name, dest, indices) &
            result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        real, intent(inout) :: dest(:)
        integer, intent(in) :: indices(:)
        integer :: bmi_status
        type (c_ptr) src
        real, pointer :: src_flattened(:)
        integer :: i, n_elements
        select case(var_name)
        end select
        return
    end function gipl_get_at_indices_float

    ! Get values of a double variable at the given locations.
    function gipl_get_at_indices_double(self, var_name, dest, indices) &
            result (bmi_status)
        class (bmi_gipl), intent(in) :: self
        character (len = *), intent(in) :: var_name
        double precision, intent(inout) :: dest(:)
        integer, intent(in) :: indices(:)
        integer :: bmi_status
        type (c_ptr) src
        double precision, pointer :: src_flattened(:)
        integer :: i, n_elements

        select case(var_name)
        case default
            bmi_status = BMI_FAILURE
        end select
    end function gipl_get_at_indices_double

    ! Set new integer values.
    function gipl_set_int(self, var_name, src) result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        character (len = *), intent(in) :: var_name
        integer, intent(in) :: src(:)
        integer :: bmi_status

        select case(var_name)
        case("write_or_not")
            self%model%write_outputs_or_not = src(1)
            bmi_status = BMI_SUCCESS
        case default
            bmi_status = BMI_FAILURE
        end select

    end function gipl_set_int

    ! Set new real values.
    function gipl_set_float(self, var_name, src) result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        character (len = *), intent(in) :: var_name
        real, intent(in) :: src(:)
        integer :: bmi_status

        select case(var_name)
        case("land_surface_air__temperature")
            self%model%utemp(int(self%model % top_run_time), 1) = src(1)
            bmi_status = BMI_SUCCESS
        case("snowpack__depth")
            self%model%snd(int(self%model % top_run_time), 1) = src(1)
            bmi_status = BMI_SUCCESS
        case("snow__thermal_conductivity")
            self%model%stcon(int(self%model % top_run_time), 1) = src(1)
            bmi_status = BMI_SUCCESS
        case('soil_water__volume_fraction')
            print*, "please use [set_value_at_indices]"
            bmi_status = BMI_FAILURE
        case('soil_unfrozen_water__a')
            print*, "please use [set_value_at_indices]"
            bmi_status = BMI_FAILURE
        case('soil_unfrozen_water__b')
            print*, "please use [set_value_at_indices]"
            bmi_status = BMI_FAILURE
        case default
            bmi_status = BMI_FAILURE
        end select

    end function gipl_set_float

    ! Set new double values.
    function gipl_set_double(self, var_name, src) result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        character (len = *), intent(in) :: var_name
        double precision, intent(in) :: src(:)
        integer :: bmi_status

        select case(var_name)
        case default
            bmi_status = BMI_FAILURE
        end select
    end function gipl_set_double

    ! Set integer values at particular locations.
    function gipl_set_at_indices_int(self, var_name, indices, src) &
            result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        character (len = *), intent(in) :: var_name
        integer, intent(in) :: indices(:)
        integer, intent(in) :: src(:)
        integer :: bmi_status
        type (c_ptr) dest
        integer, pointer :: dest_flattened(:)
        integer :: i

        select case(var_name)
        case default
            bmi_status = BMI_FAILURE
        end select
    end function gipl_set_at_indices_int

    ! Set real values at particular locations.
    function gipl_set_at_indices_float(self, var_name, indices, src) &
            result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        character (len = *), intent(in) :: var_name
        integer, intent(in) :: indices(:)
        real, intent(in) :: src(:)
        integer :: bmi_status
        type (c_ptr) dest
        real, pointer :: dest_flattened(:)
        integer :: i

        select case(var_name)
        case("soil_water__volume_fraction")
            if (maxval(indices) .le. self%model%n_lay) then
                self%model%vwc(indices, 1) = src(:)
            else
                bmi_status = BMI_FAILURE
            end if
        case("soil_unfrozen_water__a")
            if (maxval(indices) .le. self%model%n_lay) then
                self%model%a_coef(indices, 1) = src(:)
            else
                bmi_status = BMI_FAILURE
            end if
        case("soil_unfrozen_water__b")
            if (maxval(indices) .le. self%model%n_lay) then
                self%model%b_coef(indices, 1) = src(:)
            else
                bmi_status = BMI_FAILURE
            end if
        case default
            bmi_status = BMI_FAILURE
        end select
        return
    end function gipl_set_at_indices_float

    ! Set double values at particular locations.
    function gipl_set_at_indices_double(self, var_name, indices, src) &
            result (bmi_status)
        class (bmi_gipl), intent(inout) :: self
        character (len = *), intent(in) :: var_name
        integer, intent(in) :: indices(:)
        double precision, intent(in) :: src(:)
        integer :: bmi_status
        type (c_ptr) dest
        double precision, pointer :: dest_flattened(:)
        integer :: i

        select case(var_name)
        case default
            bmi_status = BMI_FAILURE
        end select
    end function gipl_set_at_indices_double

    ! A non-BMI procedure for model introspection.
    subroutine print_model_info(self)
        class (bmi_gipl), intent(in) :: self

        call print_info(self%model)

    end subroutine print_model_info

end module bmigiplf

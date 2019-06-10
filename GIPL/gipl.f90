! Geophysical Instatitue Permafrost Laboratory model version 2 GIPLv2
! version 2 is a numerical transient model that employs phase changes and the effect of the unfrozen volumetric  water content in the non-homogeniuos soil texture 
! Original version of the model developed by Romanovsky and Tipenko 2004 and described in Marchenko et al., (2008)
! Current version been significanlty modefied from its predicessor and using the IRF coding design
! This version is maintained by E. Jafarov at INSTAAR, CU Boulder
! Please cite Jafarov et al., (2012) work when using it.
module const

    real*8, parameter :: hcap_snow = 840000.0 ! heat capacity of snow (constant)
    real*8, parameter :: Lf = 333.2 * 1.D+6 ! Latent of water fusion
    integer, parameter :: lbound = 2 ! 1 const temp, 2 heat flux condition at the bottom boundary
    integer, parameter :: n_lay = 10 ! total allowed number of soil layer

end module const

module bnd
    integer :: n_temp ! number of upper boundary points for temperature (input)
    real*8, allocatable :: utemp_time(:), utemp(:, :) ! upper boundary time and temperature (input)
    real*8, allocatable :: utemp_time_i(:), utemp_i(:, :) ! time and upper boundary temprature (interpolated)
    integer :: n_snow ! number of upper boundary points for snow (input)
    real*8, allocatable :: snd_time(:), snd(:, :) ! upper boundary snow time and snow depth (input)
    integer :: n_stcon
    real*8, allocatable :: stcon_time(:), stcon(:, :) ! snow thermal conductivity time and itself (input)
    real*8, allocatable :: snd_i(:, :), stcon_i(:, :) ! snow depth and thermal conductivity (interpolated)
    real*8 :: TINIR
    real*8 :: time_restart ! restart time in restart file
    integer, allocatable :: idx_site(:)

    real*8 :: time_s, time_e ! internal start and end times

    ! Parameter read from cmd file
    integer :: restart ! 0/1 start from previous time step / start from the begining
    real*8 :: time_step ! step is the timestep in the example it is 1 yr
    real*8 :: TAUM ! taum is the convergence parameter used by the stefan subroutine
    real*8 :: TMIN ! tmin minimal timestep used in the Stefan subroutine
    real*8 :: time_beg, time_end ! inbegin time, end time
    integer :: itmax ! maximum number of iterations in Stefan subroutine
    integer :: n_time ! number of time steps that temp will be averaged over
    integer :: n_frz_max ! maximum number of freezing fronts
    real*8 :: smooth_coef ! smoothing factor
    real*8 :: unf_water_coef ! unfrozen water coefficient
    real*8 :: n_sec_day ! number of second in a day
    real*8 :: frz_frn_max, frz_frn_min ! freezing front min and max depth [meters]
    real*8 :: sat_coef ! saturation coefficient [dimensionless, fraction of 1]
    ! output file names
    character(164) :: restart_file, result_file, aver_res_file

    type site_gipl
        real*8 :: time
    end type site_gipl

end module bnd

module thermo
    real*8 L_fus ! Latent heat of fusion [W/mK]
    real*8 sea_level ! how many meter above the sea level the borehole is

    ! thermo physical parameters of soil for each soil layer
    real*8, allocatable :: vwc(:, :) ! volumetric water content
    real*8, allocatable :: a_coef(:, :), b_coef(:, :) ! a and b unfrozen water curve coefficients
    real*8, allocatable :: temp_frz(:, :) ! temperature freezing depression
    real*8, allocatable :: EE(:, :)
    real*8, allocatable :: hcap_frz(:, :), hcap_thw(:, :) ! soil layer heat capacity thawed/frozen
    real*8, allocatable :: tcon_frz(:, :), tcon_thw(:, :) ! soil layer thermal conductivity thawed/frozen

    real*8 :: hcap_s ! heat capacity of snow (constant) nondimentional

    real*8, allocatable :: temp(:, :) ! soil temperature
    real, allocatable :: n_bnd_lay(:, :) ! number of boundaries between layer in soil
    integer k0

    integer, allocatable :: snow_code(:), veg_code(:) ! (not necccessary) required for runing in parallel
    integer, allocatable :: geo_code(:), gt_zone_code(:) ! (not necccessary) required for runing in parallel
    real*8, allocatable :: temp_grd(:) ! temprature gradient at the lower boundary

    real*8, allocatable :: RES(:, :) ! unified variable for the writing results into the file

end module thermo

module grd

    integer, allocatable :: n_lay_cur(:) ! current number of soil layers <= n_lay
    ! calclulated as a sum of organic and mineral soil layers
    integer :: n_site ! number of sites
    integer :: n_grd ! total number of grid points with depth (grid.txt)
    real*8, allocatable :: zdepth(:), dz(:) ! vertical grid and distance between grid point 'zdepth(n_grd)'
    integer, allocatable :: lay_id(:, :) ! layer index
    integer :: m_grd ! number of grid points to store in res file
    integer, allocatable :: zdepth_id(:) ! index vector of stored grid points 'zdepth_id(m_grid)'
    integer :: n_ini ! number of vertical grid cells in init file
    real*8, allocatable :: zdepth_ini(:), ztemp_ini(:, :) ! depth and correspoding initial temperature (time=0) 'zdepth_ini(n_ini)'
    character(210) :: FMT1, FMT2 ! results formating type

end module grd

module alt
    integer, allocatable :: n_frz_frn(:, :) ! number of freezing front (e.g. when freezup is about to happened)
    integer, allocatable :: i_time(:) ! internal time step with the the main loop
    real*8, allocatable :: z_frz_frn(:, :, :) ! depth of the freezing front
end module alt

module gipl_model

    !    implicit none

    type :: gipl_model_type

        real*8 :: top_run_time
        integer :: write_outputs_or_not !1: write out to file, 0: not.
        real*8 :: dt
        integer :: n_x, n_y, n_z
        real*8 :: dx, dy, dz0

        real*8 :: tair_cur, snd_cur, stcon_cur

        integer :: initialize_status !1: pass, 0: failure.

        ! copy from const

        real*8 :: hcap_snow ! heat capacity of snow (constant)
        real*8 :: Lf ! Latent of water fusion
        integer :: lbound  ! 1 const temp, 2 heat flux condition at the bottom boundary
        integer :: n_lay  ! total allowed number of soil layer

        ! copy from bnd

        integer :: n_temp ! number of upper boundary points for temperature (input)
        real*8, allocatable :: utemp_time(:), utemp(:, :) ! upper boundary time and temperature (input)
        real*8, allocatable :: utemp_time_i(:), utemp_i(:, :) ! time and upper boundary temprature (interpolated)
        integer :: n_snow ! number of upper boundary points for snow (input)
        real*8, allocatable :: snd_time(:), snd(:, :) ! upper boundary snow time and snow depth (input)
        integer :: n_stcon
        real*8, allocatable :: stcon_time(:), stcon(:, :) ! snow thermal conductivity time and itself (input)
        real*8, allocatable :: snd_i(:, :), stcon_i(:, :) ! snow depth and thermal conductivity (interpolated)
        real*8 :: TINIR
        real*8 :: time_restart ! restart time in restart file
        integer, allocatable :: idx_site(:)
        real*8 :: time_s, time_e ! internal start and end times

        ! Parameter read from cmd file
        integer :: restart ! 0/1 start from previous time step / start from the begining
        real*8 :: time_step ! step is the timestep in the example it is 1 yr
        real*8 :: TAUM ! taum is the convergence parameter used by the stefan subroutine
        real*8 :: TMIN ! tmin minimal timestep used in the Stefan subroutine
        real*8 :: time_beg, time_end ! inbegin time, end time
        integer :: itmax ! maximum number of iterations in Stefan subroutine
        integer :: n_time ! number of time steps that temp will be averaged over
        integer :: n_frz_max ! maximum number of freezing fronts
        real*8 :: smooth_coef ! smoothing factor
        real*8 :: unf_water_coef ! unfrozen water coefficient
        real*8 :: n_sec_day ! number of second in a day
        real*8 :: frz_frn_max, frz_frn_min ! freezing front min and max depth [meters]
        real*8 :: sat_coef ! saturation coefficient [dimensionless, fraction of 1]

        ! copy from grid

        integer, allocatable :: n_lay_cur(:) ! current number of soil layers <= n_lay
        ! calclulated as a sum of organic and mineral soil layers
        integer :: n_site ! number of sites
        integer :: n_grd ! total number of grid points with depth (grid.txt)
        real*8, allocatable :: zdepth(:), dz(:) ! vertical grid and distance between grid point 'zdepth(n_grd)'
        integer, allocatable :: lay_id(:, :) ! layer index
        integer :: m_grd ! number of grid points to store in res file
        integer, allocatable :: zdepth_id(:) ! index vector of stored grid points 'zdepth_id(m_grid)'
        integer :: n_ini ! number of vertical grid cells in init file
        real*8, allocatable :: zdepth_ini(:), ztemp_ini(:, :) ! depth and correspoding initial temperature (time=0) 'zdepth_ini(n_ini)'
        character(210) :: FMT1, FMT2 ! results formating type

        ! copy from thermo

        real*8 L_fus ! Latent heat of fusion [W/mK]
        real*8 sea_level ! how many meter above the sea level the borehole is

        ! thermo physical parameters of soil for each soil layer
        real*8, allocatable :: vwc(:, :) ! volumetric water content
        real*8, allocatable :: a_coef(:, :), b_coef(:, :) ! a and b unfrozen water curve coefficients
        real*8, allocatable :: temp_frz(:, :) ! temperature freezing depression
        real*8, allocatable :: EE(:, :)
        real*8, allocatable :: hcap_frz(:, :), hcap_thw(:, :) ! soil layer heat capacity thawed/frozen
        real*8, allocatable :: tcon_frz(:, :), tcon_thw(:, :) ! soil layer thermal conductivity thawed/frozen

        real*8 :: hcap_s ! heat capacity of snow (constant) nondimentional

        real*8, allocatable :: temp(:, :) ! soil temperature
        real, allocatable :: n_bnd_lay(:, :) ! number of boundaries between layer in soil
        integer k0

        integer, allocatable :: snow_code(:), veg_code(:) ! (not necccessary) required for runing in parallel
        integer, allocatable :: geo_code(:), gt_zone_code(:) ! (not necccessary) required for runing in parallel
        real*8, allocatable :: temp_grd(:) ! temprature gradient at the lower boundary

        real*8, allocatable :: RES(:, :) ! unified variable for the writing results into the file

        !copy from ALT

        integer, allocatable :: n_frz_frn(:, :) ! number of freezing front (e.g. when freezup is about to happened)
        integer, allocatable :: i_time(:) ! internal time step with the the main loop
        real*8, allocatable :: z_frz_frn(:, :, :) ! depth of the freezing front

    end type gipl_model_type

contains

    subroutine run_model
        use const
        use bnd
        use thermo
        use grd
        use alt

        implicit none

        type(gipl_model_type) :: model

        ! variables
        real*8 :: res_save(m_grd + 3, n_site) ! save results into 2D array
        real*8 :: dfrz_frn(n_time) ! depth of the freezing front
        real :: frz_up_time_cur ! freezeup time current (within a year)
        real :: frz_up_time_tot ! freezeup time global
        ! counters (time,steps)
        !        real*8 :: time_s, time_e ! internal start and end times
        real*8 :: time_loop(n_site) ! main looping time
        real*8 :: time_cur(n_site) ! current time (e.g. current day)
        ! other counters
        integer :: i_site, j_time, i_grd, i_lay
        integer :: ierr

        !        time_s = time_step * DBLE(n_time * time_beg)
        !        time_e = time_step * DBLE(n_time * time_end)
        !
        !        i_time = 1
        !        time_loop = 0.0D0
        !        TINIR = 0.0D0

        do while (time_loop(1) .LT. time_e)
            do i_site = 1, n_site
                time_cur(i_site) = time_loop(i_site) + time_restart
                call save_results(i_site, time_cur(i_site), time_loop(i_site))
                6666 continue
                !do while (i_time(i_site).LT.n_time)
                call stefan1D(temp(i_site, :), n_grd, dz, time_loop(i_site), i_site, lay_id(i_site, :), &
                        temp_grd(i_site))
                time_loop(i_site) = time_loop(i_site) + time_step
                time_cur(i_site) = time_loop(i_site) + time_restart
                if (i_time(i_site) .LT. n_time) then
                    i_time(i_site) = i_time(i_site) + 1
                    call save_results(i_site, time_cur(i_site), time_loop(i_site))
                    call active_layer(model, i_site)
                    GOTO 6666
                endif
                !enddo
                if (time_s .LT. time_e.AND.time_loop(1) .GT. time_s)then
                    do j_time = 1, n_time ! WRITTING RESULTS
                        write(1, FMT1) idx_site(i_site), (RES(j_time, i_grd), i_grd = 1, m_grd + 3)
                    enddo
                endif
                do i_grd = 1, m_grd + 3
                    res_save(i_grd, i_site) = sum((RES(:, i_grd)))
                enddo
            enddo
            i_time = 1
            do i_site = 1, n_site
                frz_up_time_cur = -7777.D0
                frz_up_time_tot = frz_up_time_cur
                do j_time = 2, n_time
                    if ((n_frz_frn(j_time, i_site) - n_frz_frn(j_time - 1, i_site)) .EQ. -2)then
                        if (z_frz_frn(j_time - 1, n_frz_frn(j_time - 1, i_site), i_site) .GE. frz_frn_min) then
                            frz_up_time_cur = SNGL(RES(j_time, 1))
                        endif
                    endif
                enddo

                if (frz_up_time_cur .GT. 0.0)then
                    frz_up_time_tot = AMOD(frz_up_time_cur, REAL(n_time))
                    if (frz_up_time_tot .EQ. 0.0) frz_up_time_tot = REAL(n_time)
                endif

                dfrz_frn = z_frz_frn(:, 1, i_site)
                call save_results(i_site, time_cur(i_site), time_loop(i_site))
                call active_layer(model, i_site)

                !____WRITTING MEAN
                write(2, FMT2) i_site, (res_save(i_grd, i_site) / DBLE(n_time), i_grd = 1, m_grd + 3), &
                        dfrz_frn(n_time), frz_up_time_cur, frz_up_time_tot
                do j_time = 1, n_time + 2
                    utemp_time_i(j_time) = time_cur(1) + DBLE(j_time - 1) * time_step
                enddo
                call interpolate(utemp_time, utemp(:, i_site), n_temp, utemp_time_i, utemp_i(:, i_site), n_time + 2)
                call interpolate(snd_time, snd(:, i_site), n_snow, utemp_time_i, snd_i(:, i_site), n_time + 2)
                call snowfix(model, utemp_i(:, i_site), snd_i(:, i_site), n_time + 2)
                call interpolate(stcon_time, stcon(:, i_site), n_stcon, utemp_time_i, stcon_i(:, i_site), n_time + 2)
            enddo

            TINIR = time_loop(1)
        enddo

    end subroutine run_model

    subroutine update(model)

        use const
        use bnd
        use thermo
        use grd
        use alt

        implicit none

        type(gipl_model_type) :: model

        ! variables
        real*8 :: res_save(m_grd + 3, n_site) ! save results into 2D array
        real*8 :: dfrz_frn(n_time) ! depth of the freezing front
        real :: frz_up_time_cur ! freezeup time current (within a year)
        real :: frz_up_time_tot ! freezeup time global
        ! counters (time,steps)

        real*8 :: time_loop(n_site) ! main looping time
        real*8 :: time_cur(n_site) ! current time (e.g. current day)
        ! other counters
        integer :: i_site, j_time, i_grd, i_lay
        integer :: ierr

        real*8 :: hcscale

        !=========
        ! The follow lines are alternative solution to pass changes to the model.
        ! but interpolate function seems like to bring different results
        ! To be checked. (Kang, 2019-6-6)
        ! Seems function 'SNOWFIX' is that problem. Remove this, the example outputs
        ! are same. To be confirmed. (Kang, 2019-6-7)

        if (abs(time_step - model%time_step) .ne. 0) then
            time_step = model%time_step
        endif

        if ((maxval(abs(utemp - model%utemp)) .ne. 0) .or. &
                (maxval(abs(snd - model%snd)) .ne. 0) .or. &
                (maxval(abs(stcon - model%stcon)) .ne. 0)) then

            utemp = model%utemp
            snd = model%snd
            stcon = model%stcon

            do j_time = 1, n_time + 2
                utemp_time_i(j_time) = 1 + DBLE(j_time - 1) * time_step
            enddo
            !
            do i_site = 1, n_site
                call interpolate(utemp_time, utemp(:, i_site), n_temp, utemp_time_i, utemp_i(:, i_site), n_time + 2)
                call interpolate(snd_time, snd(:, i_site), n_snow, utemp_time_i, snd_i(:, i_site), n_time + 2)
                !        call snowfix(model, utemp_i(:, i_site), snd_i(:, i_site), n_time + 2)
                call interpolate(stcon_time, stcon(:, i_site), n_stcon, utemp_time_i, stcon_i(:, i_site), n_time + 2)
            enddo

        end if
        !=========

        ! The follow lines are pass soil parameter changes to the model.
        if ((maxval(abs(vwc - model%vwc)) .ne. 0) .or. &
                (maxval(abs(a_coef - model%a_coef)) .ne. 0) .or. &
                (maxval(abs(b_coef - model%b_coef)) .ne. 0) .or. &
                (maxval(abs(hcap_frz - model%hcap_frz)) .ne. 0) .or. &
                (maxval(abs(hcap_thw - model%hcap_thw)) .ne. 0) .or. &
                (maxval(abs(tcon_frz - model%tcon_frz)) .ne. 0) .or. &
                (maxval(abs(tcon_thw - model%tcon_thw)) .ne. 0)) then

            vwc = model%vwc
            a_coef = model%a_coef
            b_coef = model%b_coef
            hcap_frz = model%hcap_frz
            hcap_thw = model%hcap_thw
            tcon_frz = model%tcon_frz
            tcon_thw = model%tcon_thw

            ! recalculate phase change temperature by using vwc, and unfrozen water a & b

            do i_site = 1, n_site
                do i_lay = 1, n_lay_cur(i_site)
                    temp_frz(i_lay, i_site) = -(vwc(i_lay, i_site) / a_coef(i_lay, i_site))**(1.d0 / b_coef(i_lay, i_site))
                enddo
            end do

            hcscale = zdepth(n_grd) * zdepth(n_grd) / n_sec_day

            ! updating scales ... ...

            hcap_frz = hcap_frz * hcscale
            hcap_thw = hcap_thw * hcscale
            hcap_s = hcap_snow * hcscale
            L_fus = Lf * hcscale

        end if

        !=========

        !        i_time = 1

        if (model % top_run_time .eq. 1) then
            time_loop = model % top_run_time - 1
            ! Initialize the results array
            do i_site = 1, n_site
                time_cur(i_site) = time_loop(i_site) + time_restart
                call save_results(i_site, time_cur(i_site), time_loop(i_site))
            enddo

            if (model % top_run_time - 1 .eq. 0) then ! write the first time step.
                if (model % write_outputs_or_not .eq. 1) then
                    do i_site = 1, n_site
                        do j_time = int(model % top_run_time), int(model % top_run_time) ! WRITTING RESULTS
                            write(1, FMT1) idx_site(i_site), (RES(j_time, i_grd), i_grd = 1, m_grd + 3)
                        enddo
                    enddo
                endif
            endif

        else
            time_loop = model % top_run_time - 2
            do i_site = 1, n_site

                time_cur(i_site) = time_loop(i_site) + time_restart

                call stefan1D(temp(i_site, :), n_grd, dz, time_loop(i_site), i_site, lay_id(i_site, :), &
                        temp_grd(i_site))

                time_loop(i_site) = time_loop(i_site) + time_step
                time_cur(i_site) = time_loop(i_site) + time_restart

                i_time(i_site) = i_time(i_site) + 1
                call save_results(i_site, time_cur(i_site), time_loop(i_site))
                model % temp = temp

            enddo

        endif

        if (model % write_outputs_or_not .eq. 1) then
            if (time_s .LT. time_e.AND.time_loop(1) .GT. time_s)then
                do i_site = 1, n_site
                    do j_time = int(model % top_run_time), int(model % top_run_time) ! WRITTING RESULTS
                        write(1, FMT1) idx_site(i_site), (RES(j_time, i_grd), i_grd = 1, m_grd + 3)
                    enddo
                enddo
            endif
        endif

        model % tair_cur = utemp(model % top_run_time, 1)
        model % snd_cur = snd(model % top_run_time, 1)
        model % stcon_cur = stcon(model % top_run_time, 1)

        model % top_run_time = model % top_run_time + 1 * time_step

        model % RES = RES

    end subroutine update

    subroutine initialize(model, fconfig)

        use const
        use bnd
        use thermo
        use grd
        use alt

        implicit none

        type(gipl_model_type) :: model

        integer IREAD, ierr, status
        integer :: i, j, k, z_num, i_grd, j_time, i_site, i_lay

        real*8, allocatable :: gtzone(:, :)
        character*64 stdummy
        character*164 fconfig

        character*164 file_sites, file_bound, file_snow, file_rsnow, file_init
        character*164 file_grid, file_organic, file_mineral

        real*8, allocatable :: A1(:, :), A2(:, :), A3(:, :), A4(:, :), A5(:, :)
        real*8, allocatable :: A6(:, :), A7(:, :), A8(:, :), A9(:, :), A10(:, :)
        integer, allocatable :: veg_class(:), num_vl(:)
        integer :: vln

        real*8, allocatable :: B1(:, :), B2(:, :), B3(:, :), B4(:, :), B5(:, :)
        real*8, allocatable :: B6(:, :), B7(:, :), B8(:, :)
        integer, allocatable :: geo_class(:), num_gl(:)
        real*8 :: layer_thick
        integer :: gln
        real*8, allocatable :: z(:) ! vertical grid
        real*8 :: hcscale

        call filexist(fconfig, status)
        if (status .eq. 1) then
            open(60, file = fconfig)
            !read input files
            read(60, '(A)') stdummy
            read(60, '(A)') file_sites
            read(60, '(A)') file_bound
            read(60, '(A)') file_snow
            read(60, '(A)') file_rsnow
            read(60, '(A)') file_init
            !read(60,'(A)')cmdf
            read(60, '(A)') file_grid
            read(60, '(A)') file_organic
            read(60, '(A)') file_mineral

            ! read output files
            read(60, '(A)') stdummy
            read(60, '(A)') stdummy
            read(60, '(A)') aver_res_file
            read(60, '(A)') result_file
            read(60, '(A)') restart_file

            ! read input parameters
            read(60, '(A)') stdummy
            read(60, '(A)') stdummy
            read(60, *) restart
            read(60, '(A)') stdummy
            read(60, *) time_step, TAUM, TMIN
            read(60, '(A)') stdummy
            read(60, *) time_beg, time_end
            read(60, '(A)') stdummy
            read(60, *) smooth_coef, unf_water_coef, itmax !smoothing factor | unfrozen water parameter | max number of iterations
            read(60, '(A)') stdummy
            read(60, *) n_sec_day, n_time ! number of second in a day [sec] | number of time steps (in the example number of days in a year )
            read(60, '(A)') stdummy
            read(60, *) sea_level, n_frz_max
            read(60, '(A)') stdummy
            read(60, *) frz_frn_min, frz_frn_max
            read(60, '(A)') stdummy
            read(60, *) sat_coef

            close(60)

            ! pass values to model interface:

            model%n_sec_day = n_sec_day
            model%restart = restart
            model%time_step = time_step
            model%n_time = n_time

            call filexist(file_sites, status)
            if (status .eq. 1) then
                call filexist(file_bound, status)
                if (status .eq. 1) then
                    call filexist(file_snow, status)
                    if (status .eq. 1) then
                        call filexist(file_rsnow, status)
                        if (status .eq. 1) then
                            call filexist(file_grid, status)
                            if (status .eq. 1) then
                                call filexist(file_init, status)
                                if (status .eq. 1) then
                                    call filexist(file_mineral, status)
                                    if (status .eq. 1) then
                                        call filexist(file_organic, status)
                                        if (status .eq. 1) then

                                            open(60, FILE = file_sites)
                                            read(60, *) n_site
                                            allocate(snow_code(n_site), STAT = IERR)
                                            allocate(veg_code(n_site), STAT = IERR)
                                            allocate(geo_code(n_site), STAT = IERR)
                                            allocate(gt_zone_code(n_site), STAT = IERR)
                                            allocate(temp_grd(n_site), STAT = IERR)
                                            allocate(idx_site(n_site), STAT = IERR)
                                            read(60, *)
                                            do i_site = 1, n_site
                                                read(60, *) IREAD, snow_code(i_site), veg_code(i_site), geo_code(i_site), &
                                                        gt_zone_code(i_site), temp_grd(i_site)

                                                idx_site(i_site) = IREAD

                                            enddo
                                            close(60)
                                            !   print*, trim(file_sites),' has been read'

                                            model%n_site = n_site

                                            open(60, file = file_bound)
                                            read(60, *) n_temp
                                            allocate(utemp_time(n_temp), STAT = IERR)
                                            allocate(utemp(n_temp, n_site), STAT = IERR)
                                            do i = 1, n_temp
                                                read(60, *) utemp_time(I), (utemp(I, i_site), i_site = 1, n_site)
                                            enddo
                                            close(60)
                                            !   print*,trim(file_bound),' has been read'

                                            ! pass upper boundary to model interface

                                            allocate(model%utemp_time(n_temp), STAT = IERR)
                                            allocate(model%utemp(n_temp, n_site), STAT = IERR)

                                            open(60, file = file_rsnow)
                                            read(60, *) n_stcon
                                            allocate(stcon_time(n_stcon), STAT = IERR)
                                            allocate(stcon(n_stcon, n_site), STAT = IERR)
                                            do i = 1, n_stcon
                                                read(60, *) stcon_time(i), (stcon(i, i_site), i_site = 1, n_site)
                                            enddo
                                            close(60)
                                            !   print*,trim(file_rsnow),' has been read'

                                            ! pass upper boundary to model interface

                                            allocate(model%stcon_time(n_stcon), STAT = IERR)
                                            allocate(model%stcon(n_stcon, n_site), STAT = IERR)

                                            open(60, file = file_snow)
                                            read(60, *) n_snow
                                            allocate(snd_time(n_snow), STAT = IERR)
                                            allocate(snd(n_snow, n_site), STAT = IERR)
                                            do I = 1, n_snow
                                                read(60, *) snd_time(i), (snd(i, i_site), i_site = 1, n_site)
                                            enddo
                                            close(60)
                                            !   print*,trim(file_snow),' has been read'

                                            ! pass upper boundary to model interface

                                            allocate(model%snd_time(n_snow), STAT = IERR)
                                            allocate(model%snd(n_snow, n_site), STAT = IERR)

                                            open(60, file = file_init, action = 'read')
                                            read(60, *) z_num, n_ini!,time_restart
                                            allocate(zdepth_ini(n_ini), STAT = IERR)
                                            allocate(ztemp_ini(n_ini, n_site), STAT = IERR)
                                            allocate(gtzone(n_ini, z_num + 1), STAT = IERR)
                                            read(60, *) stdummy
                                            do i = 1, n_ini
                                                read(60, *) (gtzone(i, j), j = 1, z_num + 1)
                                            enddo
                                            close(60)
                                            !   print*,trim(file_init),'has been read'

                                            allocate(model%zdepth_ini(n_ini), STAT = IERR)
                                            allocate(model%ztemp_ini(n_ini, n_site), STAT = IERR)

                                            time_restart = utemp_time(1)
                                            zdepth_ini(:) = gtzone(:, 1)
                                            do i = 1, n_site
                                                k = gt_zone_code(i)
                                                ztemp_ini(:, I) = gtzone(:, k + 1)
                                            enddo

                                            model%time_restart = time_restart

                                            open(60, file = file_grid)
                                            read(60, *) n_grd
                                            allocate(zdepth(n_grd), STAT = IERR)
                                            do i = 1, n_grd
                                                read(60, *) zdepth(i)
                                            enddo
                                            read(60, *) m_grd
                                            allocate(zdepth_id(m_grd), STAT = IERR)
                                            do j = 1, m_grd
                                                read(60, *) zdepth_id(j)
                                            enddo
                                            close(60)
                                            !   print*,trim(file_grid),' has been read'

                                            model%n_grd = n_grd
                                            model%m_grd = m_grd

                                            allocate(model%zdepth(n_grd), STAT = IERR)
                                            allocate(model%zdepth_id(m_grd), STAT = IERR)

                                            ! note: that all max n_lay_cur layers has to be read or it will a give segmantation error
                                            !      n_lay=10!MAXVAL(n_lay_cur)
                                            !----------------------------------------------------
                                            open (60, file = file_organic)
                                            read(60, *) vln ! reads numbers of  classes
                                            allocate(A1(n_lay, vln), STAT = IERR) ! vwc
                                            allocate(A2(n_lay, vln), STAT = IERR) ! a_coef
                                            allocate(A3(n_lay, vln), STAT = IERR) ! b_coef
                                            allocate(A4(n_lay, vln), STAT = IERR) ! hcap_frz
                                            allocate(A5(n_lay, vln), STAT = IERR) !hcap_thw
                                            allocate(A6(n_lay, vln), STAT = IERR) !tcon_frz
                                            allocate(A7(n_lay, vln), STAT = IERR) !tcon_thw
                                            allocate(A8(vln, n_lay), STAT = IERR) !bot_cond
                                            allocate(veg_class(vln), STAT = IERR) !veg_class
                                            allocate(num_vl(vln), STAT = IERR) !num_vl number of vegetation layers
                                            do I = 1, vln
                                                read(60, *) veg_class(i), num_vl(i)
                                                do j = 1, num_vl(i)
                                                    read(60, *) A1(J, I), A2(J, I), A3(J, I), &
                                                            A4(J, I), A5(J, I), A6(J, I), A7(J, I), A8(I, J)
                                                enddo
                                            enddo
                                            close(60)
                                            !   print*,trim(file_organic),' has been read'

                                            open (60, file = file_mineral)
                                            read(60, *) gln ! reads numbers of  classes
                                            allocate(B1(n_lay, gln), STAT = IERR) ! vwc
                                            allocate(B2(n_lay, gln), STAT = IERR) ! a_coef
                                            allocate(B3(n_lay, gln), STAT = IERR) ! b_coef
                                            allocate(B4(n_lay, gln), STAT = IERR) ! hcap_frz
                                            allocate(B5(n_lay, gln), STAT = IERR) !hcap_thw
                                            allocate(B6(n_lay, gln), STAT = IERR) !tcon_frz
                                            allocate(B7(n_lay, gln), STAT = IERR) !tcon_thw
                                            allocate(B8(gln, n_lay), STAT = IERR) !bot_cond
                                            allocate(geo_class(gln), STAT = IERR) !geo_class
                                            allocate(num_gl(gln), STAT = IERR) !num_vl number of lithologic layers
                                            do I = 1, gln
                                                read(60, *) geo_class(i), num_gl(i)
                                                do j = 1, num_gl(i)
                                                    read(60, *) B1(J, I), B2(J, I), B3(J, I), &
                                                            B4(J, I), B5(J, I), B6(J, I), B7(J, I), B8(I, J)
                                                enddo
                                            enddo
                                            close(60)
                                            !      print*,trim(file_mineral),' has been read'

                                            allocate(vwc(n_lay, n_site), STAT = IERR)
                                            allocate(a_coef(n_lay, n_site), STAT = IERR)
                                            allocate(b_coef(n_lay, n_site), STAT = IERR)
                                            allocate(EE(n_lay, n_site), STAT = IERR)
                                            allocate(hcap_frz(n_lay, n_site), STAT = IERR)
                                            allocate(hcap_thw(n_lay, n_site), STAT = IERR)
                                            allocate(tcon_frz(n_lay, n_site), STAT = IERR)
                                            allocate(tcon_thw(n_lay, n_site), STAT = IERR)
                                            allocate(n_lay_cur(n_site), STAT = IERR)
                                            allocate(n_bnd_lay(n_site, n_lay + 1), STAT = IERR)

                                            allocate(model%vwc(n_lay, n_site), STAT = IERR)
                                            allocate(model%a_coef(n_lay, n_site), STAT = IERR)
                                            allocate(model%b_coef(n_lay, n_site), STAT = IERR)
                                            allocate(model%EE(n_lay, n_site), STAT = IERR)
                                            allocate(model%hcap_frz(n_lay, n_site), STAT = IERR)
                                            allocate(model%hcap_thw(n_lay, n_site), STAT = IERR)
                                            allocate(model%tcon_frz(n_lay, n_site), STAT = IERR)
                                            allocate(model%tcon_thw(n_lay, n_site), STAT = IERR)
                                            allocate(model%n_lay_cur(n_site), STAT = IERR)
                                            allocate(model%n_bnd_lay(n_site, n_lay + 1), STAT = IERR)

                                            do i = 1, n_site
                                                layer_thick = 0
                                                n_bnd_lay(i, 1) = layer_thick
                                                layer_thick = 0
                                                n_bnd_lay(i, 1) = layer_thick
                                                do j = 1, num_vl(veg_code(i))
                                                    vwc(J, I) = A1(j, veg_code(i));
                                                    a_coef(J, I) = A2(j, veg_code(i));
                                                    b_coef(J, I) = A3(j, veg_code(i));
                                                    hcap_thw(J, I) = A4(j, veg_code(i));
                                                    hcap_frz(J, I) = A5(j, veg_code(i));
                                                    tcon_thw(J, I) = A6(j, veg_code(i));
                                                    tcon_frz(J, I) = A7(j, veg_code(i));
                                                    if (j .eq. 1) then
                                                        layer_thick = A8(veg_code(i), j)
                                                    else
                                                        layer_thick = layer_thick + A8(veg_code(i), j);
                                                    endif
                                                    n_bnd_lay(i, j + 1) = layer_thick
                                                    EE(J, I) = 0
                                                    !	     write(*,'(3(f8.3),2(f12.1),3(f8.3))') vwc(J,I),a_coef(J,I),b_coef(J,I), &
                                                    !		  hcap_thw(J,I),hcap_frz(J,I),tcon_thw(J,I),tcon_frz(J,I),n_bnd_lay(i,j+1)
                                                enddo
                                                k = 1
                                                n_lay_cur(I) = num_vl(veg_code(i)) + num_gl(geo_code(i)) ! maximum number of soil layer = organic layers + mineral layers
                                                do j = num_vl(veg_code(i)) + 1, n_lay_cur(I)
                                                    vwc(J, I) = B1(k, geo_code(i));
                                                    a_coef(J, I) = B2(k, geo_code(i));
                                                    b_coef(J, I) = B3(k, geo_code(i));
                                                    hcap_thw(J, I) = B4(k, geo_code(i));
                                                    hcap_frz(J, I) = B5(k, geo_code(i));
                                                    tcon_thw(J, I) = B6(k, geo_code(i));
                                                    tcon_frz(J, I) = B7(k, geo_code(i));
                                                    EE(J, I) = 0
                                                    layer_thick = layer_thick + B8(geo_code(i), k);
                                                    n_bnd_lay(i, j + 1) = layer_thick!B8(geo_code(i),j)
                                                    k = k + 1
                                                enddo
                                                n_bnd_lay(i, n_lay_cur(I) + 1) = zdepth(n_grd)
                                            enddo

                                            allocate(z(n_grd), STAT = IERR)
                                            allocate(dz(n_grd), STAT = IERR)
                                            allocate(temp(n_site, n_grd), STAT = IERR)
                                            allocate(lay_id(n_site, n_grd), STAT = IERR)
                                            allocate(i_time(n_site), STAT = IERR)
                                            allocate(z_frz_frn(n_time, n_frz_max, n_site), STAT = IERR)
                                            allocate(n_frz_frn(n_time, n_site), STAT = IERR)
                                            allocate(temp_frz(n_lay, n_site), STAT = IERR)
                                            allocate(RES(n_time, m_grd + 3), STAT = IERR)

                                            allocate(model%dz(n_grd), STAT = IERR)
                                            allocate(model%temp(n_site, n_grd), STAT = IERR)
                                            allocate(model%lay_id(n_site, n_grd), STAT = IERR)
                                            allocate(model%temp_frz(n_lay, n_site), STAT = IERR)
                                            allocate(model%RES(n_time, m_grd + 3), STAT = IERR)
                                            allocate(model%n_frz_frn(n_time, n_site), STAT = IERR)
                                            allocate(model%z_frz_frn(n_time, n_frz_max, n_site), STAT = IERR)

                                            i_time = 1 ! active_layer uses it below, needs to be initialized here
                                            z = zdepth / zdepth(n_grd)
                                            do i_grd = 2, n_grd
                                                dz(i_grd) = z(i_grd) - z(i_grd - 1)
                                            enddo

                                            hcscale = zdepth(n_grd) * zdepth(n_grd) / n_sec_day

                                            hcap_frz = hcap_frz * hcscale
                                            hcap_thw = hcap_thw * hcscale
                                            hcap_s = hcap_snow * hcscale
                                            L_fus = Lf * hcscale

                                            call assign_layer_id(n_lay, n_lay_cur, n_site, n_grd, zdepth, n_bnd_lay, lay_id)
                                            call init_cond(model, restart, n_site)

                                            allocate(utemp_time_i(n_time + 2), STAT = IERR) ! allocating interval varialbe after interation
                                            allocate(utemp_i(n_time + 2, n_site), STAT = IERR)
                                            allocate(snd_i(n_time + 2, n_site), STAT = IERR)
                                            allocate(stcon_i(n_time + 2, n_site), STAT = IERR)

                                            do j_time = 1, n_time + 2
                                                utemp_time_i(j_time) = time_restart + DBLE(j_time - 1) * time_step
                                            enddo
                                            do i_site = 1, n_site
                                                if (lbound .EQ. 2) temp_grd(i_site) = temp_grd(i_site) * zdepth(n_grd)
                                                do i_lay = 1, n_lay_cur(i_site)
                                                    temp_frz(i_lay, i_site) = -(vwc(i_lay, i_site) / a_coef(i_lay, i_site))**(1.d0 / b_coef(i_lay, i_site))
                                                enddo
                                                call interpolate(utemp_time, utemp(:, i_site), n_temp, utemp_time_i, utemp_i(:, i_site), n_time + 2)
                                                call interpolate(snd_time, snd(:, i_site), n_snow, utemp_time_i, snd_i(:, i_site), n_time + 2)
                                                !            call snowfix(model, utemp_i(:, i_site), snd_i(:, i_site), n_time + 2)
                                                call interpolate(stcon_time, stcon(:, i_site), n_stcon, utemp_time_i, stcon_i(:, i_site), n_time + 2)
                                                call active_layer(model, i_site)
                                            enddo

                                            if (model%write_outputs_or_not .eq. 1) then

                                                open(1, file = trim(adjustl(result_file)) // '.txt', STATUS = 'unknown')
                                                open(2, file = trim(adjustl(aver_res_file)) // '.txt', STATUS = 'unknown')
                                                open(3, file = trim(adjustl(restart_file)) // '.txt', STATUS = 'unknown')

                                                write(FMT1, '(A33,I0,A11)') '(1x,I0.10,1x,F10.0,2(1x,F10.4),', m_grd, '(1x,F10.4))'
                                                write(FMT2, '(A32,I0,A40)') '(1x,I0.10,1x,F10.0,2(1x,F8.3),', m_grd, '(1x,F8.3),(1x,F8.3,1x,F12.3),(1x,F12.3))'

                                            endif

                                            TINIR = 0.0D0

                                            time_s = time_step * DBLE(n_time * time_beg)
                                            time_e = time_step * DBLE(n_time * time_end)

                                            ! pass value to model interface

                                            model%n_temp = n_temp
                                            model%utemp_time = utemp_time
                                            model%utemp = utemp

                                            model%n_stcon = n_stcon
                                            model%stcon_time = stcon_time
                                            model%stcon = stcon

                                            model%n_snow = n_snow
                                            model%snd_time = snd_time
                                            model%snd = snd

                                            model%n_ini = n_ini
                                            model%zdepth_ini = zdepth_ini
                                            model%ztemp_ini = ztemp_ini

                                            model%zdepth = zdepth
                                            model%zdepth_id = zdepth_id

                                            model%dz = dz
                                            model%temp = temp
                                            model%lay_id = lay_id
                                            model%temp_frz = temp_frz
                                            model%RES = RES
                                            model%n_lay_cur = n_lay_cur

                                            model%n_lay = n_lay

                                            model%vwc = vwc
                                            model%a_coef = a_coef
                                            model%b_coef = b_coef
                                            model%EE = EE
                                            model%hcap_frz = hcap_frz
                                            model%hcap_thw = hcap_thw
                                            model%tcon_frz = tcon_frz
                                            model%tcon_thw = tcon_thw
                                            model%n_lay_cur = n_lay_cur
                                            model%n_bnd_lay = n_bnd_lay

                                            model%top_run_time = 1 ! initial is 1.
                                            model%dt = 1
                                            model%n_x = 1
                                            model%n_y = 1
                                            model%n_z = n_grd
                                            model%dx = 1
                                            model%dy = 1
                                            model%dz0 = 1

                                            model%time_beg = n_time * time_beg
                                            model%time_end = n_time * time_end

                                            model%initialize_status = 1

                                            model % tair_cur = utemp(model % top_run_time, 1)
                                            model % snd_cur = snd(model % top_run_time, 1)
                                            model % stcon_cur = stcon(model % top_run_time, 1)

                                            !        model%write_outputs_or_not = 0 ! 1: write out to file, 0: not.
                                        endif
                                    endif
                                endif
                            endif
                        endif
                    endif
                endif
            endif
        endif

    end subroutine initialize

    subroutine finalize(model)

        use bnd
        use thermo
        use grd

        type(gipl_model_type) :: model

        if (model%write_outputs_or_not==1) then

            ! -------------start file writting begin
            rewind(3)
            write(3, *) time_restart
            do i_grd = 1, n_grd
                write (3, *) (temp(i_site, i_grd), i_site = 1, n_site)
            enddo
            ! -------------start file writting end

            close(1); close(2); close(3)

        endif

    end subroutine finalize

    subroutine init_cond(model, q, last)

        use bnd
        use thermo
        use grd

        implicit none

        type(gipl_model_type) :: model

        integer q, last
        integer i, j
        character*164 file_init

        if (q .EQ. 1)then !restart=1 means reading initial data from
            do I = 1, last
                call interpolate(zdepth_ini, ztemp_ini(:, I), n_ini, zdepth, temp(I, :), n_grd)
            enddo
        elseif (restart .EQ. 0) then !restart=0 enbales spinup
            do i = 1, last
                do J = 1, n_grd
                    temp(i, j) = model%temp(i, j);
                enddo
            enddo
        endif

    end subroutine init_cond

    subroutine active_layer(model, k)

        use bnd
        use thermo
        use grd
        use alt

        implicit none

        type(gipl_model_type) :: model

        integer :: k, j, jj
        real*8 GA, GB, YFRON, GX, GY
        real*8 fsat_unf_water

        z_frz_frn(i_time(k), :, k) = sea_level
        n_frz_frn(i_time(k), k) = 0
        do 1329 JJ = 1, n_grd - 1
            J = n_grd - JJ
            if (zdepth(J) .GE. sea_level.AND.zdepth(J + 1) .LE. frz_frn_max)then
                GA = fsat_unf_water(temp(k, J), lay_id(k, J), k)
                GB = fsat_unf_water(temp(k, J + 1), lay_id(k, J + 1), k)
                if ((GA - sat_coef) * (GB - sat_coef) .LE. 0.D0) then
                    GY = (GA - GB) / (zdepth(J) - zdepth(J + 1))
                    GX = (GA + GB - GY * (zdepth(J) + zdepth(J + 1))) / 2.D0
                    if (GY .EQ. 0.D0) then
                        YFRON = (zdepth(J) + zdepth(J + 1)) / 2.D0
                    else
                        YFRON = (sat_coef - GX) / GY
                    endif
                else
                    GOTO 1329
                endif
                if (n_frz_frn(i_time(k), k) .LT. n_frz_max)then
                    n_frz_frn(i_time(k), k) = n_frz_frn(i_time(k), k) + 1
                    z_frz_frn(i_time(k), n_frz_frn(i_time(k), k), k) = YFRON
                endif
            endif
        1329 CONTINUE

    end subroutine active_layer

    subroutine save_results(k, time1, time2)
        use thermo
        use grd
        use alt

        implicit none
        integer :: k, j
        real*8 :: time1, time2
        real*8 :: futemp, fsnow_level

        RES(i_time(k), 1) = time1
        RES(i_time(k), 2) = futemp(time2, k)
        RES(i_time(k), 3) = fsnow_level(k, time2)
        do J = 1, m_grd
            RES(i_time(k), J + 3) = temp(k, zdepth_id(J))
        enddo

    end subroutine save_results

    subroutine print_info(model)

        type (gipl_model_type), intent (in) :: model

        write(*, "(a10, i8)") "n_x:", model%n_x
        write(*, "(a10, i8)") "n_y:", model%n_y

        write(*, "(a10, f8.2)") "dx:", model%dx
        write(*, "(a10, f8.2)") "dy:", model%dy

        write(*, "(a10, f8.2)") "dt:", model%time_step

        write(*, "(a10, f8.2)") "t_end:", model%time_end

    end subroutine print_info

end module gipl_model
!________________________________________________
!__________________FUNCTIONS_____________________
!________________________________________________
real*8 function funf_water(T, NNN, I)
    use thermo
    implicit none
    real*8, intent(in) :: T ! temprature
    integer, intent(in) :: NNN, I
    real*8 :: temp_dep
    real*8 :: a, b, e
    real*8 :: theta

    temp_dep = temp_frz(NNN, I) ! change I to k0 everywhere except temp_dep
    e = EE(NNN, I)
    theta = vwc(NNN, I)
    a = a_coef(NNN, I)
    b = b_coef(NNN, I)

    IF (T .LE. temp_dep - e)THEN
        funf_water = a * ((DABS(T))**b)
    ELSEIF (T .GT. temp_dep)THEN
        funf_water = theta
    ELSE
        funf_water = a * ((DABS(temp_dep - e))**b)
        funf_water = funf_water + (theta - funf_water) * (T + e - temp_dep) / e
    endif
    return
end function funf_water
!-----------------------------------------------
real*8 function fsat_unf_water(T, NNN, I)!Saturated unforzen water
    use thermo

    !IMPLICIT REAL*8(A-H,O-Z)
    implicit none
    real*8, intent(in) :: T
    integer, intent(in) :: NNN, I
    real*8 :: temp_dep
    real*8 :: a, b, e
    real*8 :: theta

    temp_dep = temp_frz(NNN, I) ! freezing temprature depression
    e = EE(NNN, I)
    theta = vwc(NNN, I)
    a = a_coef(NNN, I)
    b = b_coef(NNN, I)
    IF (T .LE. temp_dep - e)THEN
        fsat_unf_water = a * ((DABS(T))**b)
    ELSEIF (T .GT. temp_dep)THEN
        fsat_unf_water = theta
    ELSE
        fsat_unf_water = a * ((DABS(temp_dep - e))**b)
        fsat_unf_water = fsat_unf_water + (theta - fsat_unf_water) * (T + e - temp_dep) / e
    ENDIF
    fsat_unf_water = fsat_unf_water / theta
    return

end function fsat_unf_water
!-----------------------------------------------
real*8 function fdunf_water(T, NNN, I)
    use thermo
    implicit none
    real*8, intent(in) :: T ! temprature
    integer, intent(in) :: NNN, I
    real*8 :: temp_dep
    real*8 :: a, b, e
    real*8 :: theta

    temp_dep = temp_frz(NNN, I)
    e = EE(NNN, I)
    theta = vwc(NNN, I)
    a = a_coef(NNN, I)
    b = b_coef(NNN, I)

    if (T .LE. temp_dep - e)THEN
        fdunf_water = -b * a * ((DABS(T))**(b - 1.0D0))
    elseif (T .GT. temp_dep)THEN
        fdunf_water = 0.0D0
    else
        fdunf_water = a * ((DABS(temp_dep - e))**b)
        fdunf_water = (b - fdunf_water) / e
    endif
    return

end function fdunf_water
!----------------------------------------
real*8 function futemp(T, I)
    use bnd
    implicit none

    real*8 T
    integer I, II

    II = 1 + IDINT((T - TINIR) / time_step)
    futemp = utemp_i(II, I) + (T + time_restart - utemp_time_i(II)) &
            * (utemp_i(II + 1, I) - utemp_i(II, I)) / (utemp_time_i(II + 1) - utemp_time_i(II))
    return
end function futemp
!----------------------------------------
subroutine snowfix(air_temp, stcon, n)

    real*8, intent (in) :: air_temp(n)
    real*8, intent (out) :: stcon(n)
    integer :: n

    if (air_temp(1) .gt. 0.and.stcon(1) .gt. 0) stcon(1) = 0
    do i = 2, n
        if (air_temp(i) .gt. 0.and.stcon(i) .gt. 0)then
            if (stcon(i - 1) .eq. 0) stcon(i) = 0 ! puts zeros only at the begining of the year
        endif
    enddo

    return
end subroutine snowfix

!----------------------------------------
subroutine interpolate(XIN, YIN, NIN, XOUT, YOUT, n_itime)
    ! Linear interpolation
    real*8, intent(in) :: XIN(NIN), YIN(NIN), XOUT(n_itime)
    real*8, intent(out) :: YOUT(n_itime)
    integer :: NIN, n_itime
    do I = 1, n_itime
        if (XOUT(I) .LE. XIN(1))THEN
            YOUT(I) = YIN(1)
            GOTO 1
        elseif (XOUT(I) .GT. XIN(NIN))THEN
            YOUT(I) = YIN(NIN)
            GOTO 1
        else
            do J = 1, NIN - 1
                if (XIN(J) .LT. XOUT(I).AND.XOUT(I) .LE. XIN(J + 1))THEN
                    YOUT(I) = YIN(J) + (XOUT(I) - XIN(J)) * (YIN(J + 1) - YIN(J)) / (XIN(J + 1) - XIN(J))
                    GOTO 1
                endif
            enddo
        endif
        1 continue
    enddo
    return
end

!----------------------------------------
subroutine assign_layer_id(n_lay, n_lay_cur, n_site, n_grd, zdepth, n_bnd_lay, lay_id)
    !assigns correspond layer id to the grid point
    !starting from surface to the bottom
    implicit none

    integer :: n_site, n_grd, n_lay
    integer :: lay_id(n_site, n_grd), n_lay_cur(n_site)
    real*8 :: zdepth(n_grd)
    real :: n_bnd_lay(n_site, n_lay + 1)
    integer :: isite, igrd, ilay

    do isite = 1, n_site
        do 6 igrd = 1, n_grd
            lay_id(isite, igrd) = n_lay_cur(isite)
            do ilay = 1, n_lay_cur(isite) - 1
                if (n_bnd_lay(isite, ilay) .LE. zdepth(igrd).AND.zdepth(igrd) .LT. n_bnd_lay(isite, ilay + 1))then
                    lay_id(isite, igrd) = ilay
                    GOTO 6
                endif
            enddo
        6 continue
    enddo
    return
end
!----------------------------------------
real*8 function fsnow_level(site_id, time)
    use bnd
    real*8 :: time
    integer :: site_id, II

    II = 1 + IDINT((time - TINIR) / time_step)
    fsnow_level = snd_i(II, site_id) + (time + time_restart - utemp_time_i(II)) * &
            (snd_i(II + 1, site_id) - snd_i(II, site_id)) / (utemp_time_i(II + 1) - utemp_time_i(II))
    return
end function fsnow_level
!-----------------------------------------------
real*8 function ftcon(T, id, j, time_cur)
    use bnd
    use grd
    use thermo

    implicit real*8(A - H, O - Z)
    integer :: II

    gr_sur = sea_level
    dsnow = sea_level - fsnow_level(id, time_cur)
    NS = lay_id(id, j)
    if (zdepth(j) .le. dsnow)then !atmosphere
        ftcon = 1.d4
    elseif (zdepth(j) .Lt. gr_sur)then !snow
        II = 1 + IDINT((time_cur - tinir) / time_step)
        ftcon = stcon_i(II, id) + (time_cur + time_restart - utemp_time_i(II)) * &
                (stcon_i(II + 1, id) - stcon_i(II, id)) / (utemp_time_i(II + 1) - utemp_time_i(II))
    else !ground
        WC = funf_water(T, NS, id) / vwc(NS, id)
        ftcon = (tcon_thw(NS, id)**WC) * (tcon_frz(NS, id)**(1.0 - WC))
    endif
    return
end function ftcon
!----------------------------------------
real*8 function fhcap(T, NNUS, I)
    use thermo

    IMPLICIT REAL*8(A - H, O - Z)
    DIMENSION NNUS(2), T(2)

    H = 1 / (T(1) - T(2))
    if (DABS(T(1) - T(2)) .LT. 1.D-6) THEN
        fhcap = 0.5d0 * (fdunf_water(T(1), NNUS(1), I) + fdunf_water(T(2), NNUS(2), I))
    else
        if (nnus(1) .ne. nnus(2))THEN
            fhcap = 0.5D0 * (H * (funf_water(T(1), NNUS(1), I) - funf_water(T(2), NNUS(1), I)) + &
                    H * (funf_water(T(1), NNUS(2), I) - funf_water(T(2), NNUS(2), I)))
        else
            fhcap = H * (funf_water(T(1), NNUS(1), I) - funf_water(T(2), NNUS(2), I))
        endif
    endif
    fhcap = L_fus * DABS(fhcap)
    return
end function fhcap
!----------------------------------------
!----------------------------------------
real*8 function fapp_hcap(T, I, J) ! Apparent heat capacity
    use thermo
    use grd

    implicit real*8(A - H, O - Z)

    DIMENSION T(n_grd), WW(2), NN(2)

    li = lay_id(I, J) ! layer index
    gr_sur = sea_level ! ground surface
    if (zdepth(J) .lE. gr_sur)then
        fapp_hcap = hcap_s ! heat capacity for snow
    else
        WC = funf_water(T(J), li, I) / vwc(li, I)
        fapp_hcap = hcap_thw(li, I) * WC + hcap_frz(li, I) * (1.0 - WC)
        if (J .GT. (1).AND.J .LT. n_grd)then
            WW(1) = (T(J - 1) + T(J)) / 2.D0
            NN(1) = lay_id(I, J - 1)
            WW(2) = T(J)
            NN(2) = lay_id(I, J)
            fapp_hcap = fapp_hcap + fhcap(WW, NN, I) * dz(J) / (dz(J + 1) + dz(J))
            WW(1) = T(J)
            NN(1) = lay_id(I, J)
            WW(2) = (T(J + 1) + T(J)) / 2.D0
            NN(2) = lay_id(I, J + 1)
            fapp_hcap = fapp_hcap + fhcap(WW, NN, I) * dz(J + 1) / (dz(J + 1) + dz(J))
        elseif (J .EQ. 1)then
            WW(1) = T(J)
            NN(1) = lay_id(I, J)
            WW(2) = (T(J + 1) + T(J)) / 2.D0
            NN(2) = lay_id(I, J + 1)
            fapp_hcap = fapp_hcap + fhcap(WW, NN, I)
        elseif (J .EQ. n_grd)then
            WW(1) = (T(J - 1) + T(J)) / 2.D0
            NN(1) = lay_id(I, J - 1)
            WW(2) = T(J)
            NN(2) = lay_id(I, J)
            fapp_hcap = fapp_hcap + fhcap(WW, NN, I)
        endif
    endif

    return
end
!-------------------------------------------------------
subroutine stefan1D(temps, n_grd, dz, time_loop, isite, lay_idx, flux)

    use thermo
    use bnd
    use const

    implicit none

    integer, intent(inout) :: n_grd
    real*8, intent(inout) :: dz(n_grd), temps(n_grd)
    integer, intent(inout) :: lay_idx(n_grd)
    real*8, intent(inout) :: time_loop
    real*8 :: futemp, flux, fapp_hcap, ftcon, fsat_unf_water

    integer :: isite, i_grd, IT

    ! tridiagonal variables
    real*8 :: RAB1, RAB2, AKAPA2, AMU2, Q2
    real*8 :: A, B, C, D
    real*8 :: ALF(n_grd), BET(n_grd)
    real*8 :: EEY, EEY1, abs1, abs2

    real*8 :: temp_o(n_grd) ! old temperature before tridiagonal method
    real*8 :: temp_n(n_grd) ! new temperature after tridiagonal method

    ! time counter internal to this subroutine
    real*8 :: time_l ! loop time in a subroutine
    real*8 :: time_p ! present time in a subroutine
    real*8 :: timei ! main subroutine timer
    real :: time_swith ! for timei

    time_l = time_loop
    time_swith = -1.0
    timei = TAUM
    !    temps = temp(isite, :)
    64 continue
    time_p = time_l + timei
    temp_o = temps
    IT = 1
    ALF(2) = 0.D0
    BET(2) = futemp(time_p, isite)
    22 continue
    if (IT .GT. ITMAX) then
        timei = timei / 2.D0
        time_swith = -1.0
        GOTO 64
    endif

    do i_grd = 2, n_grd - 1
        D = fapp_hcap(temp_o, isite, i_grd) / timei
        A = 2.D0 * ftcon(temp_o(i_grd), isite, i_grd, time_p) / (dz(i_grd) * (dz(i_grd) + dz(i_grd + 1)))
        B = 2.D0 * ftcon(temp_o(i_grd + 1), isite, i_grd + 1, time_p) / (dz(i_grd + 1) * (dz(i_grd) + dz(i_grd + 1)))
        C = A + B + D
        ALF(i_grd + 1) = B / (C - A * ALF(i_grd))
        BET(i_grd + 1) = (A * BET(i_grd) + D * temps(i_grd)) / (C - A * ALF(i_grd))
    enddo

    RAB1 = ftcon(temp_o(n_grd), isite, n_grd, time_p)
    RAB2 = fapp_hcap(temp_o, isite, n_grd)
    AKAPA2 = 2.D0 * RAB1 / (((RAB2 * dz(n_grd) * dz(n_grd)) / timei + 2.D0 * RAB1))
    Q2 = RAB1 * flux
    AMU2 = (temps(n_grd) * RAB2 / timei + 2.D0 * Q2 / dz(n_grd)) / (RAB2 / timei + 2.D0 * RAB1 &
            / dz(n_grd)**2.D0)
    if (DABS(AKAPA2) > 1.D0) then
        print*, 'Tridiagonal method is failed - change your tau in the configuration file'
        print*, rab1, rab2, akapa2
        STOP
    endif

    ! assigns boundary condition check
    if (lbound .EQ. 2)then
        temp_n(n_grd) = (AMU2 + AKAPA2 * BET(n_grd)) / (1.D0 - ALF(n_grd) * AKAPA2)
    else
        temp_n(n_grd) = flux
    endif

    ! calculates new tempratures
    do i_grd = 1, n_grd - 1
        temp_n(n_grd - i_grd) = ALF(n_grd - i_grd + 1) * temp_n(n_grd - i_grd + 1) + BET(n_grd - i_grd + 1)
    enddo

    if (timei > tmin) then
        do i_grd = 1, n_grd
            EEY = fsat_unf_water(temp_n(i_grd), lay_idx(i_grd), isite)
            EEY1 = fsat_unf_water(temp_o(i_grd), lay_idx(i_grd), isite)
            abs1 = DABS(EEY - EEY1)
            abs2 = DABS(temp_o(i_grd) - temp_n(i_grd))
            if ((abs1 .GT. unf_water_coef).or.(abs2 .GT. smooth_coef)) then
                temp_o = temp_n
                IT = IT + 1
                GOTO 22
            endif
        enddo
    endif

    if (time_p .LT. time_loop + time_step - 1.D-12)then
        time_l = time_p
        temps = temp_n
        if (time_swith > 0) then
            if (timei .LT. TAUM) then
                timei = timei * 2.D0
                time_swith = -1.0
            endif
        else
            time_swith = 1.0
        endif
        GOTO 64
    elseif (time_p .GT. time_loop + time_step + 1.D-12)then
        timei = (time_loop + time_step - time_l)
        goto 64
    else
        temps = temp_n
    endif

end subroutine stefan1D

subroutine filexist(filename, status)
    character*164 filename
    logical chf
    integer status
    inquire(file = filename, exist = chf)
    status = 1
    if (.not.chf) then
        write(*, '(/'' FILE '',a, '' DOESNT EXIST'')') trim(filename)
        status = 0
        !         stop
    endif
end subroutine filexist
!-----------------------------------------------


program gipl2

    use gipl_model
    type (gipl_model_type) :: model

    real*8 :: run_time_start, run_time_final

    character*164 fconfig

    real, allocatable :: x(:), y(:)

    call cpu_time(run_time_start)

    fconfig = '../data/gipl_config.cfg'

    call initialize(model, fconfig)

    allocate(x(model % n_grd))
    allocate(y(model % n_grd))
    !
    x = model % zdepth
    y = model % temp(1,:)
    
    ! temp(1,40): soil surface temperature.
    
    print*, model % top_run_time, model % temp(1, 40)

    do i = 1, model % n_time -1

        call update(model)

        print*, model % top_run_time, model % temp(1, 40)

    enddo

    call finalize(model)

end ! end of main program
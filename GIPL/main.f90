program gipl2

    use gipl_model
    type (gipl_model_type) :: model

    real*8 :: run_time_start, run_time_final

    character*164 fconfig

    real, allocatable :: x(:), y(:)

    call cpu_time(run_time_start)

    model%write_outputs_or_not = 1

    fconfig = 'test.cfg'

    call initialize(model, fconfig)

    allocate(x(model % n_grd))
    allocate(y(model % n_grd))
    !
    x = model % zdepth
    y = model % temp(1, :)

    ! temp(1,40): soil surface temperature.

    !     model%vwc(1,1) = 0.1

    do i = 1, 365

        cur_time = model%top_run_time

        !        print*, model%utemp(3,1)

        !        model%utemp(3,1) = 0
        !        model%snd(:,1)   = 0
        !        model%stcon(:,1)  = 0.3

        !        print*, model%utemp(3,1)

        call update(model)

        print*, cur_time, model % temp(1, 40)

    enddo

    call finalize(model)

end
! end of main program
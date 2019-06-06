program gipl2

use gipl_model
type (gipl_model_type) :: model

real*8 :: run_time_start, run_time_final

real, allocatable :: x(:), y(:)

call cpu_time(run_time_start)

call initialize(model, '../data/gipl_config.cfg')
!call run_model
!call finalize

print*, model%n_time
print*, model%n_grd
!
!do i = 1, model%n_grd
!!    print*, model%zdepth(i), model%temp(1,i)
!enddo
!
allocate(x(model%n_grd))
allocate(y(model%n_grd))
!
x = model%zdepth
y = model%temp(1,:)

print*, model%temp(1,41)
print*, model%RES(1,5)
do i = 1, 1

call update(model)

print*, i, model%temp(1,40)
!print*, model%RES(1,4)

enddo 
!
!x = model%zdepth
!y = model%temp(1,:)

!do i = 1, model%n_time
!!   print*, modelR%ES(i,4)
!enddo

!print*, model%temp(1,41)

call finalize(model)

end ! end of main program
program gipl2

use gipl_model
type (gipl_model_type) :: model

real*8 :: run_time_start, run_time_final

call cpu_time(run_time_start)

call initialize(model, '../data/gipl_config.cfg')
!call run_model
!call finalize

print*, model%n_sec_day
print*, model%restart
print*, model%time_step
print*, model%n_time, model%n_temp, model%n_site

print*, model%n_grd

do i = 1,model%n_ini
    write(*,*) model%zdepth_ini(i), model%ztemp_ini(i,1)
!write(*,*) model%utemp_time(i),model%utemp(i,1), model%snd(i,1)
enddo

do i = 1,model%m_grd
    write(*,*) model%zdepth(i), model%zdepth_id(i)
enddo

call cpu_time(run_time_final)

end ! end of main program
program gipl2
use bnd
use thermo
use grd
use alt

real*8 :: run_time_start, run_time_final

call cpu_time(run_time_start)

call initialize('../data/gipl_config.cfg')
call run_model
call finalize

call cpu_time(run_time_final)

end ! end of main program
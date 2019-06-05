subroutine update_model_until(until_time)
  use gipl_bmi

  implicit none

  real*8 :: until_time

  do while (time_loop.LT.until_time)
    call update_model()
  enddo

end subroutine update_model_until


subroutine update_model()
  use gipl_bmi
  use gipl_const
  use bnd
  use thermo
  use grd
  use alt

  implicit none

  integer :: i_site,j_time

  do i_site=1,n_site
    call stefan1D(temp(i_site,:),dz,i_site,lay_id(i_site,:), &
      temp_grd(i_site), n_grd)
  enddo

  time_loop=time_loop+time_step

  do i_site=1,n_site
    if (mod(int(time_loop), n_time) .eq. 0) then
      i_time(i_site) = 1
    else
      i_time(i_site) = i_time(i_site)+1
    endif
    call save_results(i_site,time_loop, time_restart)
    call active_layer(i_site)
  enddo

  if (mod(int(time_loop), n_time) .eq. 0) then
    ! Perform year-end operations
    ! This section interpolates snd and stcon values to snd_i and stcon_i
    ! for use in the computations above
    ! This means that there isn't a clear set-the-value-at-this-time
    ! operation without computing how to index such a time

    do i_site=1,n_site
      i_time(i_site) = 1

      do j_time=1,n_time+2
        utemp_time_i(j_time)= &
          time_loop + time_restart + DBLE(j_time - 1) * time_step
      enddo
      call interpolate(utemp_time,utemp(:,i_site),n_temp,utemp_time_i,&
              utemp_i(:,i_site),n_time+2)
      call interpolate(snd_time,snd(:,i_site),n_snow,utemp_time_i,&
              snd_i(:,i_site),n_time+2)
      call snowfix(utemp_i(:,i_site),snd_i(:,i_site),n_time+2)
      call interpolate(stcon_time,stcon(:,i_site),n_stcon,utemp_time_i,&
              stcon_i(:,i_site),n_time+2)
    enddo
  endif

  if (mod(int(time_loop), n_time) .eq. 0) then
    TINIR=time_loop
  endif

end subroutine update_model


subroutine write_output()
  use gipl_bmi
  use grd
  use thermo
  use alt
  use bnd

  implicit none

  integer :: i_site, j_time, i_grd

  real*8 :: dfrz_frn(n_time)         ! depth of the freezing front
  real :: frz_up_time_cur            ! freezeup time current (within a year)
  real :: frz_up_time_tot            ! freezeup time global

  ! Write to results file
  if (mod(int(time_loop), n_time) .eq. n_time-1) then
    do i_site=1,n_site
      if(time_s.LT.time_e.AND.time_loop.GT.time_s)then
        do j_time=1,n_time
          write(1,FMT1) i_site, &
            monthly_time(i_site, j_time), &
            monthly_freeze_up_temp(i_site, j_time), &
            monthly_snow_level(i_site, j_time), &
            (monthly_temperature(i_site, j_time, i_grd), i_grd=1,m_grd)
        enddo
      endif
    enddo
  endif

  ! Write mean results
  if (mod(int(time_loop), n_time) .eq. n_time - 1) then
    do i_site=1,n_site
      annual_average_time(i_site) = &
        sum(monthly_time(i_site,:)) / dble(n_time)
      annual_freeze_up_temp(i_site) = &
        sum(monthly_freeze_up_temp(i_site,:)) / dble(n_time)
      annual_snow_level(i_site) = &
        sum(monthly_snow_level(i_site, :)) / dble(n_time)
      do i_grd=1,m_grd
        annual_temperature(i_site, i_grd) = &
          sum(monthly_temperature(i_site, :, i_grd)) / dble(n_time)
      enddo
    enddo

    do i_site=1,n_site
      frz_up_time_cur=-7777.D0
      frz_up_time_tot=frz_up_time_cur
      do j_time=2,n_time
        if((n_frz_frn(j_time,i_site)-n_frz_frn(j_time-1,i_site)).EQ.-2)then
          if(z_frz_frn(j_time-1,n_frz_frn(j_time-1,i_site),i_site).GE.&
                  frz_frn_min) frz_up_time_cur=SNGL(monthly_time(j_time,1))
        endif
      enddo

      if(frz_up_time_cur.GT.0.0)then
        frz_up_time_tot=AMOD(frz_up_time_cur,REAL(n_time))
        if(frz_up_time_tot.EQ.0.0)frz_up_time_tot=REAL(n_time)
      endif
      dfrz_frn=z_frz_frn(:,1,i_site)
    enddo

    do i_site = 1, n_site
      freeze_up_depth(i_site) = dfrz_frn(n_time)
      freeze_up_time_current(i_site) = frz_up_time_cur
      freeze_up_time_total(i_site) = frz_up_time_tot
    enddo

    do i_site=1,n_site
      write(2,FMT2) &
        i_site, &
        annual_average_time(i_site), &
        annual_freeze_up_temp(i_site), &
        annual_snow_level(i_site), &
        (annual_temperature(i_site, i_grd), i_grd=1,m_grd), &
        freeze_up_depth(i_site), &
        freeze_up_time_current(i_site), &
        freeze_up_time_total(i_site)
    enddo
  endif

  ! Write to the restart file
  if (mod(int(time_loop), n_time) .eq. 0) then
    call save_restart()
  endif

end subroutine write_output


!subroutine save_restart(n_site)
subroutine save_restart()
  use gipl_bmi
  use bnd
  use thermo
  use grd

  implicit none

  integer :: i_site,i_grd

  rewind(3)
  write(3, * ) time_restart
  do i_grd=1,n_grd
    write (3,* ) ( temp(i_site,i_grd),i_site=1,n_site)
  enddo

end subroutine save_restart


subroutine finalize_f90()

  implicit none

  close(1);close(2);close(3)

end subroutine finalize_f90


subroutine initialize_f90(named_config_file)
  use gipl_bmi
  use gipl_const
  use bnd
  use thermo
  use grd
  use alt

  implicit none

  character(64) :: named_config_file

  integer :: IREAD,ierr
  integer :: i,j,k,z_num,i_grd,j_time,i_site,i_lay


  real*8 ,allocatable ::gtzone(:,:)
  character(64) :: stdummy

  character(64) :: file_sites,file_bound,file_snow,file_rsnow,file_init
  character(64) :: file_grid,file_organic,file_mineral

  real*8,allocatable:: A1(:,:),A2(:,:),A3(:,:),A4(:,:),A5(:,:)
  real*8,allocatable:: A6(:,:),A7(:,:),A8(:,:)
  integer, allocatable :: veg_class(:), num_vl(:)
  integer :: vln

  real*8,allocatable:: B1(:,:),B2(:,:),B3(:,:),B4(:,:),B5(:,:)
  real*8,allocatable:: B6(:,:),B7(:,:),B8(:,:)
  integer, allocatable :: geo_class(:), num_gl(:)
  real*8 :: layer_thick
  integer :: gln
  real*8, allocatable :: z(:) ! vertical grid
  real*8 :: hcscale

  ! For now, the pre-set value of fconfig takes priority over the passed value
  if (fconfig .eq. '') then
    ! No pre-set fconfig, use named_config if it exists, or a default if not
    if (named_config_file .eq. '') then
      ! If nothing is specified, use this default configuration file
      !fconfig = 'gipl_config_3yr.cfg'
      fconfig = '../examples/gipl_config_3yr.cfg'
    else
      fconfig = named_config_file
    endif
  endif

  call filexist(fconfig)
  open(60,file=fconfig)
  !read input files
  read(60,'(A)')stdummy
  read(60,'(A)')file_sites
  read(60,'(A)')file_bound
  read(60,'(A)')file_snow
  read(60,'(A)')file_rsnow
  read(60,'(A)')file_init
  !read(60,'(A)')cmdf
  read(60,'(A)')file_grid
  read(60,'(A)')file_organic
  read(60,'(A)')file_mineral

  ! read output files
  read(60,'(A)')stdummy
  read(60,'(A)')stdummy
  read(60,'(A)')aver_res_file
  read(60,'(A)')result_file
  read(60,'(A)')restart_file

  ! read input parameters
  read(60,'(A)')stdummy
  read(60,'(A)')stdummy
  read(60,*)restart
  read(60,'(A)')stdummy
  read(60,*)time_step,TAUM,TMIN
  read(60,'(A)')stdummy
  read(60,*) time_beg,time_end
  read(60,'(A)')stdummy
  read(60,*) smooth_coef,unf_water_coef,itmax
  !smoothing factor | unfrozen water parameter | max number of iterations
  read(60,'(A)')stdummy
  read(60,*) n_sec_day,n_time
  ! number of second in a timestep [sec] | number of time steps in a year
  read(60,'(A)')stdummy
  read(60,*) sea_level,n_frz_max
  read(60,'(A)')stdummy
  read(60,*) frz_frn_min,frz_frn_max
  read(60,'(A)')stdummy
  read(60,*) sat_coef

  close(60)

  call filexist(file_sites)
  call filexist(file_bound)
  call filexist(file_snow)
  call filexist(file_rsnow)
  call filexist(file_grid)
  call filexist(file_init)
  call filexist(file_mineral)
  call filexist(file_organic)

  open(60,FILE=file_sites)
  read(60,*)n_site
  allocate(snow_code(n_site),STAT=IERR)
  allocate(veg_code(n_site),STAT=IERR)
  allocate(geo_code(n_site),STAT=IERR)
  allocate(gt_zone_code(n_site),STAT=IERR)
  allocate(temp_grd(n_site),STAT=IERR)
  do i_site=1,n_site
    read(60,*) IREAD,snow_code(i_site),veg_code(i_site),geo_code(i_site),&
      gt_zone_code(i_site),temp_grd(i_site)
  enddo
  close(60)

  open(60,file=file_bound)
  read(60,*)n_temp
  allocate(utemp_time(n_temp),STAT=IERR)
  allocate(utemp(n_temp,n_site),STAT=IERR)
  do i=1,n_temp
    read(60,*) utemp_time(I),(utemp(I,i_site),i_site=1,n_site)
  enddo
  close(60)

  open(60,file=file_rsnow)
  read(60,*)n_stcon
  allocate(stcon_time(n_stcon),STAT=IERR)
  allocate(stcon(n_stcon,n_site),STAT=IERR)
  do i=1,n_stcon
    read(60,*) stcon_time(i),(stcon(i,i_site),i_site=1,n_site)
  enddo
  close(60)

  open(60,file=file_snow)
  read(60,*)n_snow
  allocate(snd_time(n_snow),STAT=IERR)
  allocate(snd(n_snow,n_site),STAT=IERR)
  do I=1,n_snow
    read(60,*) snd_time(i),(snd(i,i_site),i_site=1,n_site)
  enddo
  close(60)

  open(60,file=file_init,action='read')
  read(60,*)z_num,n_ini!,time_restart
  allocate(zdepth_ini(n_ini),STAT=IERR)
  allocate(ztemp_ini(n_ini,n_site),STAT=IERR)
  allocate(gtzone(n_ini,z_num+1),STAT=IERR)
  read(60,*)stdummy
  do i=1,n_ini
    read(60,*) (gtzone(i,j),j=1,z_num+1)
  enddo
  close(60)

  time_restart=utemp_time(1)
  zdepth_ini(:)=gtzone(:,1)
  do i=1,n_site
    k=gt_zone_code(i)
    ztemp_ini(:,I)=gtzone(:,k+1)
  enddo

  open(60,file=file_grid)
  read(60,*)n_grd
  allocate(zdepth(n_grd),STAT=IERR)
  do i=1,n_grd
    read(60,*) zdepth(i)
  enddo
  read(60,*)m_grd
  allocate(zdepth_id(m_grd),STAT=IERR)
  do j=1,m_grd
    read(60,*)zdepth_id(j)
  enddo
  close(60)

  open (60, file=file_organic)
  ! reads numbers of  classes
  read(60,*) vln
  allocate(A1(n_lay,vln),STAT=IERR)  ! vwc
  allocate(A2(n_lay,vln),STAT=IERR)  ! a_coef
  allocate(A3(n_lay,vln),STAT=IERR)  ! b_coef
  allocate(A4(n_lay,vln),STAT=IERR)  ! hcap_frz
  allocate(A5(n_lay,vln),STAT=IERR)  ! hcap_thw
  allocate(A6(n_lay,vln),STAT=IERR)  ! tcon_frz
  allocate(A7(n_lay,vln),STAT=IERR)  ! tcon_thw
  allocate(A8(vln,n_lay),STAT=IERR)  ! bot_cond
  allocate(veg_class(vln),STAT=IERR) ! veg_class
  allocate(num_vl(vln),STAT=IERR)    ! num_vl number of vegetation layers
  do I = 1,vln
    read(60,*)veg_class(i),num_vl(i)
    do j=1,num_vl(i)
      read(60,*)A1(J,I),A2(J,I),A3(J,I), &
        A4(J,I),A5(J,I),A6(J,I),A7(J,I),A8(I,J)
    enddo
  enddo
  close(60)

  open (60, file=file_mineral)
  ! reads numbers of  classes
  read(60,*) gln
  allocate(B1(n_lay,gln),STAT=IERR)  ! vwc
  allocate(B2(n_lay,gln),STAT=IERR)  ! a_coef
  allocate(B3(n_lay,gln),STAT=IERR)  ! b_coef
  allocate(B4(n_lay,gln),STAT=IERR)  ! hcap_frz
  allocate(B5(n_lay,gln),STAT=IERR)  ! hcap_thw
  allocate(B6(n_lay,gln),STAT=IERR)  ! tcon_frz
  allocate(B7(n_lay,gln),STAT=IERR)  ! tcon_thw
  allocate(B8(gln,n_lay),STAT=IERR)  ! bot_cond
  allocate(geo_class(gln),STAT=IERR) ! geo_class
  allocate(num_gl(gln),STAT=IERR)    !num_vl number of lithologic layers
  do I = 1,gln
    read(60,*)geo_class(i),num_gl(i)
    do j=1,num_gl(i)
      read(60,*)B1(J,I),B2(J,I),B3(J,I), &
        B4(J,I),B5(J,I),B6(J,I),B7(J,I),B8(I,J)
    enddo
  enddo
  close(60)

  allocate(vwc(n_lay,n_site),STAT=IERR)
  allocate(a_coef(n_lay,n_site),STAT=IERR)
  allocate(b_coef(n_lay,n_site),STAT=IERR)
  allocate(EE(n_lay,n_site),STAT=IERR)
  allocate(hcap_frz(n_lay,n_site),STAT=IERR)
  allocate(hcap_thw(n_lay,n_site),STAT=IERR)
  allocate(tcon_frz(n_lay,n_site),STAT=IERR)
  allocate(tcon_thw(n_lay,n_site),STAT=IERR)
  allocate(n_lay_cur(n_site),STAT=IERR)
  allocate(n_bnd_lay(n_site,n_lay+1),STAT=IERR)

  do i = 1,n_site
    layer_thick=0
    n_bnd_lay(i,1)=SNGL(layer_thick)
    layer_thick=0
    n_bnd_lay(i,1)=SNGL(layer_thick)
    do j=1,num_vl(veg_code(i))
      vwc(J,I)=A1(j,veg_code(i));
      a_coef(J,I)=A2(j,veg_code(i));
      b_coef(J,I)=A3(j,veg_code(i));
      hcap_thw(J,I)=A4(j,veg_code(i));
      hcap_frz(J,I)=A5(j,veg_code(i));
      tcon_thw(J,I)=A6(j,veg_code(i));
      tcon_frz(J,I)=A7(j,veg_code(i));
      if (j.eq.1) then
        layer_thick=A8(veg_code(i),j)
      else
        layer_thick=layer_thick+A8(veg_code(i),j);
      endif
      n_bnd_lay(i,j+1)=SNGL(layer_thick)
      EE(J,I)=0
    enddo
    k=1
    ! maximum number of soil layer = organic layers + mineral layers
    n_lay_cur(I)=num_vl(veg_code(i))+num_gl(geo_code(i)) 
    do j=num_vl(veg_code(i))+1,n_lay_cur(I)
      vwc(J,I)=B1(k,geo_code(i));
      a_coef(J,I)=B2(k,geo_code(i));
      b_coef(J,I)=B3(k,geo_code(i));
      hcap_thw(J,I)=B4(k,geo_code(i));
      hcap_frz(J,I)=B5(k,geo_code(i));
      tcon_thw(J,I)=B6(k,geo_code(i));
      tcon_frz(J,I)=B7(k,geo_code(i));
      EE(J,I)=0
      layer_thick=layer_thick+B8(geo_code(i),k);
      n_bnd_lay(i,j+1)=SNGL(layer_thick)
      k=k+1
    enddo
    n_bnd_lay(i,n_lay_cur(I)+1)=SNGL(zdepth(n_grd))
  enddo

  allocate(z(n_grd),STAT=IERR)
  allocate(dz(n_grd),STAT=IERR)
  allocate(temp(n_site,n_grd),STAT=IERR)
  allocate(lay_id(n_site,n_grd),STAT=IERR)
  allocate(i_time(n_site),STAT=IERR)
  allocate(z_frz_frn(n_time,n_frz_max,n_site),STAT=IERR)
  allocate(n_frz_frn(n_time,n_site),STAT=IERR)
  allocate(temp_frz(n_lay,n_site),STAT=IERR)
  ! active_layer uses i_time below, needs to be initialized here
  i_time=1   ! this is an implicit array assignment
  z=zdepth/zdepth(n_grd)
  do i_grd=2,n_grd
    dz(i_grd)=z(i_grd)-z(i_grd-1)
  enddo

  hcscale=zdepth(n_grd)*zdepth(n_grd)/n_sec_day
  hcap_frz=hcap_frz*hcscale
  hcap_thw=hcap_thw*hcscale
  hcap_s=hcap_snow*hcscale
  L_fus=hcscale*Lf
  call  assign_layer_id(n_lay,n_lay_cur,n_site,n_grd,zdepth,n_bnd_lay,lay_id)
  call init_cond(restart,n_site)

  ! allocating interval variable after interation
  allocate(utemp_time_i(n_time+2),STAT=IERR)
  allocate(utemp_i(n_time+2,n_site),STAT=IERR)
  allocate(snd_i(n_time+2,n_site),STAT=IERR)
  allocate(stcon_i(n_time+2,n_site),STAT=IERR)

  ! Interpolate values for the first year
  do j_time=1,n_time+2
    utemp_time_i(j_time)=time_restart+DBLE(j_time-1)*time_step
  enddo
  do i_site=1,n_site
    if (lbound.EQ.2)temp_grd(i_site)=temp_grd(i_site)*zdepth(n_grd)
    do i_lay=1,n_lay_cur(i_site)
      temp_frz(i_lay,i_site)=-(vwc(i_lay,i_site)/&
              a_coef(i_lay,i_site))**(1.d0/b_coef(i_lay,i_site))
    enddo
    call interpolate(utemp_time,utemp(:,i_site),n_temp,utemp_time_i,&
            utemp_i(:,i_site),n_time+2)
    call interpolate(snd_time,snd(:,i_site),n_snow,utemp_time_i,&
            snd_i(:,i_site),n_time+2)
    call snowfix(utemp_i(:,i_site),snd_i(:,i_site),n_time+2)
    call interpolate(stcon_time,stcon(:,i_site),n_stcon,utemp_time_i,&
            stcon_i(:,i_site),n_time+2)
    call active_layer(i_site)
  enddo

  ! Allocate output arrays
  allocate(monthly_time(n_site, n_time))
  allocate(monthly_freeze_up_temp(n_site, n_time))
  allocate(monthly_snow_level(n_site, n_time))
  allocate(monthly_temperature(n_site, n_time, m_grd))
  allocate(annual_average_time(n_site))
  allocate(annual_freeze_up_temp(n_site))
  allocate(annual_snow_level(n_site))
  allocate(annual_temperature(n_site, m_grd))
  allocate(freeze_up_depth(n_site))
  allocate(freeze_up_time_current(n_site))
  allocate(freeze_up_time_total(n_site))

  open(1,file=result_file,STATUS='unknown')
  open(2,file=aver_res_file,STATUS='unknown')
  open(3,file=restart_file,STATUS='unknown')
  write(FMT1,'(A30,I0,A12)')'(1x,I10,1x,F12.3,2(1x,F16.12),',&
          m_grd,'(1x,F16.12))'
  write(FMT2,'(A28,I0,A40)')'(1x,I10,1x,F12.3,2(1x,F8.3),',&
          m_grd,'(1x,F8.3),(1x,F8.3,1x,F12.3),(1x,F12.3))'

  ! Initialize the results array
  do i_site=1,n_site
    call save_results(i_site, 0.0D0, time_restart)
  enddo

  time_s=time_step*DBLE(n_time*time_beg)
  time_e=time_step*DBLE(n_time*time_end)
  i_time=1  ! this is an implicit array assignment
  time_loop=0.0D0
  TINIR=0.0D0

  ! Allocate the 'input' arrays
  n_total_timesteps = int((time_e - time_s) / time_step) + 1
  allocate(surface_temp(n_total_timesteps))
  allocate(snow_depth(n_total_timesteps))
  allocate(snow_thermal_conductivity(n_total_timesteps))

end subroutine initialize_f90


subroutine init_cond(q,last)

  use gipl_bmi
  use bnd
  use thermo
  use grd

  implicit none
  integer q,last
  integer i,j
  character(64) :: file_init

  if(q.EQ.1)then !restart=1 means reading initial data from
    do I=1,last
      call interpolate(zdepth_ini,ztemp_ini(:,I),n_ini,zdepth,temp(I,:),n_grd)
    enddo
  elseif(restart.EQ.0)then              !restart=0 enbales spinup
    write(file_init,'(A14)') 'output/start.txt'
    open(60,file=file_init,action='READ')
    read(60,*)time_restart              ! day number in restart file
    do J=1,n_grd
      read (60,* ) ( temp(i,j),i=1,last)
    enddo
    close(60)
  endif

end subroutine init_cond


subroutine active_layer(k)

  use gipl_bmi
  use bnd
  use thermo
  use grd
  use alt

  implicit none

  integer :: k,j,jj
  real*8 GA,GB,YFRON,GX,GY
  real*8 fsat_unf_water

  z_frz_frn(i_time(k),:,k)=sea_level
  n_frz_frn(i_time(k),k)=0
  do 1329 JJ=1,n_grd-1
    J=n_grd-JJ
    if (zdepth(J).GE.sea_level.AND.zdepth(J+1).LE.frz_frn_max)then
      GA=fsat_unf_water(temp(k,J),lay_id(k,J),k)
      GB=fsat_unf_water(temp(k,J+1),lay_id(k,J+1),k)
      if((GA-sat_coef)*(GB-sat_coef).LE.0.D0) then
        GY=(GA-GB)/(zdepth(J)-zdepth(J+1))
        GX=(GA+GB-GY*(zdepth(J)+zdepth(J+1)))/2.D0
        if(GY.EQ.0.D0) then
          YFRON=(zdepth(J)+zdepth(J+1))/2.D0
        else
          YFRON=(sat_coef-GX)/GY
        endif
      else
        GOTO 1329
      endif
      if(n_frz_frn(i_time(k),k).LT.n_frz_max)then
        n_frz_frn(i_time(k),k)=n_frz_frn(i_time(k),k)+1
        z_frz_frn(i_time(k),n_frz_frn(i_time(k),k),k)=YFRON
      endif
    endif
1329 CONTINUE

end subroutine active_layer


subroutine save_results(k, time2, restart_time)
  use gipl_bmi
  use thermo
  use grd
  use alt

  implicit none
  integer :: k,j
  real*8 :: restart_time
  real*8 :: time2
  real*8 :: futemp,fsnow_level

  ! Save these values to an array instead of a file
  monthly_time(k, i_time(k)) = time2 + restart_time
  monthly_freeze_up_temp(k, i_time(k)) = futemp(time2, k)
  monthly_snow_level(k, i_time(k)) = fsnow_level(k, time2)
  do j=1,m_grd
    monthly_temperature(k, i_time(k), j) = temp(k, zdepth_id(j))
  enddo

end subroutine save_results


!________________________________________________
!__________________FUNCTIONS_____________________
!________________________________________________
real*8 function funf_water(T,NNN,I)
  use gipl_bmi
  use thermo
  implicit none
  real*8, intent(in) :: T ! temperature
  integer, intent(in) :: NNN, I
  real*8 :: temp_dep
  real*8 :: a,b,e
  real*8 :: theta

  temp_dep=temp_frz(NNN,I) ! change I to k0 everywhere except temp_dep
  e=EE(NNN,I)
  theta=vwc(NNN,I)
  a=a_coef(NNN,I)
  b=b_coef(NNN,I)

  IF(T.LE.temp_dep-e)THEN
    funf_water=a*((DABS(T))**b)
  ELSEIF(T.GT.temp_dep)THEN
    funf_water=theta
  ELSE
    funf_water=a*((DABS(temp_dep-e))**b)
    funf_water=funf_water+(theta-funf_water)*(T+e-temp_dep)/e
  endif
  return
end function funf_water


!-----------------------------------------------
real*8 function fsat_unf_water(T,NNN,I)!Saturated unfrozen water
  use gipl_bmi
  use thermo

  implicit none
  real*8, intent(in) :: T
  integer, intent(in) :: NNN, I
  real*8 :: temp_dep
  real*8 :: a,b,e
  real*8 :: theta

  temp_dep=temp_frz(NNN,I) ! freezing temperature depression
  e=EE(NNN,I)
  theta=vwc(NNN,I)
  a=a_coef(NNN,I)
  b=b_coef(NNN,I)
  IF(T.LE.temp_dep-e)THEN
    fsat_unf_water=a*((DABS(T))**b)
  ELSEIF(T.GT.temp_dep)THEN
    fsat_unf_water=theta
  ELSE
    fsat_unf_water=a*((DABS(temp_dep-e))**b)
    fsat_unf_water=fsat_unf_water+(theta-fsat_unf_water)*(T+e-temp_dep)/e
  ENDIF
  fsat_unf_water=fsat_unf_water/theta
  return

end function fsat_unf_water


!-----------------------------------------------
real*8 function fdunf_water(T,NNN,I)
  use gipl_bmi
  use thermo
  implicit none
  real*8, intent(in) :: T ! temperature
  integer, intent(in) :: NNN, I
  real*8 :: temp_dep
  real*8 :: a,b,e
  real*8 :: theta

  temp_dep=temp_frz(NNN,I)
  e=EE(NNN,I)
  theta=vwc(NNN,I)
  a=a_coef(NNN,I)
  b=b_coef(NNN,I)

  if(T.LE.temp_dep-e)THEN
    fdunf_water=-b*a*((DABS(T))**(b-1.0D0))
  elseif(T.GT.temp_dep)THEN
    fdunf_water=0.0D0
  else
    fdunf_water=a*((DABS(temp_dep-e))**b)
    fdunf_water=(b-fdunf_water)/e
  endif
  return

end function fdunf_water


!----------------------------------------
real*8 function futemp(T,I)
  use gipl_bmi
  use bnd
  implicit none
  real*8 T
  integer I,II

  II=1+IDINT((T-TINIR)/time_step)
  futemp=utemp_i(II,I)+(T+time_restart-utemp_time_i(II)) &
    *(utemp_i(II+1,I)-utemp_i(II,I))/(utemp_time_i(II+1)-utemp_time_i(II))
  return
end function futemp


!----------------------------------------
subroutine snowfix(air_temp,stcon,n)

  implicit none

  real*8, intent (in)  :: air_temp(n)
  real*8, intent (out) :: stcon(n)
  integer :: n
  integer :: i

  if(air_temp(1).gt.0.and.stcon(1).gt.0)stcon(1)=0
  do i=2,n
    if(air_temp(i).gt.0.and.stcon(i).gt.0)then
      ! puts zeros only at the begining of the year
      if (stcon(i-1).eq.0)stcon(i)=0
    endif
  enddo

  return
end subroutine snowfix

!----------------------------------------
subroutine interpolate(XIN,YIN,NIN,XOUT,YOUT,n_itime)
! Linear interpolation
  implicit none
  real*8, intent(in) :: XIN(NIN),YIN(NIN)
  real*8, intent(out) :: XOUT(n_itime),YOUT(n_itime)
  integer :: NIN,n_itime
  integer :: i, j

  do I=1,n_itime
    if(XOUT(I).LE.XIN(1))THEN
      YOUT(I)=YIN(1)
      GOTO 1
    elseif(XOUT(I).GT.XIN(NIN))THEN
      YOUT(I)=YIN(NIN)
      GOTO 1
    else
      do J=1,NIN-1
        if (XIN(J).LT.XOUT(I).AND.XOUT(I).LE.XIN(J+1))THEN
          YOUT(I)=YIN(J)+(XOUT(I)-XIN(J))*(YIN(J+1)-YIN(J))/(XIN(J+1)-XIN(J))
          GOTO 1
        endif
      enddo
    endif
1   continue
  enddo
  return
end


!----------------------------------------
subroutine assign_layer_id(n_lay,n_lay_cur,n_site,n_grd,zdepth,n_bnd_lay,lay_id)
  !assigns correspond layer id to the grid point
  !starting from surface to the bottom
  implicit none

  integer :: n_site,n_grd,n_lay
  integer :: lay_id(n_site,n_grd),n_lay_cur(n_site)
  real*8 :: zdepth(n_grd)
  real :: n_bnd_lay(n_site,n_lay+1)
  integer :: isite,igrd,ilay

  do isite=1,n_site
    do 6 igrd=1,n_grd
      lay_id(isite,igrd)=n_lay_cur(isite)
      do ilay=1,n_lay_cur(isite)-1
        if ( n_bnd_lay(isite,ilay).LE.zdepth(igrd).AND.&
                zdepth(igrd).LT.n_bnd_lay(isite,ilay+1))then
          lay_id(isite,igrd)=ilay
          GOTO 6
        endif
      enddo
6   continue
  enddo
  return
end


!----------------------------------------
real*8 function fsnow_level(site_id,time)
  use gipl_bmi
  use bnd
  real*8 :: time
  integer :: site_id,II

  II=1+IDINT((time-TINIR)/time_step)
  fsnow_level=snd_i(II,site_id)+(time+time_restart-utemp_time_i(II))* &
    (snd_i(II+1,site_id)-snd_i(II,site_id))/(utemp_time_i(II+1)&
    -utemp_time_i(II))
  return
end function fsnow_level


!-----------------------------------------------
real*8 function ftcon(T,id,j,time_cur)

  use gipl_bmi
  use bnd
  use grd
  use thermo

  implicit none

  real*8 :: T
  integer :: id, j
  real*8 :: time_cur

  integer :: II

  real*8 :: gr_sur, dsnow, wc
  integer ns

  ! Define types for functions
  real*8 :: fsnow_level
  real*8 :: funf_water

  gr_sur=sea_level
  dsnow=sea_level-fsnow_level(id,time_cur)
  NS=lay_id(id,j)
  if(zdepth(j).le.dsnow)then
    !atmosphere
    ftcon=1.d4
  elseif (zdepth(j).Lt.gr_sur)then
    !snow
    II=1+IDINT((time_cur-tinir)/time_step)
    ftcon=stcon_i(II,id)+(time_cur+time_restart-utemp_time_i(II))* &
      (stcon_i(II+1,id)-stcon_i(II,id))/(utemp_time_i(II+1)-utemp_time_i(II))
  else
    !ground
    WC=funf_water(T,NS,id)/vwc(NS,id)
    ftcon=(tcon_thw(NS,id)**WC)*(tcon_frz(NS,id)**(1.0-WC))
  endif
  return
end function ftcon


!----------------------------------------
real*8 function fhcap(T,NNUS,I)
  use gipl_bmi
  use thermo

  implicit none

  real*8, dimension(2) :: T
  integer, dimension(2) :: NNUS
  integer :: I

  real*8 :: H

  ! Functions
  real*8 :: fdunf_water
  real*8 :: funf_water

  H=1/(T(1)-T(2))
  if(DABS(T(1)-T(2)).LT.1.D-6) THEN
    fhcap=0.5d0*(fdunf_water(T(1),NNUS(1),I)+fdunf_water(T(2),NNUS(2),I))
  else
    if (nnus(1).ne.nnus(2))THEN
      fhcap=0.5D0*( H*(funf_water(T(1),NNUS(1),I)-funf_water(T(2),NNUS(1),I))+ &
        H*(funf_water(T(1),NNUS(2),I)-funf_water(T(2),NNUS(2),I)) )
    else
      fhcap=H*(funf_water(T(1),NNUS(1),I)-funf_water(T(2),NNUS(2),I))
    endif
  endif
  fhcap=L_fus*DABS(fhcap)
  return
end function fhcap


!----------------------------------------
real*8 function fapp_hcap(T,I,J,n_grd_passed)       ! Apparent heat capacity
  use gipl_bmi
  use thermo
  use grd

  implicit none

  integer :: n_grd_passed
  real*8, dimension(n_grd_passed) :: T
  real*8, dimension(2) :: WW
  integer, dimension(2) :: NN

  real*8 :: gr_sur
  real*8 :: wc
  integer :: i, j
  integer :: li

  ! Functions
  real*8 :: funf_water
  real*8 :: fhcap

  li=lay_id(I,J)                      ! layer index
  gr_sur=sea_level                    ! ground surface
  if(zdepth(J).lE.gr_sur)then
    fapp_hcap=hcap_s                  ! heat capacity for snow
  else
    WC=funf_water(T(J),li,I)/vwc(li,I)
    fapp_hcap=hcap_thw(li,I)*WC+hcap_frz(li,I)*(1.0-WC)
    if(J.GT.(1).AND.J.LT.n_grd)then
      WW(1)=(T(J-1)+T(J))/2.D0
      NN(1)=lay_id(I,J-1)
      WW(2)=T(J)
      NN(2)=lay_id(I,J)
      fapp_hcap=fapp_hcap+fhcap(WW,NN,I)*dz(J)/(dz(J+1)+dz(J))
      WW(1)=T(J)
      NN(1)=lay_id(I,J)
      WW(2)=(T(J+1)+T(J))/2.D0
      NN(2)=lay_id(I,J+1)
      fapp_hcap=fapp_hcap+fhcap(WW,NN,I)*dz(J+1)/(dz(J+1)+dz(J))
    elseif(J.EQ.1)then
      WW(1)=T(J)
      NN(1)=lay_id(I,J)
      WW(2)=(T(J+1)+T(J))/2.D0
      NN(2)=lay_id(I,J+1)
      fapp_hcap=fapp_hcap+fhcap(WW,NN,I)
    elseif(J.EQ.n_grd)then
      WW(1)=(T(J-1)+T(J))/2.D0
      NN(1)=lay_id(I,J-1)
      WW(2)=T(J)
      NN(2)=lay_id(I,J)
      fapp_hcap=fapp_hcap+fhcap(WW,NN,I)
    endif
  endif

  return
end


!-------------------------------------------------------
subroutine stefan1D(temps,dz, isite,lay_idx,flux, nn_grd)

  use gipl_bmi
  use thermo
  use bnd
  use gipl_const

  implicit none

  integer, intent(in) :: nn_grd
  real*8, intent(in) :: dz(nn_grd)
  real*8, intent(inout) :: temps(nn_grd)
  integer, intent(in) :: lay_idx(nn_grd)
  real*8 :: futemp,flux,fapp_hcap,ftcon,fsat_unf_water

  integer :: isite,i_grd,IT

! tridiagonal variables
  real*8 :: RAB1,RAB2,AKAPA2,AMU2,Q2
  real*8 :: A,B,C,D
  real*8 :: ALF(nn_grd),BET(nn_grd)
  real*8 :: EEY,EEY1,abs1,abs2

  ! old temperature before tridiagonal method
  real*8 :: temp_o(nn_grd)
  ! new temperature after tridiagonal method
  real*8 :: temp_n(nn_grd)

! time counter internal to this subroutine
  real*8 :: time_l                    ! loop time in a subroutine
  real*8 :: time_p                    ! present time in a subroutine
  real*8 :: timei                     ! main subroutine timer
  real :: time_swith                  ! for timei

  time_l=time_loop
  time_swith=-1.0
  timei=TAUM
  temps=temp(isite,:)
64 continue
  time_p=time_l+timei
  temp_o=temps
  IT=1
  ALF(2)=0.D0
  BET(2)=futemp(time_p,isite)
22 continue
  if(IT.GT.ITMAX) then
    timei=timei/2.D0
    time_swith=-1.0
    GOTO 64
  endif

  do i_grd=2,n_grd-1
    D=fapp_hcap(temp_o,isite,i_grd,n_grd)/timei
    A=2.D0*ftcon(temp_o(i_grd),isite,i_grd,time_p)/&
            (dz(i_grd)*(dz(i_grd)+dz(i_grd+1)))
    B=2.D0*ftcon(temp_o(i_grd+1),isite,i_grd+1,time_p)/&
            (dz(i_grd+1)*(dz(i_grd)+dz(i_grd+1)))
    C=A+B+D
    ALF(i_grd+1)=B/(C-A*ALF(i_grd))
    BET(i_grd+1)=(A*BET(i_grd)+D*temps(i_grd))/(C-A*ALF(i_grd))
  enddo

  RAB1=ftcon(temp_o(n_grd),isite,n_grd,time_p)
  RAB2=fapp_hcap(temp_o,isite,n_grd,n_grd)
  AKAPA2=2.D0*RAB1/(((RAB2*dz(n_grd)*dz(n_grd))/timei+2.D0*RAB1))
  Q2=RAB1*flux
  AMU2=(temps(n_grd)*RAB2/timei+2.D0*Q2/dz(n_grd))/(RAB2/timei+2.D0*RAB1 &
    /dz(n_grd)**2.D0)
  if(DABS(AKAPA2)>1.D0) then
    print*,'Tridiagonal method is failed - chang you time step tau'
    print*,rab1,rab2,akapa2
    STOP
  endif

! assigns boundary condition check
  if (lbound.EQ.2)then
    temp_n(n_grd)=(AMU2+AKAPA2*BET(n_grd))/(1.D0-ALF(n_grd)*AKAPA2)
  else
    temp_n(n_grd)=flux
  endif

! calculates new temperatures
  do i_grd=1,n_grd-1
    temp_n(n_grd-i_grd)=ALF(n_grd-i_grd+1)*temp_n(n_grd-i_grd+1)+&
            BET(n_grd-i_grd+1)
  enddo

  if(timei>tmin) then
    do i_grd=1,n_grd
      EEY=fsat_unf_water(temp_n(i_grd),lay_idx(i_grd),isite)
      EEY1=fsat_unf_water(temp_o(i_grd),lay_idx(i_grd),isite)
      abs1=DABS(EEY-EEY1)
      abs2=DABS(temp_o(i_grd)-temp_n(i_grd))
      if((abs1.GT.unf_water_coef).or.(abs2.GT.smooth_coef)) then
        temp_o=temp_n
        IT=IT+1
        GOTO 22
      endif
    enddo
  endif

  if(time_p.LT.time_loop+time_step-1.D-12)then
    time_l=time_p
    temps=temp_n
    if(time_swith>0) then
      if(timei.LT.TAUM) then
        timei=timei*2.D0
        time_swith=-1.0
      endif
    else
      time_swith=1.0
    endif
    GOTO 64
  elseif(time_p.GT.time_loop+time_step+1.D-12)then
    timei=(time_loop+time_step-time_l)
    goto 64
  else
    temps=temp_n
  endif

end subroutine stefan1D


subroutine filexist(filename)
  implicit none
  character(64) :: filename
  logical chf
  inquire(file=filename,exist=chf)
  if (.not.chf) then
    write(*,'(/'' FILE '',a, '' DOESNT EXIST'')')trim(filename)
    stop
  endif
end subroutine filexist
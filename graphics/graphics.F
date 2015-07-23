module module_g2_graphics
  use kwm_plot_utilities

  implicit none

  type noah_mp_options_type
     character(len=256) :: filename
     real :: dt
     integer :: opt_dveg
     integer :: opt_crs
     integer :: opt_btr
     integer :: opt_run
     integer :: opt_sfc
     integer :: opt_frz
     integer :: opt_inf
     integer :: opt_rad
     integer :: opt_alb
     integer :: opt_snf
     integer :: opt_tbot
     integer :: opt_stc
  end type noah_mp_options_type

contains

!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------

  subroutine interp_val(x_source, x_dest, y_source, y_dest)
    ! Interpolate the data in array <y_source>, valid at x-coordinates in corresponding array <x_source>, to
    ! new times in array <x_dest>.  Put the new interpolated data in <y_dest>
    implicit none
    real, pointer, dimension(:) :: x_dest, x_source, y_source, y_dest
    ! Local
    integer :: i, j
    real :: x1, y1, x2, y2

    ! New array where we'll store the interpolated data
    allocate(y_dest(size(x_dest)))

    NEWTIME_LOOP : do i = 1, size(x_dest) ! Scan through the x-values for which we want data.
       y_dest(i) = -1.E36
       SRCTIME_LOOP : do j = 1, size(x_source)
          if ( y_source(j) > -1.E25 ) then
             if ( x_source(j) == x_dest(i) ) then
                y_dest(i) = y_source(j)
                exit SRCTIME_LOOP
             else if (x_source(j) < x_dest(i)) then
                y1 = y_source(j)
                x1 = x_source(j)
             else if (x_source(j) > x_dest(i)) then
                y2 = y_source(j)
                x2 = x_source(j)
                if ( (x2-x1) < 120 ) then ! Only do interpolation if the gap is not too large
                   y_dest(i) = y1 + ( ( x_dest(i) - x1 ) * ( y2 - y1 ) / ( x2 - x1 )  )
                endif
                exit SRCTIME_LOOP
             endif
          endif
       enddo SRCTIME_LOOP

    enddo NEWTIME_LOOP

  end subroutine interp_val

!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------

  subroutine plot_em_val(startdate, enddate, &
       name1, desc1, tag1, noah1, x1, options1, &
       name2, desc2, tag2, noah2, x2, options2, &
       nameval, tagval, vald, xval)
    character(len=*),             intent(in) :: startdate, enddate
    character(len=*),             intent(in) :: name1, name2, desc1, desc2
    character(len=*),             intent(in) :: nameval
    character(len=256),           intent(in) :: tag1, tag2
    character(len=256),           intent(in) :: tagval
    real, pointer, dimension(:)              :: noah1, x1, noah2, x2
    real, pointer, dimension(:)              :: vald, xval
    type(noah_mp_options_type),   intent(in) :: options1, options2

    real :: ymin, ymax
    integer :: expn, irange
    integer :: k
    logical :: pd
    character(len=256) :: text
    character(len=256) :: text1
    character(len=256) :: text2

    real, parameter :: frame_left   = 0.15
    real, parameter :: frame_right  = 0.85
    real, parameter :: frame_bottom = 0.3
    real, parameter :: frame_top    = 0.7

    integer :: idts

!KWM    character(len=12) :: middate

!KWM    call geth_idts (enddate(1:12), startdate(1:12), idts)
!KWM    call geth_newdate(middate, startdate(1:12), idts/2)
!KWM    print*, 'middate = ', middate

    call make_sidelabels(options1, text1)
    call make_sidelabels(options2, text2)

    call set(0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)
    call pcseti("CC", color_index("blue"))
    call pchiqu(0.15, .97, trim(tag1)//":  "//name1//": "//desc1, 0.015, 0.0, -1.0)
    call pchiqu(.87, .80, trim(text1), 0.009, 0.0, -1.0)

    call pcseti("CC", color_index("red"))
    call pchiqu(0.15, .94, trim(tag2)//":  "//name2//": "//desc2, 0.015, 0.0, -1.0)
    call pchiqu(.87, .60, trim(text2), 0.009, 0.0, -1.0)

    call pcseti("CC", color_index("black"))
    if (nameval /= "-") then
       call pchiqu(0.15, .91, trim(tagval)//":  "//nameval, 0.015, 0.0, -1.0)
    endif

    write(text, '("From ", A4, "-", A2, "-", A2, "_", A2, ":", A2, " to ", A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
         startdate(1:4), startdate(5:6), startdate(7:8), startdate(9:10), startdate(11:12),  &
         enddate  (1:4), enddate  (5:6), enddate  (7:8), enddate  (9:10), enddate  (11:12)

    call pchiqu(0.18, 0.88, trim(text), 0.012, 0.0, -1.0)

    ! Bottom axis labels:

    write(text, '(A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
         startdate(1:4), startdate(5:6), startdate(7:8), startdate(9:10), startdate(11:12)
    call pchiqu(frame_left, frame_bottom-0.020, trim(text), 0.009, 0., 0.)

    write(text, '(A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
         enddate(1:4), enddate(5:6), enddate(7:8), enddate(9:10), enddate(11:12)
    call pchiqu(frame_right, frame_bottom-0.020, trim(text), 0.009, 0., 0.)

    ymin = min(minval(noah1),minval(noah2))
    ymax = max(maxval(noah1),maxval(noah2))

    if (nameval /= "-") then
       ymin = min(ymin, minval(vald, mask=(vald>-1.E25)))
       ymax = max(ymax, maxval(vald, mask=(vald>-1.E25)))
    endif

    if ( abs(ymax-ymin) < 1.E-20 ) then
        if (ymax > 0) then
            ymax = ymax*1.05
            ymin = ymin*0.95
        elseif (ymax < 0) then
            ymax = ymax*0.95
            ymin = ymin*1.05
        elseif (ymax == 0) then
            ymax = 0.01
            ymin = -0.01
        endif
        call find_good_range(ymin, ymax, expn, irange)
    else
        call find_good_range(ymin, ymax, expn, irange)
    endif

    call set(frame_left, frame_right, frame_bottom, frame_top, x1(1), x1(size(noah1)), ymin, ymax, 1)

    call gsplci(color_index("gray75"))

    call line(1.0, 0.0, x1(size(noah1)), 0.0)

    call gsplci(color_index("blue"))
    call frstpt(x1(1), noah1(1))
    do k = 2, size(noah1)
       call vector(x1(k), noah1(k))
    enddo
    call plotif(0., 0., 2)

    call gsplci(color_index("red"))
    call frstpt(x2(1), noah2(1))
    do k = 2, size(noah2)
       call vector(x2(k), noah2(k))
    enddo
    call plotif(0., 0., 2)

    call gsplci(color_index("black"))

    if (nameval /= "-") then

       pd = .FALSE.
       do k = 1, size(vald)
          if ( vald(k) > -1.E25 ) then
             if (.not. pd) then
                call frstpt(xval(k), vald(k))
                pd = .TRUE.
             else      
                call vector(xval(k), vald(k))
             endif
          else
             if (pd) then
                pd = .FALSE.
                call plotif(0., 0., 2)
             endif
          endif
       enddo
       call plotif(0., 0., 2)
    endif

    call gsplci(color_index("black"))
    call gasetc("YLF", "(F12.3)")
    call gasetc("XLF", "(I0)")

    call gridal(0,0,irange,0,   0,1,5,0.,0.)

  end subroutine plot_em_val


!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------


  subroutine plot_em_scatter(startdate, enddate, &
       name1, desc1, tag1, noah1, x1, options1, &
       name2, desc2, tag2, noah2, x2, options2, name3, tag3, vald, xval)
    character(len=*),             intent(in) :: startdate, enddate
    character(len=*),             intent(in) :: name1, name2, desc1, desc2
    character(len=*),             intent(in) :: name3
    character(len=256),           intent(in) :: tag1, tag2
    character(len=256),           intent(in) :: tag3
    real, pointer, dimension(:)              :: noah1, x1, noah2, x2
    real, pointer, dimension(:)              :: vald, xval
    type(noah_mp_options_type),   intent(in) :: options1, options2

    real :: ymin, ymax
    integer :: expn, irange
    integer :: k
    logical :: pd
    character(len=256) :: text
    character(len=256) :: text1
    character(len=256) :: text2

    real, parameter :: frame_left   = 0.20
    real, parameter :: frame_right  = 0.85
    real, parameter :: frame_bottom = 0.20
    real, parameter :: frame_top    = 0.85

    double precision :: sumx, sumxx, sumy, sumyy, sumxy, sxx, sxy, syy, ccor1, ccor2, bias, bias1, bias2, mae, mae1, mae2, rmse, rmse1, rmse2
    integer :: ccount

    call make_sidelabels(options1, text1)
    call make_sidelabels(options2, text2)

    call set(0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)
    call pcseti("CC", color_index("blue"))
    call pchiqu(0.15, .97, trim(tag1)//":  "//name1//": "//desc1, 0.015, 0.0, -1.0)
    call pchiqu(.87, .80, trim(text1), 0.009, 0.0, -1.0)

    call pcseti("CC", color_index("red"))
    call pchiqu(0.15, .94, trim(tag2)//":  "//name2//": "//desc2, 0.015, 0.0, -1.0)
    call pchiqu(.87, .60, trim(text2), 0.009, 0.0, -1.0)

    call pcseti("CC", color_index("black"))
    if (name3 /= "-") then
       call pchiqu(0.15, .91, trim(tag3)//":  "//name3, 0.015, 0.0, -1.0)
    endif

    call pcseti("CC", color_index("black"))
    write(text, '("From ", A4, "-", A2, "-", A2, "_", A2, ":", A2, " to ", A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
         startdate(1:4), startdate(5:6), startdate(7:8), startdate(9:10), startdate(11:12),  &
         enddate  (1:4), enddate  (5:6), enddate  (7:8), enddate  (9:10), enddate  (11:12)

    call pchiqu(0.18, 0.88, trim(text), 0.012, 0.0, -1.0)


    ymin = min(minval(noah1),minval(noah2))
    ymax = max(maxval(noah1),maxval(noah2))

    if (name3 /= "-") then
       ymin = min(ymin, minval(vald, mask=(vald>-1.E25)))
       ymax = max(ymax, maxval(vald, mask=(vald>-1.E25)))
    endif

    call find_good_range(ymin, ymax, expn, irange)

    call set(frame_left, frame_right, frame_bottom, frame_top, ymin, ymax, ymin, ymax, 1)

    ! Compute correlation coefficients, too.
    sumx  = 0.0
    sumy  = 0.0
    sumxx = 0.0
    sumyy = 0.0
    sumxy = 0.0
    bias  = 0.0
    mae   = 0.0
    rmse  = 0.0
    ccount = 0
    do k = 1, size(noah1)
       if (vald(k) > -1.E25) then
          bias = bias + (noah1(k) - vald(k))
          mae  = mae  + abs(noah1(k) - vald(k))
          rmse = rmse + (noah1(k) - vald(k))**2
          sumx  = sumx  + vald(k)
          sumy  = sumy  + noah1(k)
          sumxx = sumxx + (vald(k)*vald(k))
          sumyy = sumyy + (noah1(k)*noah1(k))
          sumxy = sumxy + vald(k)*noah1(k)
          ccount = ccount + 1
          call ngdots(vald(k), noah1(k), 1, (ymax-ymin)*0.005, color_index("blue"))
       endif
    enddo

    sxx = sumxx - (sumx*sumx)/dble(ccount)
    syy = sumyy - (sumy*sumy)/dble(ccount)
    sxy = sumxy - (sumx*sumy)/dble(ccount)

    ccor1 = sxy / sqrt(sxx*syy)
    bias1 = bias / dble(ccount)
    mae1  = mae  / dble(ccount)
    rmse1 = sqrt ( rmse / dble(ccount) )

    sumx  = 0.0
    sumy  = 0.0
    sumxx = 0.0
    sumyy = 0.0
    sumxy = 0.0
    bias  = 0.0
    mae   = 0.0
    rmse  = 0.0
    ccount = 0
    do k = 1, size(noah2)
       if (vald(k) > -1.E25) then
          bias = bias + (noah2(k) - vald(k))
          mae  = mae  + abs(noah2(k) - vald(k))
          rmse = rmse + (noah2(k) - vald(k))**2
          sumx  = sumx  + vald(k)
          sumy  = sumy  + noah2(k)
          sumxx = sumxx + (vald(k)*vald(k))
          sumyy = sumyy + (noah2(k)*noah2(k))
          sumxy = sumxy + (vald(k)*noah2(k))
          ccount = ccount + 1
          call ngdots(vald(k), noah2(k), 1, (ymax-ymin)*0.005, color_index("red"))
       endif
    enddo

    sxx = sumxx - (sumx*sumx)/dble(ccount)
    syy = sumyy - (sumy*sumy)/dble(ccount)
    sxy = sumxy - (sumx*sumy)/dble(ccount)

    ccor2 = sxy / sqrt(sxx*syy)
    bias2 = bias / dble(ccount)
    mae2  = mae  / dble(ccount)
    rmse2 = sqrt ( rmse / dble(ccount) )

    call gsplci(color_index("gray75"))
    call line(ymin, ymin, ymax, ymax)

    call gsplci(color_index("black"))
    call gasetc("YLF", "(F12.3)")
    call gasetc("XLF", "(I0)")

    call gridal(irange,0,irange,0,    1,1,5,0.,0.)

    call set(0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)

    write(text,'("Bias: ", F10.3, "~C~MAE: ", F10.3, "~C~RMSE:", F10.3, "~C~Corr: ", F6.4)') bias1, mae1, rmse1, ccor1
    call pcseti("CC", color_index("blue"))
    call pchiqu(frame_left+0.02, frame_top-0.03, trim(text), 0.010, 0.0, -1.0)

    write(text,'("Bias: ", F10.3, "~C~MAE: ", F10.3, "~C~RMSE:", F10.3, "~C~Corr: ", F6.4)') bias2, mae2, rmse2, ccor2
    call pcseti("CC", color_index("red"))
    call pchiqu(frame_left+0.16, frame_top-0.03, trim(text), 0.010, 0.0, -1.0)

    write(text,'(F6.4)') ccor2



    call pcseti("CC", color_index("black"))
  end subroutine plot_em_scatter

!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------

  subroutine plot_em_avg_val(startdate, enddate, &
       name1, desc1, tag1, noah1, options1, &
       name2, desc2, tag2, noah2, options2, &
       name3, tag3, vald)
    character(len=*), intent(in) :: startdate, enddate
    character(len=*), intent(in) :: name1, name2, name3, desc1, desc2
    character(len=256), intent(in) :: tag1, tag2, tag3
    real, dimension(0:1440) :: noah1, noah2, vald
    type(noah_mp_options_type), intent(in) :: options1, options2
    real :: ymin, ymax
    integer :: expn, irange
    integer :: k
    real    :: xx
    logical :: pd
    character(len=256) :: text
    character(len=256) :: text1
    character(len=256) :: text2

    real, parameter :: frame_left   = 0.15
    real, parameter :: frame_right  = 0.85
    real, parameter :: frame_bottom = 0.3
    real, parameter :: frame_top    = 0.7

    call make_sidelabels(options1, text1)
    call make_sidelabels(options2, text2)

    call set(0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)
    call pcseti("CC", color_index("blue"))
    call pchiqu(0.15, .97, trim(tag1)//": "//name1//": "//desc1, 0.015, 0.0, -1.0)
    call pchiqu(.87, .80, trim(text1), 0.009, 0.0, -1.0)

    call pcseti("CC", color_index("red"))
    call pchiqu(0.15, .94, trim(tag2)//": "//name2//": "//desc2, 0.015, 0.0, -1.0)
    call pchiqu(.87, .60, trim(text2), 0.009, 0.0, -1.0)


    call pcseti("CC", color_index("black"))
    if (name3 /= "-") then
       call pchiqu(0.15, .91, trim(tag3)//": "//name3, 0.015, 0.0, -1.0)
    endif

    write(text, '("From ", A4, "-", A2, "-", A2, "_", A2, ":", A2, " to ", A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
         startdate(1:4), startdate(5:6), startdate(7:8), startdate(9:10), startdate(11:12),  &
         enddate  (1:4), enddate  (5:6), enddate  (7:8), enddate  (9:10), enddate  (11:12)

    call pchiqu(0.18, 0.88, trim(text), 0.012, 0.0, -1.0)

    ! Bottom axis labels:

    do k = 0, 24
       xx = frame_left + (real(k)/real(24)) * (frame_right-frame_left)
       if (mod(k,3)==0) then
          write(text, '(I2.2, " Z")') mod(k,24)
          call pchiqu(xx, frame_bottom-0.020, trim(text), 0.009, 0., 0.)
       endif
       call line(xx, frame_bottom, xx, frame_bottom+0.01)
       call line(xx, frame_top,    xx, frame_top   -0.01)
    enddo

    call pchiqu(.5, .05, "Average diurnal cycle", 0.018, 0.0, 0.0)

    ymin = min(minval(noah1, mask=(noah1>-1.E25)),minval(noah2, mask=(noah2>-1.E25)))
    ymax = max(maxval(noah1, mask=(noah1>-1.E25)),maxval(noah2, mask=(noah2>-1.E25)))
    if (name3 /= "-") then
       ymin = min(ymin,minval(vald, mask=(vald>-1.E25)))
       ymax = max(ymax,maxval(vald, mask=(vald>-1.E25)))
    endif

    if ( abs(ymax-ymin) < 1.E-20 ) then
        if (ymax > 0) then
            ymax = ymax*1.05
            ymin = ymin*0.95
        elseif (ymax < 0) then
            ymax = ymax*0.95
            ymin = ymin*1.05
        elseif (ymax == 0) then
            ymax = 0.01
            ymin = -0.01
        endif
        call find_good_range(ymin, ymax, expn, irange)
    else
        call find_good_range(ymin, ymax, expn, irange)
    endif

    ! print*,' ymin, ymax = ', ymin, ymax

    call set(frame_left, frame_right, frame_bottom, frame_top, 0.0, 1440.0, ymin, ymax, 1)

    call gsplci(color_index("blue"))
    pd = .FALSE.
    do k = 0, 1440
       if (noah1(k) > -1.E25) then
          if (.not. pd) then
             pd = .TRUE.
             call frstpt(float(k), noah1(k))
          else
             call vector(float(k), noah1(k))
          endif
       endif
    enddo
    call plotif(0., 0., 2)

    pd = .FALSE.
    call gsplci(color_index("red"))
    do k = 0, 1440
       if ( noah2(k) > -1.E25) then
          if ( .not. pd ) then
             call frstpt(float(k), noah2(k))
             pd = .TRUE.
          else
             call vector(float(k), noah2(k))
          endif
       endif
    enddo
    call plotif(0., 0., 2)

    call gsplci(color_index("black"))
    if (name3 /= "-") then
       pd = .FALSE.
       do k = 0, 1440
          if ( vald(k) > -1.E25) then
             if ( .not. pd ) then
                call frstpt(float(k), vald(k))
                pd = .TRUE.
             else
                call vector(float(k), vald(k))
             endif
          endif
       enddo
       call plotif(0., 0., 2)
    endif

    call gsplci(color_index("gray75"))
    call line(0.0, 0.0, 1440., 0.0)

    call gsplci(color_index("black"))
    call gasetc("YLF", "(F12.3)")
    call gasetc("XLF", "(I0)")
    call gridal(0,0,irange,0,    0,1,5,0.,0.)
  end subroutine plot_em_avg_val

!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------

  subroutine plot_em_dailymax_val(startdate, enddate, &
       name1, tag1, noah1, x1, options1, dt1, &
       name2, tag2, noah2, x2, options2, dt2, &
       name3, tag3, vald, xval, dtval)
    character(len=*),             intent(in) :: startdate, enddate
    character(len=*),             intent(in) :: name1, name2, name3
    character(len=256),           intent(in) :: tag1, tag2, tag3
    real, pointer, dimension(:)              :: noah1, noah2, vald
    real, pointer, dimension(:)              :: x1, x2, xval
    type(noah_mp_options_type),   intent(in) :: options1, options2

    real, intent(in) :: dt1, dt2, dtval
    real :: ymin, ymax
    integer :: expn, irange
    integer :: k, kstart, kend
    logical :: pd
    integer :: dtcount1, dtcount2, dtcountval
    integer, dimension(1) :: kk
    real :: x, y
    character(len=256) :: text
    character(len=256) :: text1
    character(len=256) :: text2

    real, parameter :: frame_left   = 0.15
    real, parameter :: frame_right  = 0.85
    real, parameter :: frame_bottom = 0.3
    real, parameter :: frame_top    = 0.7

    call make_sidelabels(options1, text1)
    call make_sidelabels(options2, text2)

    call set(0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)
    call pcseti("CC", color_index("blue"))
    call pchiqu(0.15, .97, trim(tag1)//": "//name1, 0.015, 0.0, -1.0)
    call pchiqu(.87, .80, trim(text1), 0.009, 0.0, -1.0)


    call pcseti("CC", color_index("red"))
    call pchiqu(0.15, .94, trim(tag2)//": "//name2, 0.015, 0.0, -1.0)
    call pchiqu(.87, .60, trim(text2), 0.009, 0.0, -1.0)


    call pcseti("CC", color_index("black"))
    if (name3 /= "-") then
       call pchiqu(0.15, .91, trim(tag3)//": "//name3, 0.015, 0.0, -1.0)
    endif

    write(text, '("From ", A4, "-", A2, "-", A2, "_", A2, ":", A2, " to ", A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
         startdate(1:4), startdate(5:6), startdate(7:8), startdate(9:10), startdate(11:12),  &
         enddate  (1:4), enddate  (5:6), enddate  (7:8), enddate  (9:10), enddate  (11:12)

    call pchiqu(0.18, 0.88, trim(text), 0.012, 0.0, -1.0)
    call pchiqu(.5, .09, "Maximum daily value", 0.018, 0.0, 0.0)

    ! Bottom axis labels:

    write(text, '(A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
         startdate(1:4), startdate(5:6), startdate(7:8), startdate(9:10), startdate(11:12)
    call pchiqu(frame_left, frame_bottom-0.020, trim(text), 0.009, 0., 0.)

    write(text, '(A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
         enddate(1:4), enddate(5:6), enddate(7:8), enddate(9:10), enddate(11:12)
    call pchiqu(frame_right, frame_bottom-0.020, trim(text), 0.009, 0., 0.)

    dtcount1   = nint(real(86400)/dt1)
    dtcount2   = nint(real(86400)/dt2)

    if (name3 /= "-") then
       dtcountval = nint(real(86400)/dtval)
    endif

    ymax = max(maxval(noah1),maxval(noah2))
    ymin = min(minval(noah1),minval(noah2))

    if (name3 /= "-") then
       ymax = max(ymax, maxval(vald, mask=(vald>-1.E25)))
       ymin = min(ymin, minval(vald, mask=(vald>-1.E25)))
    endif

    if ( abs(ymax-ymin) < 1.E-20 ) then
        if (ymax > 0) then
            ymax = ymax*1.05
            ymin = ymin*0.95
        elseif (ymax < 0) then
            ymax = ymax*0.95
            ymin = ymin*1.05
        elseif (ymax == 0) then
            ymax = 0.01
            ymin = -0.01
        endif
        call find_good_range(ymin, ymax, expn, irange)
    else
        call find_good_range(ymin, ymax, expn, irange)
    endif

    call set(frame_left, frame_right, frame_bottom, frame_top, x1(1), x1(size(noah1)), ymin, ymax, 1)

    pd = .FALSE.
    call gsplci(color_index("blue"))
    do k = 1, size(noah1), dtcount1
       kstart = k
       kend   = min(k + (dtcount1-1), size(noah1))
       kk = maxloc(noah1(kstart:kend)) + (kstart-1)
       x = x1(kk(1))
       y = noah1(kk(1))
       if (.not. pd) then
          call frstpt(x, y)
          pd = .TRUE.
       else
          call vector(x, y)
       endif
    enddo
    call plotif(0., 0., 2)

    pd = .FALSE.
    call gsplci(color_index("red"))
    do k = 1, size(noah2), dtcount2
       kstart = k
       kend   = min(k + (dtcount2-1), size(noah2))
       kk = maxloc(noah2(kstart:kend)) + (kstart-1)
       x = x2(kk(1))
       y = noah2(kk(1))
       if (.not. pd) then
          call frstpt(x, y)
          pd = .TRUE.
       else
          call vector(x, y)
       endif
    enddo
    call plotif(0., 0., 2)

    call gsplci(color_index("black"))
    if (name3 /= "-") then
       pd = .FALSE.
       do k = 1, size(vald), dtcountval
          kstart = k
          kend   = min(k + (dtcountval-1), size(vald))
          if (count(vald(kstart:kend)>-1.E25) > 0) then
             if (.not. pd) then
                call frstpt(xval(k), maxval(vald(kstart:kend), mask=(vald(kstart:kend)>-1.E25)))
                pd = .TRUE.
             else
                call vector(xval(k), maxval(vald(kstart:kend), mask=(vald(kstart:kend)>-1.E25)))
             endif
          else
             pd = .FALSE.
             call plotif(0., 0., 2)
          endif
       enddo
       call plotif(0., 0., 2)
    endif

    call gsplci(color_index("gray75"))
    call line(1.0, 0.0, x1(size(noah1)), 0.0)

    call gsplci(color_index("black"))
    call gasetc("YLF", "(F12.3)")
    call gasetc("XLF", "(I0)")
    call gridal(0,0,irange,0,    1,1,5,0.,0.)

  end subroutine plot_em_dailymax_val

!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------

  subroutine plot_em_dailymin_val(startdate, enddate, name1, tag1, noah1, x1, options1, dt1, name2, tag2, noah2, x2, options2, dt2, name3, tag3, vald, xval, dtval)
    character(len=*),             intent(in) :: startdate, enddate
    character(len=*),             intent(in) :: name1, name2, name3
    character(len=256),           intent(in) :: tag1, tag2, tag3
    real, pointer, dimension(:)              :: noah1, noah2, vald
    real, pointer, dimension(:)              :: x1, x2, xval
    type(noah_mp_options_type),   intent(in) :: options1, options2
    real,                         intent(in) :: dt1, dt2, dtval
    real :: ymin, ymax
    integer :: expn, irange
    integer :: k, kstart, kend
    logical :: pd
    integer :: dtcount1, dtcount2, dtcountval
    integer, dimension(1) :: kk
    character(len=256) :: text
    character(len=256) :: text1
    character(len=256) :: text2

    real :: x, y

    real, parameter :: frame_left   = 0.15
    real, parameter :: frame_right  = 0.85
    real, parameter :: frame_bottom = 0.3
    real, parameter :: frame_top    = 0.7

!KWM    call make_sidelabels(options1, text1)
!KWM    call make_sidelabels(options2, text2)

    call set(0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)

!KWM    call pcseti("CC", color_index("blue"))
!KWM    call pchiqu(0.15, .97, trim(tag1)//": "//name1, 0.015, 0.0, -1.0)
!KWM    call pchiqu(.87, .80, trim(text1), 0.009, 0.0, -1.0)
!KWM
!KWM    call pcseti("CC", color_index("red"))
!KWM    call pchiqu(0.15, .94, trim(tag2)//": "//name2, 0.015, 0.0, -1.0)
!KWM    call pchiqu(.87, .60, trim(text2), 0.009, 0.0, -1.0)
!KWM
!KWM    call pcseti("CC", color_index("black"))
!KWM    if (name3 /= "-") then
!KWM       call pchiqu(0.15, .91, trim(tag3)//": "//name3, 0.015, 0.0, -1.0)
!KWM    endif

!KWM    write(text, '("From ", A4, "-", A2, "-", A2, "_", A2, ":", A2, " to ", A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
!KWM         startdate(1:4), startdate(5:6), startdate(7:8), startdate(9:10), startdate(11:12),  &
!KWM         enddate  (1:4), enddate  (5:6), enddate  (7:8), enddate  (9:10), enddate  (11:12)
!KWM
!KWM    call pchiqu(0.18, 0.88, trim(text), 0.012, 0.0, -1.0)

    call pchiqu(.5, .04, "Minimum daily value", 0.018, 0.0, 0.0)

!KWM    ! Bottom axis labels:
!KWM
!KWM    write(text, '(A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
!KWM         startdate(1:4), startdate(5:6), startdate(7:8), startdate(9:10), startdate(11:12)
!KWM    call pchiqu(frame_left, frame_bottom-0.020, trim(text), 0.009, 0., 0.)
!KWM
!KWM    write(text, '(A4, "-", A2, "-", A2, "_", A2, ":", A2)') &
!KWM         enddate(1:4), enddate(5:6), enddate(7:8), enddate(9:10), enddate(11:12)
!KWM    call pchiqu(frame_right, frame_bottom-0.020, trim(text), 0.009, 0., 0.)




    dtcount1   = nint(real(86400)/dt1)
    dtcount2   = nint(real(86400)/dt2)

    if (name3 /= "-") then
       dtcountval = nint(real(86400)/dtval)
    endif

    ymin = min(minval(noah1),minval(noah2))
    ymax = max(maxval(noah1),maxval(noah2))
    if (name3 /= "-") then
       ymin = min(ymin, minval(vald, mask=(vald>-1.E25)))
       ymax = max(ymax, maxval(vald, mask=(vald>-1.E25)))
    endif

    if ( abs(ymax-ymin) < 1.E-20 ) then
        if (ymax > 0) then
            ymax = ymax*1.05
            ymin = ymin*0.95
        elseif (ymax < 0) then
            ymax = ymax*0.95
            ymin = ymin*1.05
        elseif (ymax == 0) then
            ymax = 0.01
            ymin = -0.01
        endif
        call find_good_range(ymin, ymax, expn, irange)
    else
        call find_good_range(ymin, ymax, expn, irange)
    endif

    call set(frame_left, frame_right, frame_bottom, frame_top, x1(1), x1(size(noah1)), ymin, ymax, 1)

    pd = .FALSE.
    call gsplci(color_index("blue"))
    do k = 1, size(noah1), dtcount1
       kstart = k
       kend   = min(k + (dtcount1-1), size(noah1))
       kk = minloc(noah1(kstart:kend)) + (kstart-1)
       x = x1(kk(1))
       y = noah1(kk(1))
       if (.not. pd) then
          call frstpt(x, y)
          pd = .TRUE.
       else
          call vector(x, y)
       endif
    enddo
    call plotif(0., 0., 2)

    pd = .FALSE.
    call gsplci(color_index("red"))
    do k = 1, size(noah2), dtcount2
       kstart = k
       kend   = min(k + (dtcount2-1), size(noah2))
       kk = minloc(noah2(kstart:kend)) + (kstart-1)
       x = x2(kk(1))
       y = noah2(kk(1))
       if (.not. pd) then
          call frstpt(x, y)
          pd = .TRUE.
       else
          call vector(x, y)
       endif
    enddo
    call plotif(0., 0., 2)

    call gsplci(color_index("black"))

    if (name3 /= "-") then
       pd = .FALSE.
       do k = 1, size(vald), dtcountval
          kstart = k
          kend   = min(k + (dtcountval-1), size(vald))
          if (count(vald(kstart:kend)>-1.E25) > 0) then
             if (.not. pd) then
                call frstpt(xval(k), minval(vald(kstart:kend), mask=(vald(kstart:kend)>-1.E25)))
                pd = .TRUE.
             else
                call vector(xval(k), minval(vald(kstart:kend), mask=(vald(kstart:kend)>-1.E25)))
             endif
          else
             pd = .FALSE.
             call plotif(0., 0., 2)
          endif
       enddo
       call plotif(0., 0., 2)
    endif

    call gsplci(color_index("gray75"))
    call line(1.0, 0.0, x1(size(noah1)), 0.0)

    call gsplci(color_index("black"))
    call gasetc("YLF", "(F12.3)")
    call gasetc("XLF", "(I0)")
    call gridal(0,0,irange,0,    1,1,5,0.,0.)
  end subroutine plot_em_dailymin_val

!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------

  subroutine make_sidelabels(options, text)
    type(noah_mp_options_type),   intent(in)  :: options
    character(len=256),           intent(out) :: text

    character(len=1024) :: fmt
    fmt = '('
    fmt = trim(fmt) // '"File: ", A'
    fmt = trim(fmt) // ',' // '"~C~DVEG: ", I1'
    fmt = trim(fmt) // ',' // '"~C~CRS: ", I1'
    fmt = trim(fmt) // ',' // '"~C~BTR: ", I1'
    fmt = trim(fmt) // ',' // '"~C~RUN: ", I1'
    fmt = trim(fmt) // ',' // '"~C~SFC: ", I1'
    fmt = trim(fmt) // ',' // '"~C~FRZ: ", I1'
    fmt = trim(fmt) // ',' // '"~C~INF: ", I1'
    fmt = trim(fmt) // ',' // '"~C~RAD: ", I1'
    fmt = trim(fmt) // ',' // '"~C~ALB: ", I1'
    fmt = trim(fmt) // ',' // '"~C~SNF: ", I1'
    fmt = trim(fmt) // ',' // '"~C~TBOT: ", I1'
    fmt = trim(fmt) // ',' // '"~C~STC: ", I1'
    fmt = trim(fmt) // ')'

    write(text, FMT=trim(fmt)) &
         trim(options%filename), options%opt_dveg, options%opt_crs, options%opt_btr, options%opt_run, options%opt_sfc, &
         options%opt_frz, options%opt_inf, options%opt_rad, options%opt_alb, options%opt_snf, options%opt_tbot, options%opt_stc

  end subroutine make_sidelabels

!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------

  recursive subroutine read_nc_single_field(ncid, name, startdate, enddate, noah, x, units, description, avg_day)
    use module_netcdf_io
    use kwm_date_utilities
    implicit none
    integer,                        intent(in)    :: ncid
    character(len=*),               intent(in)    :: name
    character(len=12),              intent(inout) :: startdate
    character(len=12),              intent(inout) :: enddate
    real,             dimension(:), pointer       :: noah
    real,             dimension(:), pointer       :: x
    character(len=256),             intent(out)   :: units
    character(len=256),             intent(out)   :: description
    real,             dimension(0:1440)           :: avg_day

    integer :: noah_startindex
    integer :: noah_endindex
    character(len=12), pointer, dimension(:) :: noah_times
    integer :: itime
    integer :: lvl
    integer :: indx
    real, pointer, dimension(:,:) :: noah2d
    integer :: i
    integer :: idtm
    real, dimension(0:1440) :: acount

    ! For sum:
    character(len=256) :: name1, name2
    character(len=12)  :: startdate2
    character(len=12)  :: enddate2
    real,             dimension(:), pointer       :: noah2, x2
    character(len=256)   :: units2
    character(len=256)   :: description1, description2
    real,             dimension(0:1440)           :: avg_day1
    real,             dimension(0:1440)           :: avg_day2

    indx = index(name,"+")
    if (indx > 0) then
       startdate2 = startdate
       enddate2   = enddate
       name1 = name(1:indx-1)
       name2 = name(indx+1:)
       print*, 'name1 = ', trim(name1)
       print*, 'name2 = ', trim(name2)
       call read_nc_single_field(ncid, trim(name1), startdate , enddate , noah , x , units , description1, avg_day1)
       call read_nc_single_field(ncid, trim(name2), startdate2, enddate2, noah2, x2, units2, description2, avg_day2)
       noah = noah + noah2
       deallocate(noah2, x2)
       nullify(noah2, x2)
       avg_day = avg_day1 + avg_day2
       description = "sum"
       return
    endif


    call get_from_netcdf_output(ncid, "Times", noah_times)
    if (startdate == "000000000000") then
       noah_startindex = 1
       startdate = noah_times(noah_startindex)
       write(*,'("Setting start date to ", A)') startdate
    else
       noah_startindex = -1
       do itime = 1, size(noah_times)
          if (noah_times(itime) == startdate) then
             noah_startindex = itime
          endif
       enddo
    endif

    if (enddate == "000000000000") then
       noah_endindex = size(noah_times)
       enddate = noah_times(noah_endindex)
       write(*,'("Setting  end  date to ", A)') enddate
    else
       noah_endindex = -1
       do itime = noah_startindex, size(noah_times)
          if (noah_times(itime) == enddate) then
             noah_endindex = itime
             exit
          endif
       enddo
    endif

    indx = index(name,":Level")

    if (indx > 0) then
       read( name(indx+6:),*) lvl

       call get_from_netcdf_output(ncid,  name(1:indx-1), noah2d, units, description, noah_startindex, noah_endindex)

       allocate(noah(size(noah2d,2)))
       noah = noah2d(lvl,:)

       deallocate(noah2d)
       nullify(noah2d)
    else
       call get_from_netcdf_output(ncid,  name, noah, units, description, noah_startindex, noah_endindex)
    endif

    allocate(x(size(noah)))

    avg_day = 0.0
    acount = 0

    ! <noah_times> has strings out to minutes. So x is in minutes.

    do i = 1, size(x)
       call geth_idts(noah_times(i+noah_startindex-1), startdate, idtm)
       x(i) = real(idtm)
       avg_day(mod(idtm,1440)) = avg_day(mod(idtm,1440)) + noah(i)
       acount(mod(idtm,1440)) = acount(mod(idtm,1440)) + 1
    enddo
    where (acount > 0)
       avg_day = avg_day / real(acount)
    elsewhere
       avg_day = -1.E36
    endwhere

    deallocate(noah_times)
    nullify(noah_times)


  end subroutine read_nc_single_field


end module module_g2_graphics

program g2
  use module_bondville_validation_data
  use module_netcdf_io
  use kwm_date_utilities
  use kwm_plot_utilities
  use arguments_module
  use module_g2_graphics
  implicit none

  integer :: ncid1, ncid2
  logical :: lhelp
  character(len=12) :: startdate
  character(len=12) :: enddate
  character(len=64) :: noah_name1, noah_name2, vald_name
  real, pointer, dimension(:) :: noah1, noah2, vald, vald_new
  real, pointer, dimension(:) :: x1, x2, xval
  real, dimension(0:1440) :: avg_day1, avg_day2, avg_day_vald
  character(len=256) :: units1, units2
  character(len=256) :: description1, description2
  character(len=256) :: tag1, tag2, valtag

  ! Timesteps (s) in the various data sets
  real :: dt1
  real :: dt2
  real :: dtval

  type(noah_mp_options_type) :: options1, options2
  integer :: ierr

  logical :: minmax
  logical :: average_diurnal_cycle
  logical :: invert2
  logical :: scatter
  character(len=256) :: validation_file_name
  namelist /plot_instructions/ noah_name1, noah_name2, vald_name, scatter, minmax, average_diurnal_cycle, invert2

  namelist /datetime/ validation_file_name, startdate, enddate

  character(len=256) :: graphics_namelist_file

  arguments_help = '("-namelist <namelist_file> [-startdate <YYYYMMDDHHmm>] [-enddate <YYYYMMDDHHmm>] FILE1.nc FILE2.nc")'

  call arg("-help", .FALSE., lhelp)
  if (lhelp) then
     call print_help
  endif

  call arg("-h", .FALSE., lhelp)
  if (lhelp) then
     call print_help
  endif

  call arg("--help", .FALSE., lhelp)
  if (lhelp) then
     call print_help
  endif

  call arg("-namelist", "Null", graphics_namelist_file)

  open(15, file=trim(graphics_namelist_file), status='old', form='formatted', action='read')
  read(15, datetime, iostat=ierr)
  if (ierr /= 0) stop " +++ OUT OF CHEESE ERROR +++ "

  call arg("-startdate", startdate, startdate)
  call arg("-enddate",   enddate,   enddate)

  if ( len_trim(startdate) == 8 ) then
     startdate = trim(startdate) // "00"
  endif

  if ( len_trim(startdate) == 10 ) then
     startdate = trim(startdate) // "00"
  endif

  if ( len_trim(enddate) == 8 ) then
     enddate = trim(enddate) // "00"
  endif

  if ( len_trim(enddate) == 10 ) then
     enddate = trim(enddate) // "00"
  endif

  print*, 'startdate = ', startdate
  print*, 'enddate   = ', enddate

  call arg(" ", options1%filename)
  print*,' ncfile1 = ', trim(options1%filename)
  call arg(" ", options2%filename)
  print*,' ncfile1 = ', trim(options2%filename)

  call output_open_for_read(options1%filename, ncid1, options1%dt, options1%opt_dveg, options1%opt_crs, options1%opt_btr, &
       options1%opt_run, options1%opt_sfc, options1%opt_frz, options1%opt_inf, options1%opt_rad, options1%opt_alb, &
       options1%opt_snf, options1%opt_tbot, options1%opt_stc, tag1)

  call output_open_for_read(options2%filename, ncid2, options2%dt, options2%opt_dveg, options2%opt_crs, options2%opt_btr, &
       options2%opt_run, options2%opt_sfc, options2%opt_frz, options2%opt_inf, options2%opt_rad, options2%opt_alb, &
       options2%opt_snf, options2%opt_tbot, options2%opt_stc, tag2)

  valtag = "Bondville validation data"

  dt1 = options1%dt
  dt2 = options2%dt

  call kwm_init_ncargks("compare.cgm")

  PLOTLOOP : do

     ! Defaults for each namelist read
     noah_name1 = "-"
     noah_name2 = "-"
     vald_name = "-"
     minmax    = .FALSE.
     average_diurnal_cycle = .FALSE.
     invert2 = .FALSE.
     scatter = .FALSE.

     ! Read each namelist
     read(15, plot_instructions, iostat=ierr)
     if (ierr /= 0) exit PLOTLOOP
     write(*,plot_instructions)

     ! Do the plots as instructed from the namelist

     if (noah_name1 /= "-") then
        call read_nc_single_field(ncid1, noah_name1, startdate, enddate, noah1, x1, units1, description1, avg_day1)
     endif
     if (noah_name2 /= "-") then
        call read_nc_single_field(ncid2, noah_name2, startdate, enddate, noah2, x2, units2, description2, avg_day2)
        if (invert2) then
           noah2 = -noah2
           where (avg_day2 > -1.E25)
              avg_day2 = -avg_day2
           endwhere
        endif
     endif
     if (vald_name /= "-") then
        call read_bondville_validation(trim(validation_file_name), vald_name, startdate, enddate, dtval, vald, xval, avg_day_vald)

        ! While we're at it, let's interpolate the validation data to the same times as the Noah output.
        call interp_val(xval, x1, vald, vald_new)

     endif

     call plot_em_val(startdate, enddate, &
          trim(noah_name1), trim(description1), tag1, noah1, x1, options1, &
          trim(noah_name2), trim(description2), tag2, noah2, x2, options2, vald_name, valtag, vald, xval)
     call frame()

     if (vald_name /= "-") then
        if ( scatter ) then
           call plot_em_scatter(startdate, enddate, &
                trim(noah_name1), trim(description1), tag1, noah1, x1, options1, &
                trim(noah_name2), trim(description2), tag2, noah2, x2, options2, &
                vald_name, valtag, vald_new, x1)
           call frame()
        endif
     endif

     if (minmax) then
        call plot_em_dailymax_val(startdate, enddate, noah_name1, tag1, noah1, x1, options1, dt1, &
             noah_name2, tag2, noah2, x2, options2, dt2, &
             vald_name, valtag, vald, xval, dtval)
        call plot_em_dailymin_val(startdate, enddate, noah_name1, tag1, noah1, x1, options1, dt1, &
             noah_name2, tag2, noah2, x2, options2, dt2, &
             vald_name, valtag, vald, xval, dtval)
        call frame()
     endif

     if (average_diurnal_cycle) then
        call plot_em_avg_val(startdate, enddate, &
             trim(noah_name1), trim(description1), tag1, avg_day1, options1, &
             trim(noah_name2), trim(description2), tag2, avg_day2, options2, vald_name, valtag, avg_day_vald)
        call frame()
     endif

     if (noah_name1 /= "-") then
        deallocate(noah1, x1)
        nullify(noah1, x1)
     endif
     if (noah_name2 /= "-") then
        deallocate(noah2, x2)
        nullify(noah2, x2)
     endif

     if ( vald_name /= "-") then
        deallocate(vald, xval)
        nullify(vald, xval)
     endif

  enddo PLOTLOOP

  close(15)

  call kwm_close_ncargks()

end program g2

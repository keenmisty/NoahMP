module module_bondville_validation_data
  use kwm_date_utilities
  use kwm_string_utilities
  implicit none
  ! Bondville data are every thirty minutes.

contains
  subroutine read_bondville_validation(file_name, field_name, startdate, enddate, dt, val, xval, avg_day)
    implicit none
    character(len=*),        intent(in)  :: file_name
    character(len=*),        intent(in)  :: field_name
    character(len=12),       intent(in)  :: startdate
    character(len=12),       intent(in)  :: enddate
    real,                    intent(out) :: dt
    real, pointer, dimension(:)          :: val
    real, pointer, dimension(:)          :: xval
    real, dimension(0:1440), intent(out) :: avg_day

    integer, dimension(0:1440)           :: acount

    character(len=256), parameter    :: rdfmt = '(A4,4(1x,A2), 31(F12.5))'

    character(len=12) :: readdate
    real              :: w_speed
    real              :: w_dir
    real              :: t
    real              :: rh
    real              :: p
    real              :: rg
    real              :: par_in
    real              :: par_out
    real              :: rnet
    real              :: ghf
    real              :: rain
    real              :: wet
    real              :: skn_irt
    real              :: t_02
    real              :: t_04
    real              :: t_08
    real              :: t_16
    real              :: t_32
    real              :: t_64
    real              :: u_bar
    real              :: eddyuw
    real              :: uprim2
    real              :: vprim2
    real              :: wprim2
    real              :: H
    real              :: le
    real              :: co2
    real              :: lw_in
    real              :: sm_05
    real              :: sm_20
    real              :: sm_60

    integer :: ierr

    integer :: ntimes
    integer :: itime
    integer :: idt
    character(len=12) :: nowdate

    integer, parameter :: iunit = 41
    character(len=6) :: test_string

    real :: es, qs, q

    real, parameter :: E0  = 611.2    ! Pa
    real, parameter :: T00 = 273.15
    real, parameter :: EPS   = 0.622
    real, parameter :: SVP2  = 17.67
    real, parameter :: SVP3  = 29.65

    real :: emissi_to_use
    real :: net_longwave

    open(iunit, file=trim(file_name), status='old', form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'("Problem opening validation file ''",A,"''")'), trim(file_name)
       stop "Problem opening validation file."
    endif

    dt = 1800.0  ! a priori knowledge about this dataset

    acount = 0
    avg_day = 0

    ! Read past the file header information, until we reach the <Data> tag:
    PRELOOP : do
       read(iunit, '(A6)') test_string
       if ( upcase(test_string) == "<DATA>" ) exit PRELOOP
    enddo PRELOOP

    READLOOP : do 

       read(iunit, FMT=trim(rdfmt), iostat=ierr) &
            readdate(1:4), readdate(5:6), readdate(7:8), readdate(9:10), readdate(11:12), &
            w_speed,  w_dir,      &
            T,   RH,  P,  Rg, Par_in,   Par_out,        &
            rnet,   GHF, RAIN,   wet,  SKN_IRT,         &
            T_02,  T_04,  T_08,  T_16, T_32, T_64,      &
            u_bar,  eddyuw, uprim2,   vprim2,   wprim2, &
            H,  LE, CO2, LW_in,                         &
            sm_05, sm_20, sm_60

       if (ierr < 0) then
          write(*,'("Hit the end of file.")')
          write(*,'("Unexpected error exit.")')
          stop
       endif
       if (ierr /= 0) then
          write(*,'("Error reading from data file.")')
          stop
       endif

       nowdate = readdate

       ! The bondville data were originally in local time.
       ! I've re-dated the data in our validation file, so 
       ! this date correction is no longer needed.
       ! call geth_newdate(nowdate, readdate, 360) 

       if ( nowdate < startdate ) cycle READLOOP
       if ( nowdate > enddate   ) exit READLOOP

       if (par_out < -6900) then
          par_out = -1.E36
       endif
       if (RNET < -6900) then
          RNET = -1.E36
       endif
       if (LW_IN < -6900) then
          LW_IN = -1.E36
       endif
       if (Rg < -6900) then
          Rg = -1.E36
       endif
       if (T < -6900) then
          T = -1.E36
       else
          T = T + 273.15
       endif
       if (P < -6900) then
          P = -1.E36
       else
          P = P * 1.E2
       endif
       if (H < -6900) then
          H = -1.E36
       endif
       if (GHF < -6900) then
          GHF = -1.E36
       endif
       if (LE < -6900) then
          LE = -1.E36
       endif
       if (T_02 < -6900) then
          T_02 = -1.E36
       endif
       if (T_04 < -6900) then
          T_04 = -1.E36
       endif
       if (T_08 < -6900) then
          T_08 = -1.E36
       endif
       if (T_16 < -6900) then
          T_16 = -1.E36
       endif
       if (T_32 < -6900) then
          T_32 = -1.E36
       endif
       if (T_64 < -6900) then
          T_64 = -1.E36
       endif
       if (skn_irt < -6900) then
          skn_irt = -1.E36
       else
          skn_irt = skn_irt + 273.15
       endif

       if ( .not. associated(val)) then
          call geth_idts(enddate, startdate, idt)
          ntimes = ( idt / 30 ) + 1 ! data are every thirty minutes
          ! print*, 'ntimes = ', ntimes
          allocate(val(ntimes))
          allocate(xval(ntimes))
       endif

       call geth_idts(nowdate, startdate, idt)
       itime = ( idt / 30 ) + 1

       ! <xval> is the time in minutes.
       if ( field_name == "H" ) then
          val(itime) = H
       else if ( field_name == "Q" ) then
          ! print*, 't, p, rh = ', t, p, rh
          es=e0*exp(svp2*(t-t00)/(t-svp3))
          qs = (eps*es)/(p-es)
          q = (rh*1.E-2) * qs
          ! print*, 't, p, rh, q = ', t, p, rh, q
          val(itime) = q
       else if ( field_name == "LE" ) then
          val(itime) = LE
       else if ( field_name == "PAR_OUT" ) then
          val(itime) = par_out
       else if ( field_name == "PAR_ABS" ) then
          if ( ( rg > -1.E25 ) .and. (par_out > -1.E25) ) then
             val(itime) = Rg - par_out
          else
             val(itime) = -1.E36
          endif
       else if ( field_name == "LW_IN" ) then
          val(itime) = lw_in
       else if ( field_name == "RNET" ) then
          val(itime) = RNET
       else if ( field_name == "IRT" ) then
          val(itime) = skn_irt
       else if ( field_name == "sigma*T**4" ) then
          if (skn_irt > -1.E25) then
             val(itime) = 5.67E-8 * skn_irt * skn_irt * skn_irt * skn_irt
          else
             val(itime) = -1.E36
          endif
       else if ( field_name(1:20) == "compute net longwave" ) then
          read(field_name(22:),*) emissi_to_use
          ! print*, 'emissi_to_use = ', emissi_to_use
          if ((skn_irt > -1.E25).and.(lw_in > -1.E25)) then
             val(itime) = emissi_to_use * ( ( 5.67E-8 * skn_irt * skn_irt * skn_irt * skn_irt ) - lw_in )
          else
             val(itime) = -1.E36
          endif
       else if ( field_name(1:8) == "residual" ) then
          read(field_name(10:),*) emissi_to_use
          ! print*, 'emissi_to_use = ', emissi_to_use
          if ( ( skn_irt > -1.E25 ) .and. &
               ( lw_in   > -1.E25 ) .and. &
               ( rg      > -1.E25 ) .and. &
               ( par_out > -1.E25 ) .and. &
               ( h       > -1.E25 ) .and. &
               ( le      > -1.E25 ) .and. &
               ( ghf     > -1.E25 ) ) then
             net_longwave = emissi_to_use * ( ( 5.67E-8 * skn_irt * skn_irt * skn_irt * skn_irt ) - lw_in )
             val(itime) = net_longwave + h + le + ghf - ( Rg - par_out )
          else
             val(itime) = -1.E36
          endif
       else if ( field_name == "GHF" ) then
          val(itime) = GHF
       else if ( field_name == "Par_in" ) then
          val(itime) = Par_in
       else if ( ( field_name == "Rg" ) .or. ( field_name == "RG" ) ) then
          val(itime) = Rg
       else if ( field_name == "T_02" ) then
          val(itime) = T_02 + 273.15
       else if ( field_name == "T_04" ) then
          val(itime) = T_04 + 273.15
       else if ( field_name == "T_08" ) then
          val(itime) = T_08 + 273.15
       else if ( field_name == "T_16" ) then
          val(itime) = T_16 + 273.15
       else if ( field_name == "T_32" ) then
          val(itime) = T_32 + 273.15
       else if ( field_name == "T_64" ) then
          val(itime) = T_64 + 273.15
       else if ( field_name == "SM_05" ) then
          val(itime) = SM_05
       else if ( field_name == "SM_20" ) then
          val(itime) = SM_20
       else if ( field_name == "SM_60" ) then
          val(itime) = SM_60
       else if ( field_name == "T" ) then
          val(itime) = t
       else
          print*, 'Unrecognized validation field requested:  "'//trim(field_name)//'"'
          stop "read_bondville_validation"
       endif
       xval(itime) = real(idt)

       if ( val(itime) > -1.E25 ) then
          avg_day(mod(idt,1440)) = avg_day(mod(idt,1440)) + val(itime)
          acount(mod(idt,1440)) = acount(mod(idt,1440)) + 1
       endif

    enddo READLOOP

    where(acount > 0) 
       avg_day = avg_day / real(acount)
    elsewhere
       avg_day = -1.E36
    endwhere

    close(iunit)


  end subroutine read_bondville_validation
end module module_bondville_validation_data

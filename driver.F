program driver
  use module_io
  use module_sf_noahmplsm, only : dveg, opt_crs, opt_btr, opt_run, opt_sfc, opt_frz, opt_inf, opt_rad, &
       &                          opt_alb, opt_snf, opt_tbot, opt_stc, noahmp_options, redprm, noahmp_sflx, &
       &                          read_mp_veg_parameters
  use kwm_date_utilities

  implicit none

  real, external :: month_d

  character(len=12)   :: startdate        ! Starting date of the data ( YYYYMMDDHHmm ) 
  character(len=12)   :: enddate          ! Ending date of the data ( YYYYMMDDHHmm )
  integer             :: forcing_timestep ! The time interval ( seconds ) of the data in the forcing file
  integer             :: noahlsm_timestep ! The timestep ( seconds ) to use when integrating the Noah LSM
  real, dimension(12) :: albedo_monthly   ! Monthly values of background (i.e., snow-free) albedo ( Fraction [0.0-1.0] )
  real, dimension(12) :: shdfac_monthly   ! Monthly values for green vegetation fraction ( Fraction [0.0-1.0] )
  real, dimension(12) :: z0brd_monthly    ! Monthly values for background (i.e., snow-free) roughness length ( m )
  real, dimension(12) :: lai_monthly      ! Monthly values for Leaf Area Index ( dimensionless )
  real :: cmc
  integer :: ktime
  integer :: n

  character(len=256) :: executable_name   ! The name of the executable, as found by Fortran library 
  !                                       ! function GETARG

  character(len=256) :: forcing_filename  ! The name of the initial/forcing conditions file, as found
  !                                       ! by Fortran library function GETARG

  character(len=4096) :: infotext         ! Character string returned by subroutine OPEN_FORCING_FILE, 
                                          ! giving some possibly useful information for the user.

  integer :: ierr
  integer, parameter :: iunit = 10
  real :: latitude, longitude
  integer :: isurban
  real :: longwave
  logical :: rdlai2d
  character(len=12) :: nowdate
  real :: sfcspd, sfcu, sfcv
  integer :: slopetype

  real, pointer, dimension(:) :: SLDPTH ! Thicknesses of each soil level
  real, pointer, dimension(:) :: STC_PTR ! Thicknesses of each soil level
  real, pointer, dimension(:) :: SMC_PTR ! Thicknesses of each soil level
  real, pointer, dimension(:) :: SH2O_PTR ! Thicknesses of each soil level
  real :: snoalb
  integer :: soiltype
  real :: t1v, t1, th2v, zlvl_wind
  logical :: use_urban_module, usemonalb


  character(len=256) :: LLANDUSE  ! Land-use dataset.  Valid values are :
  !                               ! "USGS" (USGS 24/27 category dataset) and
  !                               ! "MODIFIED_IGBP_MODIS_NOAH" (MODIS 20-category dataset)
  character(len=256) :: LSOIL = "STAS" ! Soil-category dateset.  Only "STAS" (STATSGO dataset) supported.

  character(len=256) :: teststr
  character(len=256) :: cmd
  integer :: reloop_count = 0
  integer :: loop_for_a_while

  integer :: ice
  integer :: ist
  integer :: vegtype
  integer :: isc
  integer :: nsnow
  integer :: nsoil
  real, allocatable, dimension(:) :: zsoil
  real    :: dt
  real    :: q2
  real    :: sfctmp
  real    :: uu
  real    :: vv
  real    :: soldn
  real    :: lwdn
  real    :: prcp
  real    :: zlvl
  real    :: co2air
  real    :: o2air
  real    :: cosz
  real    :: tbot
  real    :: foln
  real    :: sfcprs
  integer :: yearlen
  real    :: julian
  real    :: shdfac
  real    :: shdmax
  real    :: lat
  real    :: z0
  integer :: ix
  integer :: iy
  integer :: ipoint
  real    :: eah
  real    :: tah
  real    :: fwet
  real, allocatable, dimension(:) :: ficeold
  real    :: qsnow
  real    :: sneqvo
  integer :: isnow
  real, allocatable, dimension(:) :: zsnso
  real    :: canliq
  real    :: canice
  real    :: snowh
  real    :: sneqv
  real, allocatable, dimension(:) :: snice
  real, allocatable, dimension(:) :: snliq
  real    :: tv
  real    :: tg
  real, allocatable, dimension(:) :: stc
  real, allocatable, dimension(:) :: sh2o
  real, allocatable, dimension(:) :: smc
  real, allocatable, dimension(:) :: tsno
  real    :: zwt
  real    :: wa
  real    :: wt
  real    :: wslake
  real    :: lfmass
  real    :: rtmass
  real    :: stmass
  real    :: wood
  real    :: stblcp
  real    :: fastcp
  real    :: lai
  real    :: sai
  real    :: albold
  real    :: cm
  real    :: ch

  real    :: FSA
  real    :: FSR
  real    :: FIRA
  real    :: FSH
  real    :: SSOIL
  real    :: FCEV
  real    :: FGEV
  real    :: FCTR
  real    :: TRAD
  real    :: ECAN
  real    :: ETRAN
  real    :: EDIR
  real    :: RUNSRF
  real    :: RUNSUB
  real    :: APAR
  real    :: PSN
  real    :: SAV
  real    :: SAG
  real    :: FSNO
  real    :: NEE
  real    :: GPP
  real    :: NPP
  real    :: TS
  real    :: FVEG
  real    :: ALBEDO

  real    :: qfx
  real    :: flxsum
  real    :: ir_sh_ev_gh
  real    :: fsa_fsr
  real    :: dz8w
  real    :: bgap
  real    :: wgap
  real    :: tgv
  real    :: tgb
  real    :: tstar
  real    :: t2mb
  real    :: t2mv
  real    :: rssun
  real    :: rssha
  real    :: qsfc
  real    :: q1
  real    :: q2v
  real    :: q2b
  real    :: qmelt
  real    :: qc
  real    :: ponding
  real    :: ponding1
  real    :: ponding2
  real    :: psfc
  real    :: pblh
  real    :: chstar
  real    :: tauss
  real    :: chstar2
  real    :: dx
  real    :: errwat
  real    :: gap
  real    :: cqstar2
  integer :: iz0tlnd
  real    :: chv
  real    :: chb
  real    :: emissi
  character(len=1024) :: output_dir

  call getarg(0, executable_name)
  call getarg(1, forcing_filename)

  if (forcing_filename == "") then
     write(*,'(/," ***** Problem:  Program expects a command-line argument *****")')
     write(*,'(" ***** Please specify the forcing filename on the command-line.")')
     write(*,'(" ***** E.g.:  ''",A,1x,A,"''",/)') trim(executable_name), "bondville.dat"
     stop ":  ERROR EXIT"
  endif

  ! LOOP_FOR_A_WHILE:  Number of times to rerun the same year of forcing.

  ! ICE:     Ice flag: 0 = no ice, 1 = ice [ may want to expand this to include sea-ice as -1, like the Noah3.1. ]
  ! IST:     Surface type 1=soil; 2=lake;  [ LSM now runs over lakes? Over water points? ]
  ! VEGTYPE:  Vegetation type.  [ There are checks on hard-coded vegetation types.  This needs to be corrected. ]
  ! ISC:     Soil color type, an integer index from 1 to 8 [ but code checks on ISC == 9 ? ]
  ! NSNOW:   Maximum number of snow layers.  [ Is this adjustable? ]
  ! NSOIL:   Number of soil layers.
  ! ZSOIL:   Depth (from ground surface) of layer bottom [ ground surface and not snow surface? ]  [ Negative numbers? ]
  ! DT:      Time step in seconds
  ! Q2:      Mixing ratio (kg/kg) [ at some level above ground ]
  ! SFCTMP:  Air temperature (K)  [ at some level above ground ]
  ! UU:      U-component of the wind (m/s)
  ! VV:      V-component of the wind (m/s)
  ! SOLDN:   Downward solar radiation ( W m{-2} )
  ! LWDN:    Downward longwave radiation ( W m{-2} )
  ! PRCP:    Precipitation rate ( kg m{-2} s{-1} )
  ! ZLVL:    Reference height (m) [ may be modified by SFLX->ENERGY, so should properly go in the (inout) block ]
  ! CO2AIR:  Atmospheric C02 concentration ( Pa )  [ Partial pressure, not concentration? ]
  ! O2AIR:   Atmospheric O2 concentration ( Pa ) [ Partial pressure, not concentration? ]
  ! COSZ:    Cosine of the solar zenith angle ( 0.0 - 1.0 )
  ! TBOT:    Deep-layer soil temperature (K)
  ! FOLN:    Foliage nitrogen (%) (1.0 = saturated)  [ Fraction, not percentage ? ]
  ! SFCPRS:  Pressure (Pa) [ At what level? ]
  ! YEARLEN: Number of days in the particular year
  ! JULIAN:  Julian day
  ! SHDFAC:  Green vegetation fraction ( 0.0 - 1.0 ), used only for DVEG=1 option
  ! LAT:     Latitude ( radians )
  ! Z0:      Roughness length ( m )
  ! IX:      Grid index in the x dimension [ unused ]
  ! IY:      Grid index in the y dimension [ unused ]
  ! IPOINT:  Grid index [ used only for informational print ]

  ! EAH:     Canopy air vapor pressure (Pa)
  ! TAH:     Canopy air temperature (K)
  ! FWET:    Wetted or snowed fraction of canopy 
  ! FICEOLD: Ice fraction at the last timestep. [ Should be in the (inout) block ? ]
  ! QSNOW:   Snowfall ( mm/s ) [ Snow accumulation rate? Water equivalent? ]
  ! SNEQVO:  Snow mass at the last time step (mm) (kg m{-2})
  ! ISNOW:   Actual number of snow layers.   [ negative ? ] [ a prognostic variable]
  ! ZSNSO:   Snow/soil layer-bottom depth from snow surface (m)
  ! CANLIQ:  Intercpted liquid water (mm)
  ! CANICE:  Intercepted ice mass (mm)
  ! SNOWH:   Snow height (m)
  ! SNEQV:   Snow water equivalent (mm)
  ! SNICE:   Snow-layer ice (mm) [?]
  ! SNLIQ:   Snow-layer liquid water (mm)
  ! TV:      Vegetation temperature (K)
  ! TG:      Ground temperature (K) [ Skin temperature ? ]
  ! STC:     Snow/soil temperature (K)
  ! SH2O:    Volumetric liquid soil moisture (m{3} m{-3})
  ! SMC:     Volumetric soil moisture, ice + liquid (m{3} m{-3})
  ! ZWT:     Depth to water table (m)
  ! WA:      Water storage in aquifer (mm)
  ! WT:      Water in aquifer and saturated soil (mm)
  ! WSLAKE:  Lake water storage (can be negative) (mm) [?]
  ! LFMASS:  Leaf mass  (g m{-2}) ! Used only for DVEG=2
  ! RTMASS:  Mass of fine roots (g m{-2})
  ! STMASS:  Stem mass (g m{-2})
  ! WOOD:    Mass of wood (including woody roots) (g m{-2})
  ! STBLCP:  Stable carbon in deep soil (g m{-2})
  ! FASTCP:  Short-lived carbon in shallow soil (g m{-2})
  ! LAI:     Leave area index
  ! SAI:     Stem area index
  ! ALBOLD:  Snow albedo at previous time step (CLASS type) [ What is the "CLASS type" of which you speak? ]
  ! CM:      Momentum drag coefficient
  ! CH:      Sensible heat exchange coefficient
  ! CHB:     Sensible heat exchange coefficient over bare-ground fraction
  ! CHV:     Sensible heat exchange coeffieient over vegetated fraction

  call open_forcing_file(iunit, output_dir, forcing_filename, infotext, nsoil, startdate, enddate, loop_for_a_while, & 
       latitude, longitude, &
       forcing_timestep, noahlsm_timestep, ice, t1, stc_ptr, smc_ptr, sh2o_ptr, sldpth, cmc, snowh, sneqv, tbot,        &
       dveg, opt_crs, opt_btr, opt_run, opt_sfc, &
       opt_frz, opt_inf, opt_rad, opt_alb, opt_snf, opt_tbot, opt_stc, &
       vegtype, soiltype, slopetype, snoalb, zlvl, zlvl_wind, albedo_monthly, shdfac_monthly,                  &
       z0brd_monthly, lai_monthly, use_urban_module, isurban, usemonalb, rdlai2d, llanduse)

  if (opt_sfc == 3) then
     stop "(OPT_SFC == 3) and (OPT_SFC == 4) are not for offline use"
  endif

  call soil_veg_gen_parm( LLANDUSE, LSOIL )

  call read_mp_veg_parameters ( LLANDUSE )

  ist     = 1  ! Surface type:  IST=1 => soil;  IST=2 => lake
  isc     = 4  ! Soil color type
  ice     = 0  ! Surface type:  ICE=0 => soil;  ICE=1 => sea-ice

#ifdef _OUT_
  albold = albtbl(vegtype)
  z0     = z0tbl (vegtype)
#else
  albold = 0.19
#endif

  ! Results are not sensitive to the LAI initialization, because 
  ! LAI gets updated in SFLX depending on RTMASS.  Does this depend 
  ! on the Noah-MP options chosen?
#ifdef _OUT_
  lai    = laitbl(vegtype)
#endif
  lai    = 0.5 ! Try this instead

  allocate(zsoil(nsoil))

  ! ZSOIL is negative.
  zsoil(1) = -sldpth(1)
  do n = 2, nsoil
     zsoil(n) = zsoil(n-1) - sldpth(n)
  enddo

  dt = noahlsm_timestep

  nsnow   = 3
  ix      = 1  ! Unused, but probably handy for output and debugging.
  iy      = 1  ! Unused, but probably handy for output and debugging.
  ipoint  = 1  ! Unused, but probably handy for output and debugging.

  allocate(ficeold(-nsnow+1:0))
  allocate(zsnso(-nsnow+1:nsoil))
  allocate(snice(-nsnow+1:0))
  allocate(snliq(-nsnow+1:0))
  allocate(stc(-nsnow+1:nsoil))
  allocate(sh2o(1:nsoil))
  allocate(smc(1:nsoil))
  allocate(tsno(-nsnow+1:0))

  tauss   = 0.0    ! Initialize with new snow.
  qsnow   = 0.0    ! Initialization value from NOAH-MP-WRF
  sneqvo  = 0.0    ! Initialization value from NOAH-MP-WRF
  fwet    = 0.00   ! Initialization value from NOAH-MP-WRF
  foln    = 1.0    ! Initialization value from NOAH-MP-WRF
  wa      = 4900.0 ! Initialization value from NOAH-MP-WRF
  wt      = wa     ! Initialization value from NOAH-MP-WRF
  zwt     = (25.0 + 2.0) - wa/1000.0/0.2 ! Initialization value from NOAH-MP-WRF
  wslake  = 0.0    ! Initialization value from NOAH-MP-WRF
  rtmass  = 500.0  ! Initialization value from NOAH-MP-WRF
  wood    = 500.0  ! Initialization value from NOAH-MP-WRF
  stblcp  = 1000.0 ! Initialization value from NOAH-MP-WRF
  fastcp  = 1000.0 ! Initialization value from NOAH-MP-WRF
  sai     = 0.1    ! Initialization value from NOAH-MP-WRF

  cm      = 0.00 ! Initialization value from NOAH-MP-WRF
  ch      = 0.00 ! Initialization value from NOAH-MP-WRF

  ! LFMASS = 9.0 seems to be a good initial value for the Bondville 1998 case.
  ! it makes LAI match the annual cycle pretty well.
  lfmass = 9.0 
  ! STMASS = 3.33 seems to be a good initial value for the Bondville 1998 case.
  ! it makes STMASS match the annual cycle pretty well.
  stmass = 3.33 

  nowdate = startdate
  lat     = latitude * (3.1415926535/180.0)

  ! Initial values:
  ktime = 0
  stc(1:4) = stc_ptr

  smc     = smc_ptr
  sh2o = smc

  tv      = t1  ! Initialized with skin temperature as in NOAH-MP-WRF
  tg      = t1  ! Initialized with skin temperature as in NOAH-MP-WRF

  canliq  = cmc ! Initialized with the canopy water content as in NOAH-MP-WRF
  canice  = 0.0 ! Initialization value from NOAH-MP-WRF

  ! intent (in)  :: NSNOW, NSOIL, ZSOIL, SNEQV, TG, SNOWH
  ! intent (out) :: ZSNSO, TSNO, SNICE, SNLIQ, ISNOW
  call snow_init ( 1, 1, 1, 1, 1, 1, 1, 1, NSNOW, NSOIL, ZSOIL,  &
       sneqv, tg, snowh, zsnso, tsno, snice, snliq, isnow)


  TIMELOOP : do

     call geth_newdate(nowdate, startdate, ktime*(noahlsm_timestep/60))
     ktime = ktime + 1

     if ((loop_for_a_while > 0) .and. (nowdate == enddate)) then
        write(*,'(I6, ": Nowdate: ", A, "  Switching to startdate: ", A)') reloop_count, nowdate, startdate

        if (reloop_count >= loop_for_a_while ) exit TIMELOOP

        call output_close()

        nowdate = startdate
        reloop_count = reloop_count + 1
        ktime = 1

        call read_forcing_text(iunit, nowdate, forcing_timestep, &
             sfcspd, sfcu, sfcv, sfctmp, q2, sfcprs, soldn, longwave, prcp, ierr)

        if (ierr == 0) stop "Wrong input for looping a year."

        rewind(iunit)

     endif

     call read_forcing_text(iunit, nowdate, forcing_timestep, &
          sfcspd, sfcu, sfcv, sfctmp, q2, sfcprs, soldn, longwave, prcp, ierr)

     if (ierr /= 0) then
        exit TIMELOOP
        stop ":  FORCING DATA READ PROBLEM"
     endif

     if (ktime == 1 .and. reloop_count == 0) then
        tah     = sfctmp              ! Temperature of the canopy; NOAH-MP-WRF initializes this with 287.  ????
        eah = (sfcprs*q2)/(0.622+q2)  ! Water Vapor Pressure of the canopy; NOAH-MP-WRF initializes this with 2000. ????
     endif

     call calc_declin(nowdate(1:4)//"-"//nowdate(5:6)//"-"//nowdate(7:8)//"_"//nowdate(9:10)//":"//nowdate(11:12)//":00", &
          latitude, longitude, cosz, yearlen, julian)

     ! CALL SFCDIF_off (ZLVL_WIND,Z0,T1V,TH2V,SFCSPD,CZIL,CM,CH) ! Out:  CM, CH

     ! All arguments to REDPRM are intent(in).  REDPRM sets many module variables:
     !    NROOT, RSMIN, RGL, RSMAX, QUARTZ, SMCWLT, DWSAT, DKSAT, PSISAT, SMCREF,
     !    SMCMAX, F1, SMCDRY, BEXP, REFKDT, REFDK, CSOIL, ZBOT, CZIL, FRZK, FRZX,
     !    TOPT, KDT, HS
     call REDPRM (VEGTYPE, SOILTYPE, SLOPETYPE, SLDPTH, ZSOIL, NSOIL, ISURBAN)

     uu = sfcu
     vv = sfcv

     lwdn = longwave ! The SFLX routine takes into account emissivity

     co2air  = 355.E-6 * SFCPRS ! Partial pressure of CO2 (Pa) ! From NOAH-MP-WRF
     o2air   = 0.209   * SFCPRS ! Partial pressure of O2 (Pa)  ! From NOAH-MP-WRF
     ficeold(isnow+1:0) = snice(isnow+1:0) / ( snice(isnow+1:0)+snliq(isnow+1:0) ) ! Ice fraction at the last timestep

     stc(isnow+1:0) = tsno(isnow+1:0) ! fill snow levels of STC array with TSNO

     if ( DVEG == 1 ) then
        ! With DVEG==1, SHDFAC is fed directly to FVEG
        shdfac = month_d(shdfac_monthly, nowdate)
     else
        ! With DVEG==2, FVEG is computed from LAI and SAI, and SHDFAC is unused
        shdfac = -1.E38
     endif
     shdmax = maxval(shdfac_monthly)

     DZ8W = -1.E36 ! Not used
     QSFC = Q2 !?
     PSFC = SFCPRS !?

! #define _DEBUG_
#ifdef _DEBUG_
     write(*,'("Before call to NOAHMP_SFLX")')
     write(*,'(10x, "ICE        = ",  I10   )') ICE
     write(*,'(10x, "IST        = ",  I10   )') IST
     write(*,'(10x, "VEGTYPE    = ",  I10   )') VEGTYPE
     write(*,'(10x, "ISC        = ",  I10   )') ISC
     write(*,'(10x, "NSOIL      = ",  I10   )') NSOIL
     write(*,'(10x, "ZSOIL      = ", 7F20.10)') ZSOIL
     write(*,'(10x, "DT         = ",  F20.10)') DT
     write(*,'(10x, "Q2         = ",  F20.10)') Q2
     write(*,'(10x, "SFCTMP     = ",  F20.10)') SFCTMP
     write(*,'(10x, "UU         = ",  F20.10)') UU
     write(*,'(10x, "VV         = ",  F20.10)') VV
     write(*,'(10x, "SOLDN      = ",  F20.10)') SOLDN
     write(*,'(10x, "LWDN       = ",  F20.10)') LWDN
     write(*,'(10x, "PRCP       = ",  F20.10)') PRCP
     write(*,'(10x, "ZLVL       = ",  F20.10)') ZLVL
     write(*,'(10x, "CO2AIR     = ",  F20.10)') CO2AIR
     write(*,'(10x, "O2AIR      = ",  F20.10)') O2AIR
     write(*,'(10x, "COSZ       = ",  F20.10)') COSZ
     write(*,'(10x, "TBOT       = ",  F20.10)') TBOT
     write(*,'(10x, "FOLN       = ",  F20.10)') FOLN
     write(*,'(10x, "SFCPRS     = ",  F20.10)') SFCPRS
     write(*,'(10x, "SHDFAC     = ",  F20.10)') SHDFAC
     write(*,'(10x, "LAT        = ",  F20.10)') LAT
     write(*,'(10x, "DZ8W       = ",  F20.10)') DZ8W
     write(*,'(10x, "EAH        = ",  F20.10)') EAH
     write(*,'(10x, "TAH        = ",  F20.10)') TAH
     write(*,'(10x, "FWET       = ",  F20.10)') FWET
     write(*,'(10x, "FICEOLD    = ", 7F20.10)') FICEOLD
     write(*,'(10x, "QSNOW      = ",  F20.10)') QSNOW
     write(*,'(10x, "SNEQVO     = ",  F20.10)') SNEQVO
     write(*,'(10x, "ISNOW      = ",  I10   )') ISNOW
     write(*,'(10x, "ZSNSO      = ", 7F20.10)') ZSNSO
     write(*,'(10x, "CANLIQ     = ",  F20.10)') CANLIQ
     write(*,'(10x, "CANICE     = ",  F20.10)') CANICE
     write(*,'(10x, "SNOWH      = ",  F20.10)') SNOWH
     write(*,'(10x, "SNEQV      = ",  F20.10)') SNEQV
     write(*,'(10x, "SNICE      = ", 7F20.10)') SNICE
     write(*,'(10x, "SNLIQ      = ", 7F20.10)') SNLIQ
     write(*,'(10x, "TV         = ",  F20.10)') TV
     write(*,'(10x, "TG         = ",  F20.10)') TG
     write(*,'(10x, "STC        = ", 7F20.10)') STC
     write(*,'(10x, "SH2O       = ", 7F20.10)') SH2O
     write(*,'(10x, "SMC        = ", 7F20.10)') SMC
     write(*,'(10x, "ZWT        = ",  F20.10)') ZWT
     write(*,'(10x, "WA         = ",  F20.10)') WA
     write(*,'(10x, "WT         = ",  F20.10)') WT
     write(*,'(10x, "WSLAKE     = ",  F20.10)') WSLAKE
     write(*,'(10x, "LFMASS     = ",  F20.10)') LFMASS
     write(*,'(10x, "RTMASS     = ",  F20.10)') RTMASS
     write(*,'(10x, "STMASS     = ",  F20.10)') STMASS
     write(*,'(10x, "WOOD       = ",  F20.10)') WOOD
     write(*,'(10x, "STBLCP     = ",  F20.10)') STBLCP
     write(*,'(10x, "FASTCP     = ",  F20.10)') FASTCP
     write(*,'(10x, "LAI        = ",  F20.10)') LAI
     write(*,'(10x, "SAI        = ",  F20.10)') SAI
     write(*,'(10x, "ALBOLD     = ",  F20.10)') ALBOLD
     write(*,'(10x, "CM         = ",  F20.10)') CM
     write(*,'(10x, "CH         = ",  F20.10)') CH
     write(*,'(10x, "DX         = ",  F20.10)') DX
     write(*,'(10x, "ISURBAN    = ",  I10   )') ISURBAN
     write(*,'(10x, "IZ0TLND    = ",  I10   )') IZ0TLND
     write(*,'(10x, "QC         = ",  F20.10)') QC
     write(*,'(10x, "PBLH       = ",  F20.10)') PBLH
     write(*,'(10x, "QSFC       = ",  F20.10)') QSFC
     write(*,'(10x, "PSFC       = ",  F20.10)') PSFC
#endif

     call noahmp_sflx(&
       &           IX      , IY      , LAT     , YEARLEN , JULIAN  , COSZ    , & ! IN : Time/Space-related
       &           DT      , DX      , DZ8W    , NSOIL   , ZSOIL   , NSNOW   , & ! IN : Model configuration 
       &           SHDFAC  , SHDMAX  , VEGTYPE , ISURBAN , ICE     , IST     , & ! IN : Vegetation/Soil characteristics
       &           ISC     ,                                                   & ! IN : Vegetation/Soil characteristics
       &           IZ0TLND ,                                                   & ! IN : User options
       &           SFCTMP  , SFCPRS  , PSFC    , UU      , VV      , Q2      , & ! IN : Forcing
       &           QC      , SOLDN   , LWDN    , PRCP    , TBOT    , CO2AIR  , & ! IN : Forcing
       &           O2AIR   , FOLN    , FICEOLD , PBLH    ,                     & ! IN : Forcing
       &           ZLVL    , ALBOLD  , SNEQVO  ,                               & ! IN/OUT : 
       &           STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
       &           CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & ! IN/OUT : 
       &           ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & ! IN/OUT : 
       &           ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
       &           STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & ! IN/OUT : 
       &           CM      , CH      , CHSTAR  , TAUSS   ,                     & ! IN/OUT : 
       &           FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
       &           FGEV    , FCTR    , ECAN    , ETRAN   , EDIR    , TRAD    , & ! OUT :
       &           TS      , TGB     , TGV     , T2MV    , T2MB    , TSTAR   , & ! OUT :
       &           Q1      , Q2V     , Q2B     , RUNSRF  , RUNSUB  , APAR    , & ! OUT :
       &           PSN     , SAV     , SAG     , FSNO    , NEE     , GPP     , & ! OUT :
       &           NPP     , FVEG    , ALBEDO  , QMELT   , PONDING , PONDING1, & ! OUT :
       &           PONDING2, RSSUN   , RSSHA   , BGAP    , WGAP    , GAP     , & ! OUT :
       &           ERRWAT  , CHV     , CHB     , EMISSI )                        ! OUT :


#ifdef _DEBUG_
     write(*,'("After  call to NOAHMP_SFLX")')
     write(*,'(10x, "ZLVL       = ", 7F20.10)') ZLVL
     write(*,'(10x, "EAH        = ",  F20.10)') EAH
     write(*,'(10x, "TAH        = ",  F20.10)') TAH
     write(*,'(10x, "FWET       = ",  F20.10)') FWET
     write(*,'(10x, "QSNOW      = ",  F20.10)') QSNOW
     write(*,'(10x, "SNEQVO     = ",  F20.10)') SNEQVO
     write(*,'(10x, "ISNOW      = ",  I10   )') ISNOW
     write(*,'(10x, "ZSNSO      = ", 7F20.10)') ZSNSO
     write(*,'(10x, "CANLIQ     = ",  F20.10)') CANLIQ
     write(*,'(10x, "CANICE     = ",  F20.10)') CANICE
     write(*,'(10x, "SNOWH      = ",  F20.10)') SNOWH
     write(*,'(10x, "SNEQV      = ",  F20.10)') SNEQV
     write(*,'(10x, "SNICE      = ", 3F20.10)') SNICE
     write(*,'(10x, "SNLIQ      = ", 3F20.10)') SNLIQ
     write(*,'(10x, "TV         = ",  F20.10)') TV
     write(*,'(10x, "TG         = ",  F20.10)') TG
     write(*,'(10x, "STC        = ", 7F20.10)') STC
     write(*,'(10x, "SH2O       = ", 7F20.10)') SH2O
     write(*,'(10x, "SMC        = ", 7F20.10)') SMC
     write(*,'(10x, "ZWT        = ",  F20.10)') ZWT
     write(*,'(10x, "WA         = ",  F20.10)') WA
     write(*,'(10x, "WT         = ",  F20.10)') WT
     write(*,'(10x, "WSLAKE     = ",  F20.10)') WSLAKE
     write(*,'(10x, "LFMASS     = ",  F20.10)') LFMASS
     write(*,'(10x, "RTMASS     = ",  F20.10)') RTMASS
     write(*,'(10x, "STMASS     = ",  F20.10)') STMASS
     write(*,'(10x, "WOOD       = ",  F20.10)') WOOD
     write(*,'(10x, "STBLCP     = ",  F20.10)') STBLCP
     write(*,'(10x, "FASTCP     = ",  F20.10)') FASTCP
     write(*,'(10x, "LAI        = ",  F20.10)') LAI
     write(*,'(10x, "SAI        = ",  F20.10)') SAI
     write(*,'(10x, "ALBOLD     = ",  F20.10)') ALBOLD
     write(*,'(10x, "CM         = ",  F20.10)') CM
     write(*,'(10x, "CH         = ",  F20.10)') CH
     write(*,'(10x, "FSA        = ",  F20.10)') FSA
     write(*,'(10x, "FSR        = ",  F20.10)') FSR
     write(*,'(10x, "FIRA       = ",  F20.10)') FIRA
     write(*,'(10x, "FSH        = ",  F20.10)') FSH
     write(*,'(10x, "SSOIL      = ",  F20.10)') SSOIL
     write(*,'(10x, "FCEV       = ",  F20.10)') FCEV
     write(*,'(10x, "FGEV       = ",  F20.10)') FGEV
     write(*,'(10x, "FCTR       = ",  F20.10)') FCTR
     write(*,'(10x, "TRAD       = ",  F20.10)') TRAD
     write(*,'(10x, "ECAN       = ",  F20.10)') ECAN
     write(*,'(10x, "ETRAN      = ",  F20.10)') ETRAN
     write(*,'(10x, "EDIR       = ",  F20.10)') EDIR
     write(*,'(10x, "RUNSRF     = ",  F20.10)') RUNSRF
     write(*,'(10x, "RUNSUB     = ",  F20.10)') RUNSUB
     write(*,'(10x, "APAR       = ",  F20.10)') APAR
     write(*,'(10x, "PSN        = ",  F20.10)') PSN
     write(*,'(10x, "SAV        = ",  F20.10)') SAV
     write(*,'(10x, "SAG        = ",  F20.10)') SAG
     write(*,'(10x, "FSNO       = ",  F20.10)') FSNO
     write(*,'(10x, "NEE        = ",  F20.10)') NEE
     write(*,'(10x, "GPP        = ",  F20.10)') GPP
     write(*,'(10x, "NPP        = ",  F20.10)') NPP
     write(*,'(10x, "TS         = ",  F20.10)') TS
     write(*,'(10x, "FVEG       = ",  F20.10)') FVEG
     write(*,'(10x, "ALBEDO     = ",  F20.10)') ALBEDO
     write(*,'(10x, "ERRWAT     = ",  F20.10)') ERRWAT
     write(*,'(10x, "QMELT      = ",  F20.10)') QMELT
     write(*,'(10x, "PONDING    = ",  F20.10)') PONDING
     write(*,'(10x, "PONDING1   = ",  F20.10)') PONDING1
     write(*,'(10x, "PONDING2   = ",  F20.10)') PONDING2
     write(*,'(10x, "QSFC       = ",  F20.10)') QSFC
     write(*,'(10x, "CHSTAR     = ",  F20.10)') CHSTAR
     write(*,'(10x, "CHSTAR2    = ",  F20.10)') CHSTAR2
     write(*,'(10x, "TSTAR      = ",  F20.10)') TSTAR
     write(*,'(10x, "T2MV       = ",  F20.10)') T2MV
     write(*,'(10x, "T2MB       = ",  F20.10)') T2MB
     write(*,'(10x, "RSSUN      = ",  F20.10)') RSSUN
     write(*,'(10x, "RSSHA      = ",  F20.10)') RSSHA
     write(*,'(10x, "BGAP       = ",  F20.10)') BGAP
     write(*,'(10x, "WGAP       = ",  F20.10)') WGAP
     write(*,'(10x, "GAP        = ",  F20.10)') GAP
     write(*,'(10x, "TGV        = ",  F20.10)') TGV
     write(*,'(10x, "TGB        = ",  F20.10)') TGB
     write(*,'(10x, "Q1         = ",  F20.10)') Q1
     write(*,'(10x, "EMISSI     = ",  F20.10)') EMISSI
#endif
     tsno(isnow+1:0)  = stc(isnow+1:0) ! fill TSNO with snow levels of STC array.

     qfx = fgev + fcev + fctr

     ir_sh_ev_gh = FIRA + FSH + QFX + SSOIL

     flxsum = (-FSA) + FIRA + FSH + QFX + SSOIL

     fsa_fsr = FSA + FSR

     !
     ! Write the output data for this timestep.
     !

     if (ktime == 1) then
        if (loop_for_a_while > 0) then
           write(teststr,'(A,"/OUTPUT.",I4.4,".nc")') trim(output_dir), reloop_count
           call initialize_output(trim(teststr), nsoil, nsnow, 0, 0, 0, dt, dveg, opt_crs, opt_btr, &
                opt_run, opt_sfc, opt_frz, opt_inf, opt_rad, opt_alb, opt_snf, opt_tbot, opt_stc)
        else
           call initialize_output(trim(output_dir)//"/OUTPUT.nc", nsoil, nsnow, 0, 0, 0, dt, dveg, opt_crs, opt_btr, &
                opt_run, opt_sfc, opt_frz, opt_inf, opt_rad, opt_alb, opt_snf, opt_tbot, opt_stc)
        endif
     endif

     ! Time variable
     call output_time(ktime, nowdate,   "Times",   "UTC time of data output",                           "YYYYMMDD HH:mm"  )

     ! Multi-layer variables:
     call output_levels(ktime, nsnow, "num_snow_layers", zsnso(-nsnow+1:0), "ZSNSO", "Snow and Soil layers",                       "m"             )
     call output_levels(ktime, nsoil, "num_soil_layers", stc(1:nsoil),      "STC",   "Soil temperature",                           "K"             )
     call output_levels(ktime, nsnow, "num_snow_layers", stc(-nsnow+1:0),   "SNOWT", "Snow temperature",                           "K"             )
     call output_levels(ktime, nsoil, "num_soil_layers", smc(1:nsoil),      "SMC",   "Volumetric Soil Moisture",                   "m{3} m{-3}"    )
     call output_levels(ktime, nsoil, "num_soil_layers", smc(1:nsoil),      "SH2O",  "Volumetric Soil Moisture (liquid)",          "m{3} m{-3}"    )
     call output_levels(ktime, nsnow, "num_snow_layers", snice(-nsnow+1:0), "SNICE", "Snow-layer ice",                             "mm"            )
     call output_levels(ktime, nsnow, "num_snow_layers", snliq(-nsnow+1:0), "SNLIQ", "Snow-layer liquid water",                    "mm"            )

     ! Single-layer variables:
     call output_var(ktime, q2,        "Q2",      "Mixing ratio (forcing)",                            "kg kg{-1}"        )
     call output_var(ktime, sfctmp,    "SFCTMP",  "Air temperature at ZLVL m AGL (forcing)",           "K"                )
     call output_var(ktime, uu,        "UU",      "U-component of wind (forcing)",                     "m s{-1}"          )
     call output_var(ktime, vv,        "VV",      "V-component of wind (forcing)",                     "m s{-1}"          )
     call output_var(ktime, soldn,     "SOLDN",   "Downward solar radiation flux (forcing)",           "W m{-2}"          )
     call output_var(ktime, lwdn,      "LWDN",    "Downward longwave radiation flux (forcing)",        "W m{-2}"          )
     call output_var(ktime, prcp,      "PRCP",    "Precipitation rate (forcing)",                      "kg m{-2} s{-1}"   )
     call output_var(ktime, co2air,    "CO2AIR",  "Atmospheric C02 concentration (forcing)",           "Pa"               )
     call output_var(ktime, o2air,     "O2AIR",   "Atmospheric 02  concentration (forcing)",           "Pa"               )
     call output_var(ktime, foln,      "FOLN",    "Foliage Nitrogen",                                  "fraction"         )
     call output_var(ktime, sfcprs,    "SFCPRS",  "Atmospheric Pressure (forcing)",                    "Pa"               )
     call output_var(ktime, eah,       "EAH",     "Canopy air vapor pressure",                         "Pa"               )
     call output_var(ktime, tah,       "TAH",     "Canopy air temperature",                            "K"                )
     call output_var(ktime, fwet,      "FWET",    "Wetted or snowed fraction of canopy",               "fraction"         )
     call output_var(ktime, qsnow,     "QSNOW",   "Snow accumulation rate",                            "mm s{-1}"         )
     call output_var(ktime, cosz,      "COSZ",    "Cosine of solar zenith angle",                      "dimensionless"    )
     call output_var(ktime, canliq,    "CANLIQ",  "Canopy liquid water content",                       "kg m{-2}"         )
     call output_var(ktime, canice,    "CANICE",  "Canopy ice water content",                          "kg m{-2}"         )
     call output_var(ktime, snowh,     "SNOWH",   "Snow depth",                                        "m"                )
     call output_var(ktime, sneqv,     "SNEQV",   "Snow Water Equivalent",                             "kg m{-2}"         )
     call output_var(ktime, tv,        "TV",      "Vegetation temperature",                            "K"                )
     call output_var(ktime, tg,        "TG",      "Ground temperature",                                "K"                )
     call output_var(ktime, zwt,       "ZWT",     "Depth to water table",                              "m"                )
     call output_var(ktime, wa,        "WA",      "Water storage in aquifer",                          "kg m{-2}"         )
     call output_var(ktime, wt,        "WT",      "Water in aquifer and saturated soil",               "kg m{-2}"         )
     call output_var(ktime, wslake,    "WSLAKE",  "Lake water storage",                                "kg m{-2}"         )
     call output_var(ktime, lfmass,    "LFMASS",  "Leaf mass",                                         "g m{-2}"          )
     call output_var(ktime, rtmass,    "RTMASS",  "Mass of fine roots",                                "g m{-2}"          )
     call output_var(ktime, stmass,    "STMASS",  "Stem mass",                                         "g m{-2}"          )
     call output_var(ktime, wood,      "WOOD",    "Mass of wood, including woody roots",               "g m{-2}"          )
     ! call output_var(ktime, stblcp,    "STBLCP",  "Stable carbon in deep soil",                        "g m{-2}"          )
     ! call output_var(ktime, fastcp,    "FASTCP",  "Short-lived carbon in shallow soil",                "g m{-2}"          )
     call output_var(ktime, lai,       "LAI",     "Leaf area index",                                   "-"                )
     call output_var(ktime, sai,       "SAI",     "Stem area index",                                   "-"                )
     call output_var(ktime, cm,        "CM",      "Momentumm drag coefficient",                        "-"  )
     call output_var(ktime, ch,        "CH",      "Sensible heat exchange coefficient",                "-"  )
     call output_var(ktime, fsa,       "FSA",     "Total absorbed solar radiation",                    "W m{-2}"          )
     call output_var(ktime, fsr,       "FSR",     "Total reflected solar radiation",                   "W m{-2}"          )
     call output_var(ktime, fira,      "FIRA",    "Total net longwave radiation to atmosphere",        "W m{-2}"          )
     call output_var(ktime, fsh,       "FSH",     "Total sensible heat to atmosphere",                 "W m{-2}"          )
     call output_var(ktime, ssoil,     "SSOIL",   "Ground heat flux to soil",                          "W m{-2}"          )
     call output_var(ktime, qfx,       "QFX",     "qfx",                                               "-"  )
     call output_var(ktime, fcev,      "FCEV",    "Canopy evaporative heat to atmosphere",             "W m{-2}"          )
     call output_var(ktime, fgev,      "FGEV",    "Ground evaporative heat to atmosphere",             "W m{-2}"          )
     call output_var(ktime, fctr,      "FCTR",    "Transpiration heat to atmosphere",                  "W m{-2}"          )
     call output_var(ktime, trad,      "TRAD",    "Surface radiative temperature",                     "K"                )
     call output_var(ktime, ecan,      "ECAN",    "Evaporation rate of canopy water",                  "kg m{-2} s{-1}"   )
     call output_var(ktime, etran,     "ETRAN",   "Transpiration rate",                                "kg m{-2} s{-1}"   )
     call output_var(ktime, edir,      "EDIR",    "Direct evaporation rate from surface",              "kg m{-2} s{-1}"   )
     call output_var(ktime, runsrf,    "RUNSRF",  "Surface runoff",                                    "kg m{-2} s{-1}"   )
     call output_var(ktime, runsub,    "RUNSUB",  "Baseflow (saturation excess)",                      "kg m{-2} s{-1}"   )
     call output_var(ktime, apar,      "APAR",    "Photosynthesis active energy by canopy",            "W m{-2}"          )
     call output_var(ktime, psn,       "PSN",     "Total photosynthesis of CO2",                       "umol m{-2} s{-1}" )
     call output_var(ktime, sav,       "SAV",     "Solar radiation absorbed by vegetation",            "W m{-2}"          )
     call output_var(ktime, sag,       "SAG",     "Solar radiation absorbed by ground",                "W m{-2}"          )
     call output_var(ktime, fsno,      "FSNO",    "Snow-cover fraction on the ground",                 "-"                )
     ! call output_var(ktime, nee,       "NEE",     "Net ecosystem exchange of CO2",                     "g m{-2} s{-1}"    )
     call output_var(ktime, gpp,       "GPP",     "Net instantaneous assimilation of Carbon",          "g m{-2} s{-1}"    )
     call output_var(ktime, npp,       "NPP",     "Net primary productivity of Carbon",                "g m{-2} s{-1}"    )
     call output_var(ktime, ts,        "TS",      "Surface temperature",                               "K"                )
     call output_var(ktime, fveg,      "FVEG",    "Green vegetation fraction",                         "-"                )
     call output_var(ktime, albedo,    "ALBEDO",  "Surface albedo",                                    "-"                )
     call output_var(ktime, flxsum,    "FLXSUM",  "Flux sum",                                          "W m{-2}"          )
     call output_var(ktime, tauss,     "TAUSS",   "Snow aging term",                                   "-"                )
     ! call output_var(ktime, ir_sh_ev_gh, "IR+SH+EV+GH", "IR+SH+EV+GH",                                 "W m{-2}"          )
     ! call output_var(ktime, fsa_fsr,   "FSA+FSR", "Solar: Total Absorbed + Total Reflected",           "W m{-2}"          )

     ! Close out the output for time KTIME
     call finish_output_for_time(ktime)

     ! write(*,'("STC = ", 8F10.4)') STC

  enddo TIMELOOP

  call output_close()


end program driver

!-----------------------------------------------------------------
SUBROUTINE SOIL_VEG_GEN_PARM( MMINLU, MMINSL)
!-----------------------------------------------------------------
  use module_sf_noahlsm, only : shdtbl, nrotbl, rstbl, rgltbl, &
       &                        hstbl, snuptbl, maxalb, laimintbl, &
       &                        bb, drysmc, f11, maxsmc, laimaxtbl, &
       &                        emissmintbl, emissmaxtbl, albedomintbl, &
       &                        albedomaxtbl, wltsmc, qtz, refsmc, &
       &                        z0mintbl, z0maxtbl, &
       &                        satpsi, satdk, satdw, &
       &                        fxexp_data, lvcoef_data, &
       &                        lutype, maxalb, &
       &                        slope_data, frzk_data, bare, cmcmax_data, &
       &                        cfactr_data, csoil_data, czil_data, &
       &                        refkdt_data, natural, refdk_data, &
       &                        rsmax_data, salp_data, sbeta_data, &
       &                        zbot_data, smhigh_data, smlow_data, &
       &                        lucats, topt_data, slcats, slpcats, sltype

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: MMINLU, MMINSL
  integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
  integer :: ierr
  INTEGER , PARAMETER :: OPEN_OK = 0

  character*128 :: mess , message

!-----SPECIFY VEGETATION RELATED CHARACTERISTICS :
!             ALBBCK: SFC albedo (in percentage)
!                 Z0: Roughness length (m)
!             SHDFAC: Green vegetation fraction (in percentage)
!  Note: The ALBEDO, Z0, and SHDFAC values read from the following table
!          ALBEDO, amd Z0 are specified in LAND-USE TABLE; and SHDFAC is
!          the monthly green vegetation data
!             CMXTBL: MAX CNPY Capacity (m)
!             NROTBL: Rooting depth (layer)
!              RSMIN: Mimimum stomatal resistance (s m-1)
!              RSMAX: Max. stomatal resistance (s m-1)
!                RGL: Parameters used in radiation stress function
!                 HS: Parameter used in vapor pressure deficit functio
!               TOPT: Optimum transpiration air temperature. (K)
!             CMCMAX: Maximum canopy water capacity
!             CFACTR: Parameter used in the canopy inteception calculati
!               SNUP: Threshold snow depth (in water equivalent m) that
!                     implies 100% snow cover
!                LAI: Leaf area index (dimensionless)
!             MAXALB: Upper bound on maximum albedo over deep snow
!
!-----READ IN VEGETAION PROPERTIES FROM VEGPARM.TBL
!

  OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
     WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
     CALL wrf_error_fatal ( message )
  END IF


  LUMATCH=0

  FIND_LUTYPE : DO WHILE (LUMATCH == 0)
     READ (19,*,END=2002)
     READ (19,*,END=2002)LUTYPE
     READ (19,*)LUCATS,IINDEX

     IF(LUTYPE.EQ.MMINLU)THEN
        WRITE( mess , * ) 'LANDUSE TYPE = ' // TRIM ( LUTYPE ) // ' FOUND', LUCATS,' CATEGORIES'
        ! CALL wrf_message( mess )
        LUMATCH=1
     ELSE
        call wrf_message ( "Skipping over LUTYPE = " // TRIM ( LUTYPE ) )
        DO LC = 1, LUCATS+12
           read(19,*)
        ENDDO
     ENDIF
  ENDDO FIND_LUTYPE
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(SHDTBL)       < LUCATS .OR. &
       SIZE(NROTBL)       < LUCATS .OR. &
       SIZE(RSTBL)        < LUCATS .OR. &
       SIZE(RGLTBL)       < LUCATS .OR. &
       SIZE(HSTBL)        < LUCATS .OR. &
       SIZE(SNUPTBL)      < LUCATS .OR. &
       SIZE(MAXALB)       < LUCATS .OR. &
       SIZE(LAIMINTBL)    < LUCATS .OR. &
       SIZE(LAIMAXTBL)    < LUCATS .OR. &
       SIZE(Z0MINTBL)     < LUCATS .OR. &
       SIZE(Z0MAXTBL)     < LUCATS .OR. &
       SIZE(ALBEDOMINTBL) < LUCATS .OR. &
       SIZE(ALBEDOMAXTBL) < LUCATS .OR. &
       SIZE(EMISSMINTBL ) < LUCATS .OR. &
       SIZE(EMISSMAXTBL ) < LUCATS ) THEN
     CALL wrf_error_fatal('Table sizes too small for value of LUCATS in module_sf_noahdrv.F')
  ENDIF

  IF(LUTYPE.EQ.MMINLU)THEN
     DO LC=1,LUCATS
        READ (19,*)IINDEX,SHDTBL(LC),                        &
             NROTBL(LC),RSTBL(LC),RGLTBL(LC),HSTBL(LC), &
             SNUPTBL(LC),MAXALB(LC), LAIMINTBL(LC),     &
             LAIMAXTBL(LC),EMISSMINTBL(LC),             &
             EMISSMAXTBL(LC), ALBEDOMINTBL(LC),         &
             ALBEDOMAXTBL(LC), Z0MINTBL(LC), Z0MAXTBL(LC)
     ENDDO
!
     READ (19,*)
     READ (19,*)TOPT_DATA
     READ (19,*)
     READ (19,*)CMCMAX_DATA
     READ (19,*)
     READ (19,*)CFACTR_DATA
     READ (19,*)
     READ (19,*)RSMAX_DATA
     READ (19,*)
     READ (19,*)BARE
     READ (19,*)
     READ (19,*)NATURAL
  ENDIF
!
2002 CONTINUE

  CLOSE (19)
  IF (LUMATCH == 0) then
     CALL wrf_error_fatal ("Land Use Dataset '"//MMINLU//"' not found in VEGPARM.TBL.")
  ENDIF

!
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
  OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
     WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
     CALL wrf_error_fatal ( message )
  END IF

  WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICATION = ', TRIM ( MMINSL )
  ! CALL wrf_message( mess )

  LUMATCH=0

  READ (19,*)
  READ (19,2000,END=2003)SLTYPE
2000 FORMAT (A4)
  READ (19,*)SLCATS,IINDEX
  IF(SLTYPE.EQ.MMINSL)THEN
     WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ', TRIM ( SLTYPE ) , ' FOUND', &
          SLCATS,' CATEGORIES'
     ! CALL wrf_message ( mess )
     LUMATCH=1
  ENDIF
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(BB    ) < SLCATS .OR. &
       SIZE(DRYSMC) < SLCATS .OR. &
       SIZE(F11   ) < SLCATS .OR. &
       SIZE(MAXSMC) < SLCATS .OR. &
       SIZE(REFSMC) < SLCATS .OR. &
       SIZE(SATPSI) < SLCATS .OR. &
       SIZE(SATDK ) < SLCATS .OR. &
       SIZE(SATDW ) < SLCATS .OR. &
       SIZE(WLTSMC) < SLCATS .OR. &
       SIZE(QTZ   ) < SLCATS  ) THEN
     CALL wrf_error_fatal('Table sizes too small for value of SLCATS in module_sf_noahdrv.F')
  ENDIF
  IF(SLTYPE.EQ.MMINSL)THEN
     DO LC=1,SLCATS
        READ (19,*) IINDEX,BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
             REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
             WLTSMC(LC), QTZ(LC)
     ENDDO
  ENDIF

2003 CONTINUE

  CLOSE (19)

  IF(LUMATCH.EQ.0)THEN
     CALL wrf_message( 'SOIl TEXTURE IN INPUT FILE DOES NOT ' )
     CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
     CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
  ENDIF

!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
  OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
     WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
     CALL wrf_error_fatal ( message )
  END IF

  READ (19,*)
  READ (19,*)
  READ (19,*) NUM_SLOPE

  SLPCATS=NUM_SLOPE
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(slope_data) < NUM_SLOPE ) THEN
     CALL wrf_error_fatal('NUM_SLOPE too large for slope_data array in module_sf_noahdrv')
  ENDIF

  DO LC=1,SLPCATS
     READ (19,*)SLOPE_DATA(LC)
  ENDDO

  READ (19,*)
  READ (19,*)SBETA_DATA
  READ (19,*)
  READ (19,*)FXEXP_DATA
  READ (19,*)
  READ (19,*)CSOIL_DATA
  READ (19,*)
  READ (19,*)SALP_DATA
  READ (19,*)
  READ (19,*)REFDK_DATA
  READ (19,*)
  READ (19,*)REFKDT_DATA
  READ (19,*)
  READ (19,*)FRZK_DATA
  READ (19,*)
  READ (19,*)ZBOT_DATA
  READ (19,*)
  READ (19,*)CZIL_DATA
  READ (19,*)
  READ (19,*)SMLOW_DATA
  READ (19,*)
  READ (19,*)SMHIGH_DATA
  READ (19,*)
  READ (19,*)LVCOEF_DATA
  CLOSE (19)

!-----------------------------------------------------------------
END SUBROUTINE SOIL_VEG_GEN_PARM
!-----------------------------------------------------------------
SUBROUTINE calc_declin ( nowdate, latitude, longitude, cosz, yearlen, julian)
  use kwm_date_utilities
!---------------------------------------------------------------------
  IMPLICIT NONE
!---------------------------------------------------------------------

  REAL, PARAMETER :: DEGRAD = 3.14159265/180.
  REAL, PARAMETER :: DPD    = 360./365.
! !ARGUMENTS:
  character(len=19), intent(in)  :: nowdate    ! YYYY-MM-DD_HH:mm:ss
  real,              intent(in)  :: latitude
  real,              intent(in)  :: longitude
  real,              intent(out) :: cosz
  integer,           intent(out) :: yearlen
  real,              intent(out) :: JULIAN

  REAL                           :: hrang
  real                           :: DECLIN
  real                           :: tloctim
  REAL                           :: OBECL
  REAL                           :: SINOB
  REAL                           :: SXLONG
  REAL                           :: ARG
  integer                        :: iyear
  integer                        :: iday
  integer                        :: ihour
  integer                        :: iminute
  integer                        :: isecond

  !
  ! Determine the number of days in the year
  !

  read(nowdate(1:4), '(I4)') iyear
  yearlen = 365
  if (mod(iyear,4) == 0) then
     yearlen = 366
     if (mod(iyear,100) == 0) then
        yearlen = 365
        if (mod(iyear,400) == 0) then
           yearlen = 366
           if (mod(iyear,3600) == 0) then
              yearlen = 365
           endif
        endif
     endif
  endif

  !
  ! Determine the Julian time (floating-point day of year).
  !

  call geth_idts(nowdate(1:10), nowdate(1:4)//"-01-01", iday)
  read(nowdate(12:13), *) ihour
  read(nowdate(15:16), *) iminute
  read(nowdate(18:19), *) isecond
  julian = real(iday) + real(ihour)/24.

!
! for short wave radiation

  DECLIN=0.

!-----OBECL : OBLIQUITY = 23.5 DEGREE.

  OBECL=23.5*DEGRAD
  SINOB=SIN(OBECL)

!-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:

  IF(JULIAN.GE.80.)SXLONG=DPD*(JULIAN-80.)*DEGRAD
  IF(JULIAN.LT.80.)SXLONG=DPD*(JULIAN+285.)*DEGRAD
  ARG=SINOB*SIN(SXLONG)
  DECLIN=ASIN(ARG)

  TLOCTIM = REAL(IHOUR) + REAL(IMINUTE)/60.0 + REAL(ISECOND)/3600.0 + LONGITUDE/15.0 ! Local time in hours
  tloctim = AMOD(tloctim+24.0, 24.0)
  HRANG=15.*(TLOCTIM-12.)*DEGRAD
  COSZ=SIN(LATITUDE*DEGRAD)*SIN(DECLIN)+COS(LATITUDE*DEGRAD)*COS(DECLIN)*COS(HRANG)

!KWM   write(wrf_err_message,10)DECDEG/DEGRAD
!KWM10 FORMAT(1X,'*** SOLAR DECLINATION ANGLE = ',F6.2,' DEGREES.',' ***')
!KWM   CALL wrf_debug (50, wrf_err_message)

END SUBROUTINE calc_declin

! Subroutine SNOW_INIT grabbed from NOAH-MP-WRF
SUBROUTINE SNOW_INIT ( jts, jtf, its, itf, ims, ime, jms, jme, NSNOW, NSOIL, ZSOIL,  &
     SWE, tgxy, SNODEP, ZSNSOXY, TSNOXY, SNICEXY, SNLIQXY, ISNOWXY)

! ------------------------------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------------------------------------------------------------------------
  INTEGER, INTENT(IN) :: jts,jtf,its,itf,ims,ime, jms,jme,NSNOW,NSOIL
  REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SWE 
  REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SNODEP
  REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: tgxy
  REAL,    INTENT(IN), DIMENSION(1:NSOIL) :: ZSOIL

  INTEGER, INTENT(OUT), DIMENSION(ims:ime, jms:jme) :: ISNOWXY
  REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: ZSNSOXY
  REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: TSNOXY
  REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNICEXY
  REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNLIQXY

!local
  INTEGER :: I,J,IZ
  REAL,                 DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: DZSNOXY
  REAL,                 DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: DZSNSOXY
! ------------------------------------------------------------------------------------------


  DO J = jts,jtf
     DO I = its,itf
        IF (SNODEP(I,J) < 0.025) THEN
           ISNOWXY(I,J) = 0
           DZSNOXY(I,-NSNOW+1:0,J) = 0.
        ELSE
           IF ((SNODEP(I,J) >= 0.025) .AND. (SNODEP(I,J) <= 0.05)) THEN
              ISNOWXY(I,J)    = -1
              DZSNOXY(I,0,J)  = SNODEP(I,J)
           ELSE IF ((SNODEP(I,J) > 0.05) .AND. (SNODEP(I,J) <= 0.10)) THEN
              ISNOWXY(I,J)    = -2
              DZSNOXY(I,-1,J) = SNODEP(I,J)/2.
              DZSNOXY(I, 0,J) = SNODEP(I,J)/2.
           ELSE IF ((SNODEP(I,J) > 0.10) .AND. (SNODEP(I,J) <= 0.25)) THEN
              ISNOWXY(I,J)    = -2
              DZSNOXY(I,-1,J) = 0.05
              DZSNOXY(I, 0,J) = SNODEP(I,J) - DZSNOXY(I,-1,J)
           ELSE IF ((SNODEP(I,J) > 0.25) .AND. (SNODEP(I,J) <= 0.35)) THEN
              ISNOWXY(I,J)    = -3
              DZSNOXY(I,-2,J) = 0.05
              DZSNOXY(I,-1,J) = 0.5*(SNODEP(I,J)-DZSNOXY(I,-2,J))
              DZSNOXY(I, 0,J) = 0.5*(SNODEP(I,J)-DZSNOXY(I,-2,J))
           ELSE IF (SNODEP(I,J) > 0.35) THEN
              ISNOWXY(I,J)     = -3
              DZSNOXY(I,-2,J) = 0.05
              DZSNOXY(I,-1,J) = 0.10
              DZSNOXY(I, 0,J) = SNODEP(I,J) - DZSNOXY(I,-1,J) - DZSNOXY(I,-2,J)
           END IF
        END IF
     ENDDO
  ENDDO

  DO J = jts,jtf
     DO I = its,itf
        TSNOXY( I,-NSNOW+1:0,J) = 0.
        SNICEXY(I,-NSNOW+1:0,J) = 0.
        SNLIQXY(I,-NSNOW+1:0,J) = 0.
        DO IZ = ISNOWXY(I,J)+1, 0
           TSNOXY(I,IZ,J)  = tgxy(I,J)  ! [k]
           SNLIQXY(I,IZ,J) = 0.00
           SNICEXY(I,IZ,J) = 1.00 * DZSNOXY(I,IZ,J) * (SWE(I,J)/SNODEP(I,J))  ! [kg/m3]
        END DO

        DO IZ = ISNOWXY(I,J)+1, 0
           DZSNSOXY(I,IZ,J) = -DZSNOXY(I,IZ,J)
        END DO

        DZSNSOXY(I,1,J) = ZSOIL(1)
        DO IZ = 2,NSOIL
           DZSNSOXY(I,IZ,J) = (ZSOIL(IZ) - ZSOIL(IZ-1))
        END DO

        ZSNSOXY(I,ISNOWXY(I,J)+1,J) = DZSNSOXY(I,ISNOWXY(I,J)+1,J)
        DO IZ = ISNOWXY(I,J)+2 ,NSOIL
           ZSNSOXY(I,IZ,J) = ZSNSOXY(I,IZ-1,J) + DZSNSOXY(I,IZ,J)
        ENDDO

     END DO
  END DO

END SUBROUTINE SNOW_INIT

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

real function month_d(a12, nowdate) result (nowval)
  !
  ! Given a set of 12 values, taken to be valid on the fifteenth of each month (Jan through Dec)
  ! and a date in the form <YYYYMMDD[HHmmss]> ....
  ! 
  ! Return a value valid for the day given in <nowdate>, as an interpolation from the 12
  ! monthly values.
  !
  use kwm_date_utilities
  implicit none
  real, dimension(12), intent(in) :: a12 ! 12 monthly values, taken to be valid on the 15th of
  !                                      ! the month
  character(len=12), intent(in) :: nowdate ! Date, in the form <YYYYMMDD[HHmmss]>
  integer :: nowy, nowm, nowd
  integer :: prevm, postm
  real    :: factor
  integer, dimension(12) :: ndays = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !
  ! Handle leap year by setting the number of days in February for the year in question.
  !
  read(nowdate(1:8),'(I4,I2,I2)') nowy, nowm, nowd
  ndays(2) = nfeb(nowy)

  !
  ! Do interpolation between the fifteenth of two successive months.
  !
  if (nowd == 15) then
     nowval = a12(nowm)
     return
  else if (nowd < 15) then
     postm = nowm
     prevm = nowm - 1
     if (prevm == 0) prevm = 12
     factor = real(ndays(prevm)-15+nowd)/real(ndays(prevm))
  else if (nowd > 15) then
     prevm = nowm
     postm = nowm + 1
     if (postm == 13) postm = 1
     factor = real(nowd-15)/real(ndays(prevm))
  endif

  nowval = a12(prevm)*(1.0-factor) + a12(postm)*factor

end function month_d

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------


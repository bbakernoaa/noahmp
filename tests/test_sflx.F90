program test_sflx
  use module_sf_noahmplsm
  use machine, only: kind_phys
  implicit none

  ! Parameters matching module_sf_noahmplsm (private there)
  integer, parameter :: nsoil_p = 4
  integer, parameter :: nsnow_p = 3
  integer, parameter :: nstage_p = 8

  type(noahmp_parameters) :: parameters
  integer :: iloc, jloc, nsoil, nsnow, ice, ist, vegtyp, croptype, itime, psi_opt, iz0tlnd, yearlen
  real(kind=kind_phys) :: lat, julian, cosz, dt, dx, dz8w, shdfac, shdmax, sfctmp, sfcprs, psfc, uu, vv, q2, garea1
  real(kind=kind_phys) :: qc, soldn, lwdn, pblhx, prcpconv, prcpnonc, prcpshcv, prcpsnow, prcpgrpl, prcphail
  real(kind=kind_phys) :: tbot, co2air, o2air, foln, zlvl, ep_1, ep_2, epsm1, cp

  real(kind=kind_phys), dimension(nsoil_p) :: zsoil, smceq
  real(kind=kind_phys), dimension(-nsnow_p+1:nsoil_p) :: stc, zsnso
  real(kind=kind_phys), dimension(nsoil_p) :: smc, sh2o
  real(kind=kind_phys), dimension(-nsnow_p+1:0) :: ficeold, snice, snliq

  real(kind=kind_phys) :: albold, sneqvo, tah, eah, fwet, canliq, canice, tv, tg, qsfc, qsnow, qrain
  integer :: isnow
  real(kind=kind_phys) :: snowh, sneqv, zwt, wa, wt, wslake, lfmass, rtmass, stmass, wood, stblcp, fastcp, lai, sai, cm, ch, tauss
  real(kind=kind_phys) :: grain, gdd
  integer :: pgs
  real(kind=kind_phys) :: smcwtd, deeprech, rech, ustarx, z0wrf, z0hwrf, ts
  real(kind=kind_phys) :: fsa, fsr, fira, fsh, ssoil, fcev, fgev, fctr, ecan, etran, edir, trad
  real(kind=kind_phys) :: tgb, tgv, t2mv, t2mb, q2v, q2b, runsrf, runsub, apar, psn, sav, sag
  real(kind=kind_phys) :: fsno, nee, gpp, npp, fveg, albedo, qsnbot, ponding, ponding1, ponding2, rssun, rssha
  real(kind=kind_phys), dimension(2) :: albd, albi, albsnd, albsni
  real(kind=kind_phys) :: bgap, wgap, chv, chb, emissi
  real(kind=kind_phys) :: shg, shc, shb, evg, evb, ghv, ghb, irg, irc, irb, tr, evc, chleaf, chuc, chv2, chb2, fpice, pahv, pahg, pahb, pah
  real(kind=kind_phys) :: esnow, canhs, laisun, laisha, rb, qsfcveg, qsfcbare
  character(len=1024) :: errmsg
  integer :: errflg

  integer :: i_dveg, i_run, i_sfc

  ! Initialize parameters structure
  parameters%urban_flag = .false.
  parameters%iswater = 16
  parameters%isbarren = 19
  parameters%isice = 24
  parameters%iscrop = 2
  parameters%eblforest = 13
  parameters%ch2op = 0.1
  parameters%dleaf = 0.04
  parameters%z0mvt = 0.1
  parameters%hvt = 10.0
  parameters%hvb = 0.0
  parameters%z0mhvt = 0.1
  parameters%den = 0.1
  parameters%rc = 1.0
  parameters%mfsno = 1.0
  parameters%scffac = 1.0
  parameters%cbiom = 1.0
  parameters%saim = 0.1
  parameters%laim = 1.0
  parameters%sla = 0.02
  parameters%prcpiceden = 50.0
  parameters%dilefc = 1.0e-6
  parameters%dilefw = 1.0e-6
  parameters%fragr = 0.3
  parameters%ltovrc = 1.0e-7
  parameters%c3psn = 1.0
  parameters%kc25 = 30.0
  parameters%akc = 2.1
  parameters%ko25 = 30000.0
  parameters%ako = 1.2
  parameters%vcmx25 = 50.0
  parameters%avcmx = 2.4
  parameters%bp = 2000.0
  parameters%mp = 9.0
  parameters%qe25 = 0.06
  parameters%aqe = 1.0
  parameters%rmf25 = 1.0
  parameters%rms25 = 1.0e-7
  parameters%rmr25 = 1.0e-7
  parameters%arm = 2.0
  parameters%folnmx = 1.0
  parameters%wdpool = 0.0
  parameters%wrrat = 1.0
  parameters%mrp = 1.0
  parameters%cwpvt = 1.0
  parameters%tdlef = 270.0
  parameters%nroot = 3
  parameters%rgl = 100.0
  parameters%rsmin = 100.0
  parameters%hs = 30.0
  parameters%topt = 298.0
  parameters%rsmax = 5000.0
  parameters%slarea = 100.0
  parameters%eps = 0.95
  parameters%albsat = 0.2
  parameters%albdry = 0.3
  parameters%albice = 0.5
  parameters%alblak = 0.1
  parameters%omegas = 0.8
  parameters%betads = 0.5
  parameters%betais = 0.5
  parameters%eg = 0.95
  parameters%co2 = 400.0
  parameters%o2 = 210000.0
  parameters%timean = 10.0
  parameters%fsatmx = 0.5
  parameters%z0sno = 0.002
  parameters%ssi = 0.03
  parameters%snow_ret_fac = 0.00001
  parameters%swemx = 1.0
  parameters%snow_emis = 0.95
  parameters%tau0 = 1.0e6
  parameters%grain_growth = 0.1
  parameters%extra_growth = 0.1
  parameters%dirt_soot = 0.1
  parameters%bats_cosz = 0.5
  parameters%bats_vis_new = 0.95
  parameters%bats_nir_new = 0.65
  parameters%bats_vis_age = 0.2
  parameters%bats_nir_age = 0.5
  parameters%bats_vis_dir = 0.4
  parameters%bats_nir_dir = 0.4
  parameters%rsurf_snow = 50.0
  parameters%rsurf_exp = 5.0
  parameters%tmin = 200.0
  parameters%pltday = 100
  parameters%hsday = 200
  parameters%plantpop = 1.0
  parameters%irri = 0.0
  parameters%gddtbase = 10.0
  parameters%gddtcut = 30.0
  parameters%gdds1 = 100.0
  parameters%gdds2 = 200.0
  parameters%gdds3 = 300.0
  parameters%gdds4 = 400.0
  parameters%gdds5 = 500.0
  parameters%c3c4 = 1
  parameters%aref = 10.0
  parameters%psnrf = 1.0
  parameters%i2par = 0.45
  parameters%tassim0 = 0.0
  parameters%tassim1 = 10.0
  parameters%tassim2 = 30.0
  parameters%k = 0.5
  parameters%epsi = 0.05
  parameters%q10mr = 2.0
  parameters%foln_mx = 1.0
  parameters%lefreez = 273.15
  parameters%dile_fc = 1.0e-6
  parameters%dile_fw = 1.0e-6
  parameters%fra_gr = 0.3
  parameters%lf_ovrc = 1.0e-7
  parameters%st_ovrc = 1.0e-7
  parameters%rt_ovrc = 1.0e-7
  parameters%lfmr25 = 1.0
  parameters%stmr25 = 1.0e-7
  parameters%rtmr25 = 1.0e-7
  parameters%grainmr25 = 1.0e-7
  parameters%lfpt = 0.25
  parameters%stpt = 0.25
  parameters%rtpt = 0.25
  parameters%grainpt = 0.25
  parameters%bio2lai = 0.02
  parameters%bexp = 4.0
  parameters%smcdry = 0.1
  parameters%smcwlt = 0.1
  parameters%smcref = 0.3
  parameters%smcmax = 0.45
  parameters%psisat = -0.1
  parameters%dksat = 1.0e-5
  parameters%dwsat = 1.0e-5
  parameters%quartz = 0.1
  parameters%f1 = 0.1
  parameters%slope = 0.1
  parameters%csoil = 2.0e6
  parameters%zbot = 8.0
  parameters%czil = 0.1
  parameters%refdk = 2.0e-6
  parameters%refkdt = 3.0
  parameters%kdt = 3.0
  parameters%frzx = 0.1

  iloc = 1
  jloc = 1
  lat = 0.6
  yearlen = 365
  julian = 100.0
  cosz = 0.8
  dt = 3600.0
  dx = 1000.0
  dz8w = 10.0
  nsoil = nsoil_p
  nsnow = nsnow_p
  zsoil = (/0.1, 0.4, 1.0, 2.0/)
  shdfac = 0.5
  shdmax = 0.8
  vegtyp = 1
  ice = 0
  ist = 1
  croptype = 0
  smceq = 0.3
  sfctmp = 290.0
  sfcprs = 101325.0
  psfc = 101325.0
  uu = 5.0
  vv = 0.0
  q2 = 0.01
  garea1 = 1000.0
  qc = 0.0
  soldn = 500.0
  lwdn = 300.0
  pblhx = 1000.0
  iz0tlnd = 1
  itime = 1
  psi_opt = 1
  prcpconv = 0.0
  prcpnonc = 0.0
  prcpshcv = 0.0
  prcpsnow = 0.0
  prcpgrpl = 0.0
  prcphail = 0.0
  tbot = 285.0
  co2air = 400.0
  o2air = 210000.0
  foln = 1.0
  ficeold = 0.0
  zlvl = 10.0
  ep_1 = 0.622
  ep_2 = 0.622
  epsm1 = 0.622
  cp = 1004.0
  albold = 0.2
  sneqvo = 0.0
  stc = 280.0
  sh2o = 0.3
  smc = 0.3
  tah = 285.0
  eah = 1000.0
  fwet = 0.0
  canliq = 0.0
  canice = 0.0
  tv = 285.0
  tg = 285.0
  qsfc = 0.01
  qsnow = 0.0
  qrain = 0.0
  isnow = 0
  zsnso(-nsnow_p+1:0) = (/-0.1, -0.05, -0.02/)
  zsnso(1:nsoil_p) = zsoil
  snowh = 0.0
  sneqv = 0.0
  snice = 0.0
  snliq = 0.0
  zwt = -2.0
  wa = 100.0
  wt = 100.0
  wslake = 0.0
  lfmass = 1.0
  rtmass = 1.0
  stmass = 1.0
  wood = 1.0
  stblcp = 1.0
  fastcp = 1.0
  lai = 1.0
  sai = 0.1
  cm = 0.01
  ch = 0.01
  tauss = 0.0
  grain = 0.0
  gdd = 0.0
  pgs = 0
  smcwtd = 0.3
  deeprech = 0.0
  rech = 0.0
  ustarx = 0.1

  print *, "Testing noahmp_sflx with various configurations..."

  do i_dveg = 1, 4
     do i_run = 1, 3
        do i_sfc = 1, 2
           print *, "Config: dveg=", i_dveg, " run=", i_run, " sfc=", i_sfc
           call noahmp_options(i_dveg, 1, 1, i_run, i_sfc, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)

           errmsg = ""
           errflg = 0
           call noahmp_sflx (parameters, &
                            iloc    , jloc    , lat     , yearlen , julian  , cosz    , &
                            dt      , dx      , dz8w    , nsoil   , zsoil   , nsnow   , &
                            shdfac  , shdmax  , vegtyp  , ice     , ist     , croptype, &
                            smceq   ,                                                   &
                            sfctmp  , sfcprs  , psfc    , uu      , vv , q2, garea1   , &
                            qc      , soldn   , lwdn, .true., sfcprs, sfcprs, sfcprs, &
                            pblhx   , iz0tlnd , itime         ,psi_opt                 ,&
	                    prcpconv, prcpnonc, prcpshcv, prcpsnow, prcpgrpl, prcphail, &
                            tbot    , co2air  , o2air   , foln    , ficeold , zlvl    , &
                            ep_1    , ep_2    , epsm1   , cp                          , &
                            albold  , sneqvo  ,                                         &
                            stc     , sh2o    , smc     , tah     , eah     , fwet    , &
                            canliq  , canice  , tv      , tg      , qsfc, qsnow, qrain, &
                            isnow   , zsnso   , snowh   , sneqv   , snice   , snliq   , &
                            zwt     , wa      , wt      , wslake  , lfmass  , rtmass  , &
                            stmass  , wood    , stblcp  , fastcp  , lai     , sai     , &
                            cm      , ch      , tauss   ,                               &
                            grain   , gdd     , pgs     ,                               &
                            smcwtd  ,deeprech , rech    , ustarx  ,                     &
		            z0wrf   , z0hwrf  , ts      ,                               &
                            fsa     , fsr     , fira     , fsh     , ssoil   , fcev    , &
                            fgev    , fctr    , ecan    , etran   , edir    , trad    , &
                            tgb     , tgv     , t2mv    , t2mb    , q2v     , q2b     , &
                            runsrf  , runsub  , apar    , psn     , sav     , sag     , &
                            fsno    , nee     , gpp     , npp     , fveg    , albedo  , &
                            qsnbot  , ponding , ponding1, ponding2, rssun   , rssha   , &
                            albd    , albi    , albsnd  , albsni                      , &
                            bgap    , wgap    , chv     , chb     , emissi  ,           &
		            shg     , shc     , shb     , evg     , evb     , ghv     , &
		            ghb     , irg     , irc     , irb     , tr      , evc     , &
		            chleaf  , chuc    , chv2    , chb2    , fpice   , pahv    , &
                            pahg    , pahb    , pah     , esnow   , canhs   , laisun  , &
                            laisha  , rb      , qsfcveg , qsfcbare                      &
                            ,errmsg, errflg)
           if (errflg /= 0) then
              print *, "FAILED in config: errflg=", errflg
              print *, "errmsg: ", trim(errmsg)
           end if
        end do
     end do
  end do

  ! Also test with ice = 1
  print *, "Testing noahmp_sflx with ice=1..."
  ice = 1
  call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
  errmsg = ""
  errflg = 0
  call noahmp_sflx (parameters, &
                   iloc    , jloc    , lat     , yearlen , julian  , cosz    , &
                   dt      , dx      , dz8w    , nsoil   , zsoil   , nsnow   , &
                   shdfac  , shdmax  , vegtyp  , ice     , ist     , croptype, &
                   smceq   ,                                                   &
                   sfctmp  , sfcprs  , psfc    , uu      , vv , q2, garea1   , &
                   qc      , soldn   , lwdn, .true., sfcprs, sfcprs, sfcprs, &
                   pblhx   , iz0tlnd , itime         ,psi_opt                 ,&
	           prcpconv, prcpnonc, prcpshcv, prcpsnow, prcpgrpl, prcphail, &
                   tbot    , co2air  , o2air   , foln    , ficeold , zlvl    , &
                   ep_1    , ep_2    , epsm1   , cp                          , &
                   albold  , sneqvo  ,                                         &
                   stc     , sh2o    , smc     , tah     , eah     , fwet    , &
                   canliq  , canice  , tv      , tg      , qsfc, qsnow, qrain, &
                   isnow   , zsnso   , snowh   , sneqv   , snice   , snliq   , &
                   zwt     , wa      , wt      , wslake  , lfmass  , rtmass  , &
                   stmass  , wood    , stblcp  , fastcp  , lai     , sai     , &
                   cm      , ch      , tauss   ,                               &
                   grain   , gdd     , pgs     ,                               &
                   smcwtd  ,deeprech , rech    , ustarx  ,                     &
		   z0wrf   , z0hwrf  , ts      ,                               &
                   fsa     , fsr     , fira     , fsh     , ssoil   , fcev    , &
                   fgev    , fctr    , ecan    , etran   , edir    , trad    , &
                   tgb     , tgv     , t2mv    , t2mb    , q2v     , q2b     , &
                   runsrf  , runsub  , apar    , psn     , sav     , sag     , &
                   fsno    , nee     , gpp     , npp     , fveg    , albedo  , &
                   qsnbot  , ponding , ponding1, ponding2, rssun   , rssha   , &
                   albd    , albi    , albsnd  , albsni                      , &
                   bgap    , wgap    , chv     , chb     , emissi  ,           &
		   shg     , shc     , shb     , evg     , evb     , ghv     , &
		   ghb     , irg     , irc     , irb     , tr      , evc     , &
		   chleaf  , chuc    , chv2    , chb2    , fpice   , pahv    , &
                   pahg    , pahb    , pah     , esnow   , canhs   , laisun  , &
                   laisha  , rb      , qsfcveg , qsfcbare                      &
                   ,errmsg, errflg)
  if (errflg /= 0) then
     print *, "FAILED in ice=1: errflg=", errflg
     print *, "errmsg: ", trim(errmsg)
  end if

  print *, "noahmp_sflx comprehensive test finished"

end program test_sflx

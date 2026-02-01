program test_glacier
  use module_sf_noahmp_glacier
  use machine, only: kind_phys
  implicit none

  integer :: iloc, jloc, nsnow, nsoil, psi_opt, iz0tlnd, itime, isnow
  real(kind=kind_phys) :: dt, cosz, sfctmp, sfcprs, uu, vv, q2, soldn, prcp, lwdn, tbot, zlvl, psfc, pblhx, ep_1, ep_2, epsm1, cp, sigmaf1, garea1
  real(kind=kind_phys), dimension(4) :: zsoil
  real(kind=kind_phys), dimension(3) :: ficeold
  real(kind=kind_phys) :: qsnow, sneqvo, albold, cm, ch, sneqv, snowh, tg, tauss, qsfc
  real(kind=kind_phys), dimension(4) :: smc, sh2o
  real(kind=kind_phys), dimension(-2:4) :: zsnso, stc
  real(kind=kind_phys), dimension(-2:0) :: snice, snliq
  real(kind=kind_phys) :: fsa, fsr, fira, fsh, fgev, ssoil, trad, edir, runsrf, runsub, sag, albedo, qsnbot, ponding, ponding1, ponding2, t2m, q2e, z0h_total
  real(kind=kind_phys) :: emissi, fpice, ch2b, esnow
  real(kind=kind_phys), dimension(2) :: albsnd, albsni
  character(len=256) :: errmsg
  integer :: errflg

  integer :: i_alb, i_snf, i_tbot, i_stc, i_gla, i_sfc, i_trs

  iloc = 1
  jloc = 1
  dt = 3600.0
  nsnow = 3
  nsoil = 4
  psi_opt = 1
  cosz = 0.8
  sfctmp = 260.0
  sfcprs = 101325.0
  uu = 5.0
  vv = 0.0
  q2 = 0.001
  soldn = 500.0
  prcp = 0.0
  lwdn = 200.0
  tbot = 270.0
  zlvl = 10.0
  psfc = 101325.0
  pblhx = 1000.0
  ep_1 = 0.622
  ep_2 = 0.622
  epsm1 = 0.622
  cp = 1004.0
  iz0tlnd = 1
  itime = 1
  sigmaf1 = 0.0
  garea1 = 1000.0

  zsoil = (/0.1, 0.4, 1.0, 2.0/)
  ficeold = (/0.0, 0.0, 0.0/)
  qsnow = 0.0
  sneqvo = 0.0
  albold = 0.5
  cm = 0.01
  ch = 0.01
  isnow = 0
  sneqv = 0.0
  smc = 0.3
  zsnso(-2) = -0.1
  zsnso(-1) = -0.05
  zsnso(0) = -0.02
  zsnso(1:4) = zsoil
  snowh = 0.0
  snice = 0.0
  snliq = 0.0
  tg = 260.0
  stc = 260.0
  sh2o = 0.3
  tauss = 0.0
  qsfc = 0.001

  print *, "Testing noahmp_glacier with various configurations..."

  do i_alb = 1, 2
     do i_snf = 1, 3
        do i_stc = 1, 2
           do i_gla = 1, 2
              print *, "Config: alb=", i_alb, " snf=", i_snf, " stc=", i_stc, " gla=", i_gla
              call noahmp_options_glacier(i_alb, i_snf, 2, i_stc, i_gla, 1, 1)

              errmsg = ""
              errflg = 0
              call noahmp_glacier ( &
                               iloc      ,jloc    ,cosz     ,nsnow    ,nsoil   ,dt        , &
                               sfctmp    ,sfcprs  ,uu       ,vv       ,q2      ,soldn     , &
                               prcp      ,lwdn    ,tbot     ,zlvl     ,ficeold ,zsoil     , &
                               .true.    ,sfcprs  ,sfcprs   ,sfcprs   ,                     &
                               psfc      ,pblhx  ,iz0tlnd   ,itime    ,                     &
                               sigmaf1   ,garea1  ,psi_opt   ,                               &
                               ep_1      ,ep_2   ,epsm1     ,cp       ,                     &
                               qsnow     ,sneqvo  ,albold   ,cm       ,ch      ,isnow     , &
                               sneqv     ,smc     ,zsnso    ,snowh    ,snice   ,snliq     , &
                               tg        ,stc     ,sh2o     ,tauss    ,qsfc               , &
                               fsa       ,fsr     ,fira     ,fsh      ,fgev    ,ssoil     , &
                               trad      ,edir    ,runsrf   ,runsub   ,sag     ,albedo    , &
                               qsnbot    ,ponding ,ponding1 ,ponding2 ,t2m,q2e ,z0h_total , &
                               emissi    ,fpice   ,ch2b     , esnow   , albsnd , albsni   , &
                               errmsg    ,errflg)
           end do
        end do
     end do
  end do

  print *, "noahmp_glacier comprehensive test finished"

end program test_glacier

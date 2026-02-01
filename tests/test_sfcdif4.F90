program test_sfcdif4
  use module_sf_noahmplsm
  use machine, only: kind_phys
  implicit none

  integer :: iloc, jloc, itime, psi_opt, isice, iz0tlnd
  real(kind=kind_phys) :: pblhx, tsk, psfcpa, p1d, t1d, qx, zlvl, hfx, qfx, dx, ux, vx, znt, snwh, ep_1, ep_2, cp
  real(kind=kind_phys) :: qsfc, ust, chs, chs2, cqs2, cm, rmolx, rbx, fmx, fhx, stressx, fm10x, fh2x, wspdx, flhcx, flqcx

  iloc = 1
  jloc = 1
  itime = 1
  psi_opt = 1
  isice = 0
  pblhx = 1000.0
  tsk = 300.0
  psfcpa = 101325.0
  p1d = 100000.0
  t1d = 298.0
  qx = 0.01
  zlvl = 10.0
  hfx = 10.0
  qfx = 0.001
  dx = 1000.0
  ux = 5.0
  vx = 0.0
  znt = 0.1
  snwh = 0.0
  ep_1 = 0.622
  ep_2 = 0.622
  cp = 1004.0
  iz0tlnd = 1

  qsfc = 0.01
  ust = 0.1
  chs = 0.01
  chs2 = 0.01
  cqs2 = 0.01
  cm = 0.01
  rmolx = 0.0
  rbx = 0.0
  fmx = 0.0
  fhx = 0.0
  stressx = 0.0
  fm10x = 0.0
  fh2x = 0.0
  wspdx = 5.0
  flhcx = 0.0
  flqcx = 0.0

  print *, "Testing sfcdif4 with various configurations..."

  do isice = 0, 1
     do itime = 1, 2
        do psi_opt = 0, 1
           do tsk = 260.0, 310.0, 50.0
              do hfx = -10.0, 10.0, 20.0
                 print *, "Config: isice=", isice, " itime=", itime, " psi_opt=", psi_opt, " tsk=", tsk, " hfx=", hfx
                 call sfcdif4(iloc, jloc, ux, vx, t1d, &
                              p1d, psfcpa, pblhx, dx, znt, &
                              ep_1, ep_2, cp, &
                              itime, snwh, isice, psi_opt, &
                              tsk, qx, zlvl, iz0tlnd, qsfc, &
                              hfx, qfx, cm, chs, chs2, &
                              cqs2, &
                              rmolx, ust, rbx, fmx, fhx, stressx, &
                              fm10x, fh2x, wspdx, flhcx, flqcx)
              end do
           end do
        end do
     end do
  end do

  print *, "sfcdif4 comprehensive test PASSED"

end program test_sfcdif4

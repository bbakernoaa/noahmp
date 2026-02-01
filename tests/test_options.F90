program test_options
  use module_sf_noahmplsm
  implicit none

  print *, "Testing noahmp_options..."

  ! idveg, iopt_crs, iopt_btr, iopt_run, iopt_sfc, iopt_frz, iopt_inf, iopt_rad, iopt_alb, iopt_snf, iopt_tbot, iopt_stc, iopt_rsf, iopt_soil, iopt_pedo, iopt_crop, iopt_trs, iopt_diag, iopt_z0m
  call noahmp_options(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)

  ! Testing if global variables in module_sf_noahmplsm are updated
  if (dveg == 1) then
     print *, "PASSED: dveg"
  else
     print *, "FAILED: dveg", dveg
     stop 1
  endif

  print *, "All options tests PASSED"
  stop 0

end program test_options

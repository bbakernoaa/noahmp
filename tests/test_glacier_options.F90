program test_glacier_options
  use module_sf_noahmp_glacier
  implicit none

  print *, "Testing noahmp_options_glacier..."

  ! iopt_alb, iopt_snf, iopt_tbot, iopt_stc, iopt_gla, iopt_sfc, iopt_trs
  call noahmp_options_glacier(1, 1, 1, 1, 1, 1, 1)

  if (opt_alb == 1) then
     print *, "PASSED: opt_alb"
  else
     print *, "FAILED: opt_alb", opt_alb
     stop 1
  endif

  print *, "All glacier options tests PASSED"
  stop 0

end program test_glacier_options

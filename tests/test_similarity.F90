program test_similarity
  use module_sf_noahmplsm, only: psi_init
  implicit none

  character(len=256) :: errmsg
  integer :: errflg

  print *, "Testing psi_init..."

  ! Test with psi_opt = 0
  call psi_init(0, errmsg, errflg)
  if (errflg /= 0) then
    print *, "FAILED: psi_init(0): ", trim(errmsg)
    stop 1
  else
    print *, "PASSED: psi_init(0): ", trim(errmsg)
  endif

  ! Test with psi_opt = 1
  call psi_init(1, errmsg, errflg)
  if (errflg /= 0) then
    print *, "FAILED: psi_init(1): ", trim(errmsg)
    stop 1
  else
    print *, "PASSED: psi_init(1): ", trim(errmsg)
  endif

  print *, "All similarity tests PASSED"
  stop 0

end program test_similarity

program test_physcons
  use physcons
  implicit none

  logical :: all_passed = .true.
  real(kind=kind_phys), parameter :: epsilon = 1.0e-6_kind_phys

  print *, "Testing physical constants..."

  if (abs(con_pi - 3.1415926535897931_kind_phys) > epsilon) then
    print *, "FAILED: con_pi"
    all_passed = .false.
  else
    print *, "PASSED: con_pi"
  endif

  if (abs(con_g - 9.80665_kind_phys) > epsilon) then
    print *, "FAILED: con_g"
    all_passed = .false.
  else
    print *, "PASSED: con_g"
  endif

  if (abs(con_rd - 287.05_kind_phys) > epsilon) then
    print *, "FAILED: con_rd"
    all_passed = .false.
  else
    print *, "PASSED: con_rd"
  endif

  if (all_passed) then
    print *, "All physical constants tests PASSED"
    stop 0
  else
    print *, "Some physical constants tests FAILED"
    stop 1
  endif

end program test_physcons

program test_options
  use module_sf_noahmplsm
  implicit none
  integer :: i
  print *, "Testing noahmp_options..."
  do i = 1, 10
    call noahmp_options(i, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
  end do
  do i = 1, 4
    call noahmp_options(1, i, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, i, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, i, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, i, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, i, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, i, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, i, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, i, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, i, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, i, 1, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, i, 1, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, i, 1, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, i, 1, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, i, 0, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, i, 1, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, i, 1, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, i, 1)
    call noahmp_options(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, i)
  end do
  print *, "All options tests PASSED"
  stop 0
end program test_options

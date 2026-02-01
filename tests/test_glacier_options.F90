program test_glacier_options
  use module_sf_noahmp_glacier
  implicit none
  integer :: i
  print *, "Testing noahmp_options_glacier..."
  do i = 1, 4
    call noahmp_options_glacier(i, 1, 1, 1, 1, 1, 1)
    call noahmp_options_glacier(1, i, 1, 1, 1, 1, 1)
    call noahmp_options_glacier(1, 1, i, 1, 1, 1, 1)
    call noahmp_options_glacier(1, 1, 1, i, 1, 1, 1)
    call noahmp_options_glacier(1, 1, 1, 1, i, 1, 1)
    call noahmp_options_glacier(1, 1, 1, 1, 1, i, 1)
    call noahmp_options_glacier(1, 1, 1, 1, 1, 1, i)
  end do
  print *, "All glacier options tests PASSED"
  stop 0
end program test_glacier_options

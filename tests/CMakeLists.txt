# Tests CMakeLists.txt

# Add test for physical constants
add_executable(test_physcons test_physcons.F90)
target_link_libraries(test_physcons noahmp)
add_test(NAME test_physcons COMMAND test_physcons)

# Add test for similarity functions
add_executable(test_similarity test_similarity.F90)
target_link_libraries(test_similarity noahmp)
add_test(NAME test_similarity COMMAND test_similarity)

# Add test for noahmp_options
add_executable(test_options test_options.F90)
target_link_libraries(test_options noahmp)
add_test(NAME test_options COMMAND test_options)

# Add test for noahmp_options_glacier
add_executable(test_glacier_options test_glacier_options.F90)
target_link_libraries(test_glacier_options noahmp)
add_test(NAME test_glacier_options COMMAND test_glacier_options)

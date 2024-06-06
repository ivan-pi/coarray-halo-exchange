set(SHARED_SRC coarray_collectives.f90 main.f90)

# Since each index_map_type.f90 produces a .mod file we need
# to ensure the tests are built sequentially and not in parallel.

add_executable(test-coarray1 method1/index_map_type.f90 ${SHARED_SRC})

add_executable(test-coarray1a method1a/index_map_type.f90 ${SHARED_SRC})
add_dependencies(test-coarray1a test-coarray1)

add_executable(test-coarray1b method1b/index_map_type.f90 ${SHARED_SRC})
add_dependencies(test-coarray1b test-coarray1a)

add_executable(test-coarray2 method2/index_map_type.f90 ${SHARED_SRC})
add_dependencies(test-coarray2 test-coarray1b)

add_executable(test-coarray3 method3/index_map_type.f90 ${SHARED_SRC})
add_dependencies(test-coarray3 test-coarray2)

add_executable(test-coarray4 method4/index_map_type.f90 ${SHARED_SRC})
add_dependencies(test-coarray4 test-coarray3)


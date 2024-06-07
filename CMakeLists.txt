cmake_minimum_required(VERSION 3.21)

project(coarray_halo_exchange Fortran)

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Setting build type to 'Release' as none was specified")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build" FORCE)
endif()

option(BUILD_MPI_TEST "Build an MPI version of the test program" OFF)

add_compile_definitions("$<$<NOT:$<CONFIG:Debug>>:NDEBUG>")

# Set missing CMake defaults for the NAG Fortran compiler
if(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)
  set(CMAKE_Fortran_FLAGS_DEBUG "-u -O0 -gline -C -nan")
  set(CMAKE_Fortran_FLAGS_RELEASE "-u -O3")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELEASE} -gline")
endif()

if(BUILD_MPI_TEST)
  find_package(MPI REQUIRED Fortran)
  add_subdirectory(mpi)
else()
  add_compile_options($<$<Fortran_COMPILER_ID:NAG,Intel,IntelLLVM>:-coarray>)
  add_link_options($<$<Fortran_COMPILER_ID:NAG,Intel,IntelLLVM>:-coarray>)
  add_subdirectory(coarray)
endif()

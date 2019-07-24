cmake_minimum_required(VERSION 2.8)

# set fortran compiler
# set(CMAKE_Fortran_COMPILER gfortran-mp-9) 
#set(CMAKE_Fortran_FLAGS --std=legacy)

project(bmigipl Fortran)

set(bmi_version 1.0)
set(bmigipl_lib bmigiplf)
set(data_dir ${CMAKE_SOURCE_DIR}/data)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

set(CMAKE_MACOSX_RPATH 1)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

add_subdirectory(GIPL)
add_subdirectory(GIPL/tests)
add_subdirectory(GIPL/examples)

include(CTest)

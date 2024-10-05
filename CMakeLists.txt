cmake_minimum_required(VERSION 3.12)

project(bmigipl
  VERSION 1.3.dev0
  LANGUAGES Fortran
)

include(GNUInstallDirs)

set(bmi_version 1.0)
set(bmigipl_lib bmigiplf)
set(data_dir ${CMAKE_SOURCE_DIR}/data)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_subdirectory(GIPL)
add_subdirectory(GIPL/tests)
add_subdirectory(GIPL/examples)

include(CTest)

set(pkg_name gipl_model)

# GIPL model
add_library(${pkg_name} SHARED gipl.f90)
set_target_properties(${pkg_name} PROPERTIES
  PUBLIC_HEADER ${CMAKE_Fortran_MODULE_DIRECTORY}/${pkg_name}.mod
)

# BMI specification
add_library(bmif SHARED bmi.f90)
set_target_properties(bmif PROPERTIES
  VERSION ${bmi_version}
  PUBLIC_HEADER ${CMAKE_Fortran_MODULE_DIRECTORY}/bmif_${bmi_major_version}_${bmi_minor_version}.mod
)

# BMI for GIPL model
add_library(${bmigipl_lib} SHARED bmigiplf.f90)
target_link_libraries(${bmigipl_lib} ${pkg_name} bmif)
set_target_properties(${bmigipl_lib} PROPERTIES
  PUBLIC_HEADER ${CMAKE_Fortran_MODULE_DIRECTORY}/${bmigipl_lib}.mod
)

# Executable main programs
add_executable(run_${pkg_name} main.f90)
target_link_libraries(run_${pkg_name} ${pkg_name})
add_executable(run_bmi${pkg_name} bmi_main.f90)
target_link_libraries(run_bmi${pkg_name} ${bmigipl_lib})

install(
  TARGETS ${pkg_name}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(
  TARGETS bmif
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(
  TARGETS ${bmigipl_lib}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(
  TARGETS run_${pkg_name}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
install(
  TARGETS run_bmi${pkg_name}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

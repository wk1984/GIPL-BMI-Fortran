# GIPL model
add_library(${model_name} SHARED gipl.f90)
set_target_properties(${model_name} PROPERTIES
  PUBLIC_HEADER ${CMAKE_Fortran_MODULE_DIRECTORY}/${model_name}.mod
)

# BMI specification
add_library(bmif SHARED bmi.f90)
set_target_properties(bmif PROPERTIES
  VERSION ${bmi_version}
  PUBLIC_HEADER ${CMAKE_Fortran_MODULE_DIRECTORY}/bmif_${bmi_major_version}_${bmi_minor_version}.mod
)

# BMI for GIPL model
add_library(${bmigipl_lib} SHARED bmigiplf.f90)
target_link_libraries(${bmigipl_lib} ${model_name} bmif)
set_target_properties(${bmigipl_lib} PROPERTIES
  PUBLIC_HEADER ${CMAKE_Fortran_MODULE_DIRECTORY}/${bmigipl_lib}.mod
)

# Executable main programs
add_executable(run_${model_name} main.f90)
target_link_libraries(run_${model_name} ${model_name})
add_executable(run_bmi${model_name} bmi_main.f90)
target_link_libraries(run_bmi${model_name} ${bmigipl_lib})

install(
  TARGETS ${model_name}
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
  TARGETS run_${model_name}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
install(
  TARGETS run_bmi${model_name}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

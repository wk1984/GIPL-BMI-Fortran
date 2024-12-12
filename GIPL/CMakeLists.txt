set(pkg_name gipl_model)

add_library(gipl OBJECT gipl.f90)
add_library(${bmigipl_lib} SHARED bmigiplf.f90 bmi.f90 $<TARGET_OBJECTS:gipl>)

add_executable(run_${pkg_name} main.f90 $<TARGET_OBJECTS:gipl>)
add_executable(run_bmi${pkg_name} bmi_main.f90)
target_link_libraries(run_bmi${pkg_name} ${bmigipl_lib})

install(
  TARGETS run_${pkg_name}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
install(
  TARGETS run_bmi${pkg_name}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
install(
  TARGETS ${bmigipl_lib}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
install(
  FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/${pkg_name}.mod
        ${CMAKE_Fortran_MODULE_DIRECTORY}/${bmigipl_lib}.mod
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

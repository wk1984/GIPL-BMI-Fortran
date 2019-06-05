set(pkg_name gipl_model)
set(mod_name ${pkg_name})
set(src_${pkg_name} gipl.f90 main.f90)
set(src_bmi${pkg_name} gipl.f90 bmigiplf.f90 bmi.f90)

add_library(${bmigipl_lib} SHARED ${src_bmi${pkg_name}})

add_executable(run_${pkg_name} ${src_${pkg_name}})

add_executable(run_bmi${pkg_name} bmi_main.f90)
target_link_libraries(run_bmi${pkg_name} ${bmigipl_lib})

install(
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run_${pkg_name}
  DESTINATION bin
  COMPONENT ${pkg_name})
install(
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run_bmi${pkg_name}
  DESTINATION bin
  COMPONENT ${pkg_name})
install(
  TARGETS ${bmigipl_lib}
  DESTINATION lib
  COMPONENT ${pkg_name})
install(
  FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/${mod_name}.mod
        ${CMAKE_Fortran_MODULE_DIRECTORY}/${bmigipl_lib}.mod
  DESTINATION include)

include(CTest)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

add_library(helpers OBJECT testing_helpers.f90)

function(make_example example_name)
  add_test(NAME ${example_name} COMMAND ${example_name})
  set(src_${example_name} ${example_name}.f90 $<TARGET_OBJECTS:helpers>)
  add_executable(${example_name} ${src_${example_name}})
  target_link_libraries(${example_name} ${bmi_lib} ${bmigipl_lib})
endfunction(make_example)

make_example(info_ex)
make_example(irf_ex)
make_example(get_value_ex)
make_example(set_value_ex)
make_example(conflicting_instances_ex)
make_example(change_air_temperature_ex)
make_example(vargrid_ex)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/test.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples)
  
file(
  COPY ${data_dir}/in/bound.txt
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples/in/)
  
file(
  COPY ${data_dir}/in/geo.txt
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples/in/)
  
file(
  COPY ${data_dir}/in/grid.txt
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples/in/)
  
file(
  COPY ${data_dir}/in/initial.txt
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples/in/)

file(
  COPY ${data_dir}/in/rsnow.txt
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples/in/)

file(
  COPY ${data_dir}/in/snow.txt
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples/in/)
  
file(
  COPY ${data_dir}/in/sites.txt
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples/in/)
  
file(
  COPY ${data_dir}/in/vegetation.txt
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples/in/)
  
file(
  COPY ${data_dir}/output/result.txt.txt
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples/output/)
  
file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/test2.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/GIPL/examples)

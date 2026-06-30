if (NOT PYTHON_ML_FSCA)
  add_library(py_fSCA_lib
    stub_fSCA.f90
    )
  return()
endif()


# Find Python
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)
# Execute Python to find NumPy include
execute_process(COMMAND ${Python3_EXECUTABLE} -c "import numpy; print(numpy.get_include())"
                OUTPUT_VARIABLE NumPy_INCLUDE_DIR
                OUTPUT_STRIP_TRAILING_WHITESPACE)
include_directories(${Python3_INCLUDE_DIRS} ${NumPy_INCLUDE_DIR})


add_library(py_fSCA_lib
  py_fSCA_bind_c.f90
  call_py_fSCA.c
)
target_link_libraries(py_fSCA_lib ${Python3_LIBRARIES})
target_include_directories(py_fSCA_lib PRIVATE ${Python3_INCLUDE_DIRS})

add_executable(test_ml_fSCA_driver
  python_test_driver.f90
)
target_link_libraries(test_ml_fSCA_driver py_fSCA_lib ${Python3_LIBRARIES})
target_include_directories(test_ml_fSCA_driver PRIVATE ${Python3_INCLUDE_DIRS})

set_target_properties(test_ml_fSCA_driver PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Create symlink after build so the test driver will run
set(PY_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/ml_fSCA.py)
set(PY_SCRIPT_LINK ${CMAKE_BINARY_DIR}/bin/ml_fSCA.py)
add_custom_command(TARGET test_ml_fSCA_driver POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/bin
    COMMAND ${CMAKE_COMMAND} -E create_symlink
        ${PY_SCRIPT} ${PY_SCRIPT_LINK}
    COMMENT "Creating symlink to ml_fSCA.py in bin directory"
)

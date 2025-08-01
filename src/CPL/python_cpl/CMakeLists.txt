# Find Python
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)

add_executable(call_python
  call_python_test_driver.f90
  call_python_bind_c.f90
  call_python.c
  )

# Link against Python
target_include_directories(call_python PRIVATE ${Python3_INCLUDE_DIRS})
target_link_libraries(call_python PRIVATE ${Python3_LIBRARIES})


# target_link_libraries(call_python
#   call_python_bind_c)

install(TARGETS call_python
    RUNTIME DESTINATION ${CMAKE_BINARY_DIR}/bin
)

# Path to Python script
set(PY_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/get_weights.py)
set(PY_SCRIPT_LINK ${CMAKE_BINARY_DIR}/bin/get_weights.py)

# Create symlink after build
add_custom_command(TARGET call_python POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/bin
    COMMAND ${CMAKE_COMMAND} -E create_symlink
        ${PY_SCRIPT} ${PY_SCRIPT_LINK}
    COMMENT "Creating symlink to get_weights.py in bin directory"
)

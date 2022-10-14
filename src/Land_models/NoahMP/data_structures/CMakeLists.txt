cmake_minimum_required (VERSION 2.8)

add_library(noahmp_data STATIC
        forcing.f90
        geometry.f90
        parameters.f90
        state.f90
        )

target_link_libraries(noahmp_data PUBLIC hydro_orchestrator)
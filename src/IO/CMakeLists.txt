cmake_minimum_required (VERSION 2.8)

# build the orchestrator static library
add_library(hydro_netcdf_layer STATIC
        netcdf_layer.f90
        )

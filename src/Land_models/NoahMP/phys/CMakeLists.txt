add_subdirectory("surfex")

add_library(noahmp_phys STATIC
        module_sf_noahmpdrv.F
        module_sf_noahmp_glacier.F
        module_sf_noahmp_groundwater.F
        module_sf_noahmplsm.F
)

target_link_libraries(noahmp_phys snowcro crocus_surfex py_fSCA_lib)

cmake_minimum_required (VERSION 2.8)

add_library(crocus_surfex STATIC
  ini_csts.F
  modd_csts.F
  modd_snow_metamo.F
  modd_snow_par.F
  modd_surf_atm.F
  mode_snow3l.F
  mode_surf_coefs.F
  mode_thermos.F
  tridiag_ground_snowcro.F
)

if (BUILD_CROCUS)
  add_library(snowcro
    module_snowcro.F
    )
else()
  add_library(snowcro
    module_snowcro_intercept.F
    )
endif()

target_link_libraries(snowcro crocus_surfex)

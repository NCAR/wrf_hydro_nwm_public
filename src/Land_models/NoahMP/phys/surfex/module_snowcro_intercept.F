! The purpose of this module is to intercept the snowcro function and raise an
! error if the BUILD_CROCUS compile option is off but the user chose the
! namelist runtime option for Crocus
module module_snowcro
  implicit none
contains
  subroutine snowcro(HSNOWRES, OGLACIER, HIMPLICIT_WIND,     &
       PPEW_A_COEF, PPEW_B_COEF,                                 &
       PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,       &
       PSNOWSWE,PSNOWRHO,PSNOWHEAT,PSNOWALB,                     &
       PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,PSNOWAGE,                 &
       PTSTEP,PPS,PSR,PRR,PPSN3L,                                &
       PTA,PTG,PSW_RAD,PQA,PVMOD,PLW_RAD, PRHOA,                 &
       PUREF,PEXNS,PEXNA,PDIRCOSZW,                              &
       PZREF,PZ0,PZ0EFF,PZ0H,PALB,                               &
       PSOILCOND,PD_G,                                           &
       PSNOWLIQ,PSNOWTEMP,PSNOWDZ,                               &
       PTHRUFAL,PGRNDFLUX,PEVAPCOR,PRNSNOW,PHSNOW,PGFLUXSNOW,    &
       PHPSNOW,PLES3L,PLEL3L,PEVAP,PSNDRIFT,PRI,                 &
       PEMISNOW,PCDSNOW,PUSTAR,PCHSNOW,PSNOWHMASS,PQS,           &
       PPERMSNOWFRAC,PZENITH,                                    &
       OSNOWDRIFT,OSNOWDRIFT_SUBLIM,OSNOW_ABS_ZENITH,            &
       HSNOWMETAMO,HSNOWRAD,                                     &
       act_level, VIS_ICEALB,                                    &
       PFSA_CROCUS, PFSR_CROCUS, PFIRA_CROCUS,                   &
       FLOW_ICE, FLOW_SNOW,                                      &
       I,J)

    real, intent(in)                    :: PTSTEP
    character(len=*), intent(in)        :: HSNOWRES
    logical, intent(in)                 :: OGLACIER
    character(len=*), intent(in)        :: HIMPLICIT_WIND
    integer, intent(in)                 :: act_level
    integer, intent(in)                 :: I,J
    real, dimension(:), intent(in), optional :: VIS_ICEALB
    real, dimension(:), intent(in)      :: PPS, PTA, PSW_RAD, PQA, PVMOD, PLW_RAD, PSR, PRR
    real, dimension(:), intent(in)      :: PTG, PSOILCOND, PD_G, PPSN3L
    real, dimension(:), intent(in)      :: PZREF, PUREF, PEXNS, PEXNA, PDIRCOSZW, PRHOA, PZ0, &
         PZ0EFF, PALB, PZ0H, PPERMSNOWFRAC
    real, dimension(:), intent(in)      :: PPEW_A_COEF, PPEW_B_COEF, &
         PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,      &
         PPEQ_B_COEF
    real, dimension(:), intent(inout)   :: PSNOWALB
    real, dimension(:,:), intent(inout) :: PSNOWHEAT, PSNOWRHO, PSNOWSWE
    real, dimension(:,:), intent(inout) :: PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST
    real, dimension(:,:), intent(inout) :: PSNOWAGE
    real, dimension(:,:), intent(out)   :: PSNOWLIQ, PSNOWTEMP, PSNOWDZ
    real, dimension(:), intent(out)     :: PTHRUFAL, PGRNDFLUX, PEVAPCOR
    real, dimension(:), intent(out)     :: FLOW_ICE,FLOW_SNOW
    real, dimension(:), intent(out)     :: PRNSNOW, PHSNOW, PGFLUXSNOW, PLES3L, PLEL3L, &
         PHPSNOW, PCDSNOW, PUSTAR, PEVAP, PSNDRIFT
    real, dimension(:), intent(out)     :: PCHSNOW, PEMISNOW, PSNOWHMASS
    real, dimension(:), intent(out)     :: PFSA_CROCUS, PFSR_CROCUS, PFIRA_CROCUS
    real, dimension(:), intent(out)     :: PRI, PQS
    real, dimension(:), intent(in)      :: PZENITH
    logical, intent(in)                 :: OSNOWDRIFT, OSNOWDRIFT_SUBLIM
    logical, intent(in)                 :: OSNOW_ABS_ZENITH
    character(3), intent(in)            :: HSNOWMETAMO, HSNOWRAD


    print *, "ERROR: recompile with Crocus option turned on to run Crocus"
    stop "ERROR: Crocus compile time option off"
  end subroutine snowcro
end module module_snowcro

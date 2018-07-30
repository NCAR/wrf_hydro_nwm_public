module module_version

implicit none

character (len=512), parameter :: wrfHydroVersion=WRF_HYDRO_VERSION
character (len=512), parameter :: nwmVersion=NWM_VERSION
private :: wrfHydroVersion, nwmVersion
public  :: get_model_version

contains

function get_model_version()
character (len=512) :: get_model_version

#ifdef NCEP_WCOSS
   get_model_version=NWM_VERSION
#else
   get_model_version=WRF_HYDRO_VERSION
#endif

end function get_model_version

end module module_version

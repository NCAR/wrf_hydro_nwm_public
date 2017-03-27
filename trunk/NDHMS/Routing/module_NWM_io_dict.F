! Module for handling all associated scale_factor, add_offset, and
! attributes for individual files across various possible 
! National Water Model output files. In the future, this will move to a
! table that the user will be able to switch on/off variables for
! outputting. For now, attributes, etc will be stored here. 

! Logan Karsten
! National Center for Atmospheric Research
! Research Applications Laboratory
! karsten@ucar.edu     
! 303-497-2693
! 303-497-2693

module module_NWM_io_dict
implicit none

! Declare parameter values for module.
integer, parameter :: numChVars=10
integer, parameter :: numLdasVars = 95
integer, parameter :: numRtDomainVars = 4
integer, parameter :: numLakeVars = 2
integer :: i

! Declare public types that will hold metadata
public :: chrtMeta ! Public CHRTOUT metadata for NWM output.
public :: ldasMeta ! Public LDASOUT metadata for NWM output.
public :: rtDomainMeta ! Public RT_DOMAIN metadata for NWM output. 
public :: lakeMeta ! Public lake metadata for NWM output

! Establish types for each output type
type chrtMeta
   ! Variable names
   character (len=64), dimension(numChVars) :: varNames
   integer :: numVars = numChVars
   ! Output variable attributes
   real, dimension(numChVars) :: scaleFactor ! scale_factor values used for each
                                                  ! variable to converte from real to
                                                  ! integer.
   real, dimension(numChVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numChVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numChVars) :: units ! Units for each variable.
   character (len=64), dimension(numChVars) :: coordNames ! Coordinate names for each variable. 
   integer*8, dimension(numChVars) :: validMinComp ! Valid min (after conversion to integer)
   integer*8, dimension(numChVars) :: validMaxComp ! Valid max (after conversion to integer)
   real, dimension(numChVars) :: validMinReal ! Valid minimum (before conversion to integer)
   real, dimension(numChVars) :: validMaxReal ! Valid maximum (before converstion to integer)
   integer*8, dimension(numChVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numChVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numChVars) :: fillReal ! Fill value (before conversion to integer)
   integer*8, dimension(numChVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numChVars) :: outFlag ! 0/1 flag to turn outputting off/on 
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   ! feature_id attributes
   character (len=64) :: featureIdLName ! long_name - usually Reach ID
   character (len=256) :: featureIdComment ! Comment attribute
   ! latitude variable attributes
   character (len=64) :: latLName ! long_name
   character (len=64) :: latUnits ! units
   character (len=64) :: latStName ! Standard Name
   ! longitude variable attributes
   character (len=64) :: lonLName ! long_name
   character (len=64) :: lonUnits ! units
   character (len=64) :: lonStName ! Standard Name
   ! Elevation variable attributes
   character (len=64) :: elevLName ! long_name
   character (len=64) :: elevUnits ! units
   character (len=64) :: elevStName ! Standard Name
   ! Order variable attributes
   character (len=64) :: orderLName ! long_name
   character (len=64) :: orderStName ! Standard Name
   ! Global attributes
   character (len=128) :: fType ! featureType attribute
   character (len=128) :: proj4 ! proj4 attribute
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: stDim ! station_dimension attribute
   integer :: stOrder ! stream_order_output attribute
   character (len=128) :: cdm ! cdm_datatype attribute
   character (len=1024) :: esri ! esri_pe_string attribute
   character (len=128) :: conventions ! Conventions string

end type chrtMeta

type ldasMeta
   ! Variable names
   character (len=64), dimension(numLdasVars) :: varNames
   integer :: numVars = numLdasVars
   integer :: numSnowLayers = 3
   integer :: numSoilLayers = 4
   ! Output variable attributes
   real, dimension(numLdasVars) :: scaleFactor ! scale_factor values used for each
                                                  ! variable to converte from
                                                  ! real to
                                                  ! integer.
   real, dimension(numLdasVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numLdasVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numLdasVars) :: units ! Units for each variable.
   integer, dimension(numLdasVars) :: numLev ! Number of levels for each variable.
   integer*8, dimension(numLdasVars) :: validMinComp ! Valid min (after conversion to integer)
   integer*8, dimension(numLdasVars) :: validMaxComp ! Valid max (after conversion to integer)
   real, dimension(numLdasVars) :: validMinReal ! Valid minimum (before conversion to integer)
   real, dimension(numLdasVars) :: validMaxReal ! Valid maximum (before converstion to integer)
   integer*8, dimension(numLdasVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numLdasVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numLdasVars) :: fillReal ! Fill value (before conversion to integer)
   integer*8, dimension(numLdasVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numLdasVars) :: outFlag ! 0/1 flag to turn outputting off/on 
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   ! Projection variable attributes
   character (len=64) :: yLName,xLName ! long_name
   character (len=64) :: yStName,xStName ! standard_name
   character (len=64) :: yAxisType,xAxisType ! _CoordinateAxisType
   character (len=64) :: yUnits,xUnits ! units
   real :: yRes, xRes ! Resoltution in meters
   ! Establish ProjectionCoordinateSystem variable attributes
   character (len=64) :: projTransform,projTransformName
   character (len=64) :: projGridMappingName,projCoordAxes
   character (len=2048) :: projEsri
   real, dimension(2) :: stParallel
   real*8 :: lonCtrlMer,latOrig,falseEast,falseNorth,radius
   character (len=512) :: proj4
   character (len=128) :: gridMapping

   ! Global attributes
   character (len=128) :: title ! File TITLE
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: conventions ! Conventions string

end type ldasMeta

type rtDomainMeta
   ! Variable names
   character (len=64), dimension(numRtDomainVars) :: varNames
   integer :: numVars = numRtDomainVars
   ! Output variable attributes
   real, dimension(numRtDomainVars) :: scaleFactor ! scale_factor values used for each
                                                  ! variable to converte from
                                                  ! real to
                                                  ! integer.
   real, dimension(numRtDomainVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numRtDomainVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numRtDomainVars) :: units ! Units for each variable.
   character (len=64), dimension(numRtDomainVars) :: coordNames ! Coordinate names for each variable. 
   integer*8, dimension(numRtDomainVars) :: validMinComp ! Valid min (after conversion to integer)
   integer*8, dimension(numRtDomainVars) :: validMaxComp ! Valid max (after conversion to integer)
   real, dimension(numRtDomainVars) :: validMinReal ! Valid minimum (before conversion to integer)
   real, dimension(numRtDomainVars) :: validMaxReal ! Valid maximum (before converstion to integer)
   integer*8, dimension(numRtDomainVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numRtDomainVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numRtDomainVars) :: fillReal ! Fill value (before conversion to integer)
   integer*8, dimension(numRtDomainVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numRtDomainVars) :: outFlag ! 0/1 flag to turn outputting off/on 
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970

   ! Projection variable attributes
   character (len=64) :: yLName,xLName ! long_name
   character (len=64) :: yStName,xStName ! standard_name
   character (len=64) :: yAxisType,xAxisType ! _CoordinateAxisType
   character (len=64) :: yUnits,xUnits ! units
   real :: yRes, xRes ! Resoltution in meters
   ! Establish ProjectionCoordinateSystem variable attributes
   character (len=64) :: projTransform,projTransformName
   character (len=64) :: projGridMappingName,projCoordAxes
   character (len=2048) :: projEsri
   real, dimension(2) :: stParallel
   real*8 :: lonCtrlMer,latOrig,falseEast,falseNorth,radius
   character (len=512) :: proj4
   character (len=128) :: gridMapping

   ! Global attributes
   integer :: decimation ! Decimation factor
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: conventions ! Conventions string

end type rtDomainMeta

type lakeMeta
   ! Variable names
   character (len=64), dimension(numLakeVars) :: varNames
   integer :: numVars = numLakeVars
   ! Output variable attributes
   real, dimension(numLakeVars) :: scaleFactor ! scale_factor values used for each
                                               ! variable to converte from
                                               ! real to
                                               ! integer.
   real, dimension(numLakeVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numLakeVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numLakeVars) :: units ! Units for each variable.
   character (len=64), dimension(numLakeVars) :: coordNames ! Coordinate names for each variable. 
   integer*8, dimension(numLakeVars) :: validMinComp ! Valid min (after conversion to integer)
   integer*8, dimension(numLakeVars) :: validMaxComp ! Valid max (after conversion to integer)
   real, dimension(numLakeVars) :: validMinReal ! Valid minimum (before conversion to integer)
   real, dimension(numLakeVars) :: validMaxReal ! Valid maximum (before converstion to integer)
   integer*8, dimension(numLakeVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numLakeVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numLakeVars) :: fillReal ! Fill value (before conversion to integer)
   integer*8, dimension(numLakeVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numLakeVars) :: outFlag ! 0/1 flag to turn outputting off/on 
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   ! lake_id attributes
   character (len=64) :: lakeIdLName ! long_name - usually Lake COMMON ID
   character (len=256) :: LakeIdComment ! Comment attribute
   ! feature_id attributes
   character (len=64) :: featureIdLName ! long_name - usually lake COMMON ID
   character (len=256) :: featureIdComment ! Comment attribute
   ! latitude variable attributes
   character (len=64) :: latLName ! long_name
   character (len=64) :: latUnits ! units
   character (len=64) :: latStName ! Standard Name
   ! longitude variable attributes
   character (len=64) :: lonLName ! long_name
   character (len=64) :: lonUnits ! units
   character (len=64) :: lonStName ! Standard Name
   ! Global attributes
   character (len=128) :: fType ! featureType attribute
   character (len=128) :: proj4 ! proj4 attribute
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: lakeDim ! lake_dimension attribute
   character (len=128) :: cdm ! cdm_datatype attribute
   character (len=1024) :: esri ! esri_pe_string attribute
   character (len=128) :: conventions ! Conventions string

end type lakeMeta

contains

subroutine initChrtDict(chrtOutDict)
   implicit none

   type(chrtMeta), intent(inout) :: chrtOutDict
   ! CHRTOUT FILES
   ! NOTE !!!!! If you see PLC, this means OWP has no desire to output these,
   !            which means meta-data standards have yet to be determined 
   !            for these variables. Fill in if it's desired to output....
   ! First establish global attributes for the channel output files 
   chrtOutDict%fType = 'timeSeries'
   chrtOutDict%proj4 = '+proj=longlat +datum=NAD83 +no_defs'
   chrtOutDict%initTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   chrtOutDict%validTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   chrtOutDict%stDim = 'feature_id'
   chrtOutDict%stOrder = 1
   chrtOutDict%cdm = 'Station'
   chrtOutDict%esri = 'GEOGCS[GCS_North_American_1983,DATUM[D_North_American_1983,&
                      &SPHEROID[GRS_1980,6378137.0,298.257222101]],&
                      &PRIMEM[Greenwich,0.0],UNIT[Degree,0.017453292519943295]]'
   chrtOutDict%conventions = 'CF-1.6'
   
   ! Next establish time attribues
   chrtOutDict%timeLName = 'valid output time'
   chrtOutDict%timeUnits = 'minutes since 1970-01-01 00:00:00 UTC'
   chrtOutDict%timeStName = 'time'
   chrtOutDict%rTimeLName = 'model initialization time'
   chrtOutDict%rTimeStName = 'forecast_reference_time'
   chrtOutDict%rTimeUnits = 'minutes since 1970-01-01 00:00:00 UTC'

   ! Esatablish lat/lon attributes
   chrtOutDict%latLName = "Feature latitude"
   chrtOutDict%latUnits = "degrees_north"
   chrtOutDict%latStName = "latitude"
   chrtOutDict%lonLName = "Feature longitude"
   chrtOutDict%lonUnits = "degrees_east"
   chrtOutDict%lonStName = "longitude"
  
   ! Establish streamflw order attributes
   chrtOutDict%orderLName = "Streamflow Order"
   chrtOutDict%StName = "order"

   ! Establish point elevation attributes
   chrtOutDict%elevLName = "Feature Elevation"
   chrtOutDict%elevUnits = "meters"
   chrtOutDict%elevStName = "Elevation"
 
   ! Next establish feature_id attributes
   chrtOutDict%featureIdLName = 'Reach ID'
   chrtOutDict%featureIdComment = 'NHDPlusv2 ComIDs within CONUS, arbitrary Reach IDs outside of CONUS'
   
   ! Now establish attributes for output variables.
   chrtOutDict%varNames(:) = (/"streamflow","nudge","q_lateral","velocity",&
                               "Head","QSfcLatRunoff","QBucket",&
                               "QBtmVertRunoff","AccSfcLatRunoff","accBucket"/)
   chrtOutDict%longName(:) = (/"River Flow","Amount of stream flow alteration",&
                               "Runoff into channel reach","River Velocity",&
                               "PLC","PLC","PLC",&
                               "PLC","PLC","PLC"/)
   chrtOutDict%units(:) = (/"m3 s-1","m3 s-1","m3 s-1","m s-1",&
                            "PLC","PLC","PLC",&
                            "PLC","PLC","PLC"/)
   chrtOutDict%coordNames(:) = (/"latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude"/)
   chrtOutDict%scaleFactor(:) = [0.0001,0.1,0.1,0.01,-99.0,-99.0,-99.0,-99.0,&
                                 -99.0,-99.0]
   chrtOutDict%addOffset(:) = [0.0,0.0,0.0,0.0,-99.0,-99.0,-99.0,-99.0,-99.0,&
                               -99.0]
   ! Specify the outputflags. 0 - Do not output, 1 - Output. DO NOT MODIFY
   ! WITHOUT CONSULTING WITH OWP AND NCAR!!!!!!!!!!!!!!
   chrtOutDict%outFlag(:) = [1,0,1,1,0,0,0,0,0,0] 
   chrtOutDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0]
   chrtOutDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0]
   chrtOutDict%validMinReal(:) = [0.0,-500000.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0]
   chrtOutDict%validMaxReal(:) = [500000.0,500000.0,500000.0,10000.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0]
   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numChVars
      chrtOutDict%fillComp(i) = int((chrtOutDict%fillReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
      chrtOutDict%missingComp(i) = int((chrtOutDict%missingReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
      chrtOutDict%validMinComp(i) = int((chrtOutDict%validMinReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
      chrtOutDict%validMaxComp(i) = int((chrtOutDict%validMaxReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
   end do
end subroutine initChrtDict

subroutine initLdasDict(ldasOutDict)
   implicit none

   type(ldasMeta), intent(inout) :: ldasOutDict
   ! LDASOUT FILES

   ! First establish global attributes.
   ldasOutDict%title = "OUTPUT FROM HRLDAS v20150506"
   ldasOutDict%initTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   ldasOutDict%validTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   ldasOutDict%conventions = "CF-1.6"

   ! Next establish time attributes
   ldasOutDict%timeLName = "valid output time"
   ldasOutDict%timeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   ldasOutDict%timeStName = "time"
   ldasOutDict%rTimeLName = "model initialization time"
   ldasOutDict%rTimeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   ldasOutDict%rTimeStName = "forecast_reference_time"

   ! Establish attributes for coordinate variables.
   ldasOutDict%xLName = "x coordinate of projection"
   ldasOutDict%xStName = "projection_x_coordinate"
   ldasOutDict%xAxisType = "GeoX"
   ldasOutDict%xUnits = "m"
   ldasOutDict%xRes = 1000.0
   ldasOutDict%yLName = "y coordinate of projection"
   ldasOutDict%yStName = "projection_y_coordinate"
   ldasOutDict%yAxisType = "GeoY"
   ldasOutDict%yUnits = "m"
   ldasOutDict%yRes = 1000.0

   ! Establish attributes for the ProjectionCoordinateSystem variable.
   ldasOutDict%projTransform = "Projection"
   ldasOutDict%projTransformName = "lambert_conformal_conic"
   ldasOutDict%projGridMappingName= "lambert_conformal_conic"
   ldasOutDict%projCoordAxes = "y x"
   ldasOutDict%projEsri = 'PROJCS[\"Sphere_Lambert_Conformal_Conic\",GEOGCS[\"GCS_Sphere\",&
                          &DATUM[\"D_Sphere\",SPHEROID[\"Sphere\",6370000.0,0.0]]&
                          &,PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],&
                          &PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"false_easting\",0.0],&
                          &PARAMETER[\"false_northing\",0.0],PARAMETER[\"central_meridian\",-97.0],&
                          &PARAMETER[\"standard_parallel_1\",30.0],PARAMETER[\"standard_parallel_2\",60.0],&
                          &PARAMETER[\"latitude_of_origin\",40.0000076294],UNIT[\"Meter\",1.0]];&
                          &-35691800 -29075200 126180232.640845;-100000 10000;-100000&
                          & 10000;0.001;0.001;0.001;IsHighPrecision'
   ldasOutDict%stParallel = [30.0,60.0]
   ldasOutDict%lonCtrlMer = -97.0
   ldasOutDict%latOrig = 40.0000076294
   ldasOutDict%falseEast = 0.0
   ldasOutDict%falseNorth = 0.0
   ldasOutDict%radius = 6370000.0
   ldasOutDict%proj4 = '+proj=lcc +lat_1=30 +lat_2=60 +lat_0=40 +lon_0=-97 +x_0=0&
                       & +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs'
   ldasOutDict%gridMapping = "ProjectionCoordinateSystem"

   ! Now establish metadata attributes for variables. ESRI string is defined
   ! above and applies to all gridded variables for LDASOUT.
   ldasOutDict%varNames(:) = (/"IVGTYP","ISLTYP","FVEG","LAI","SAI","SWFORC",&
                               "COSZ","LWFORC","RAINRATE","EMISS","FSA","FIRA",&
                               "GRDFLX","HFX","LH","ECAN","EDIR","ALBEDO","ETRAN",&
                               "UGDRNOFF","SFCRNOFF","CANLIQ","CANICE","ZWT","WA",&
                               "WT","ACCPRCP","ACCECAN","ACCEDIR","ACCETRAN","SAV",&
                               "TR","EVC","IRC","SHC","IRG","SHG","EVG","GHV","SAG",&
                               "IRB","SHB","EVB","GHB","TRAD","TG","TV","TAH","TGV",&
                               "TGB","T2MV","T2MB","Q2MV","Q2MB","EAH","FWET",&
                               "ZSNSO_SN","SNICE","SNLIQ","SOIL_T","SOIL_W","SNOW_T",&
                               "SOIL_M","SNOWH","SNEQV","QSNOW","ISNOW","FSNO",&
                               "ACSNOW","ACSNOM","CM","CH","CHV","CHB","CHLEAF",&
                               "CHUC","CHV2","CHB2","LFMASS","RTMASS","STMASS",&
                               "WOOD","STBLCP","FASTCP","NEE","GPP","NPP","PSN",&
                               "APAR","ACCET","CANWAT","SOILICE","SOILSAT_TOP",&
                               "SOILSAT","SNOWT_AVG"/)
   ldasOutDict%longName(:) = (/"Dominant vegetation category",&
                               "Dominant soil category",&
                               "Green Vegetation Fraction",&
                               "Leaf area index",&
                               "Stem area index",&
                               "Shortwave forcing",&
                               "Cosine of zenith angle",&
                               "Longwave forcing",&
                               "Precipitation rate",&
                               "Grid emissivity",&
                               "Total absorved SW radiation",&
                               "Total net LW radiation to atmosphere",&
                               "Heat flux into the soil",&
                               "Total sensible heat to the atmosphere",&
                               "Total latent heat to the atmosphere",&
                               "Canopy water evaporation rate",&
                               "Direct from soil evaporation rate",&
                               "Surface albedo",&
                               "Transpiration rate",&
                               "Accumulated underground runoff",&
                               "Accumulated surface runoff",&
                               "Canopy liquid water content",&
                               "Canopy ice water content",&
                               "Depth to water table",&
                               "Water in aquifer",&
                               "Water in aquifer and saturated soil",&
                               "Accumulated precip",&
                               "Accumulated canopy evap",&
                               "Accumulated direct soil evap",&
                               "Accumulated transpiration",&
                               "Solar radiative heat flux absorbed by vegetation",&
                               "Transpiration heat",&
                               "Canopy evap heat",&
                               "Canopy net LW rad",&
                               "Canopy sensible heat",&
                               "Ground net LW rad",&
                               "Ground sensible heat",&
                               "Ground evap heat",&
                               "Ground heat flux + to soil vegetated",&
                               "Solar radiative heat flux absorbed by ground",&
                               "Net LW rad to atm bare",&
                               "Sensible heat atm bare",&
                               "Evaporation heat to atm bare",&
                               "Ground heat flux + to soil bare",&
                               "Surface radiative temperature",&
                               "Ground temperature",&
                               "Vegetation temperature",&
                               "Canopy air temperature",&
                               "Ground surface Temp vegetated",&
                               "Ground surface Temp bare",&
                               "2m Air Temp vegetated",&
                               "2m Air Temp bare",&
                               "2m mixing ratio vegetated",&
                               "2m mixing ratio bare",&
                               "Canopy air vapor pressure",&
                               "Wetted or snowed fraction of canopy",&
                               "Snow layer depths from snow surface",&
                               "Snow layer ice",&
                               "Snow layer liquid water",&
                               "soil temperature",&
                               "liquid volumetric soil moisture",&
                               "snow temperature",&
                               "volumetric soil moisture",&
                               "Snow depth",&
                               "Snow water equivalent",&
                               "Snowfall rate",&
                               "Number of snow layers",&
                               "Snow-cover fraction on the ground",&
                               "accumulated snow fall",&
                               "accumulated melting water out of snow bottom",&
                               "Momentum drag coefficient",&
                               "Sensible heat exchange coefficient",&
                               "Exchange coefficient vegetated",&
                               "Exchange coefficient bare",&
                               "Exchange coefficient leaf",&
                               "Exchange coefficient bare",&
                               "Exchange coefficient 2-meter vegetated",&
                               "Exchange coefficient 2-meter bare",&
                               "Leaf mass",&
                               "Mass of fine roots",&
                               "Stem mass",&
                               "Mass of wood and woody roots",&
                               "Stable carbon in deep soil",&
                               "Short-lived carbon in shallow soil",&
                               "Net ecosystem exchange",&
                               "Net instantaneous assimilation",&
                               "Net primary productivity",&
                               "Total photosynthesis",&
                               "Photosynthesis active energy by canopy",&
                               "Accumulated total ET",&
                               "Total canopy water (liquid + ice)",&
                               "fraction of soil moisture that is ice",&
                               "fraction of soil saturation, top 2 layers",&
                               "fraction of soil saturation, column integrated",&
                               "average snow temperature (by layer mass)"/)
   ldasOutDict%units(:) = (/"category","category","-","-","-",&
                            "W m-2","W m-2","W m-2","kg m-2s-1",&
                            "-","W m-2","W m-2","W m-2","W m-2","W m-2","kg m-2s-1","kg m-2s-1","-","kg m-2s-1",&
                            "mm","mm","mm","mm","m","kg m-2","kg m-2",&
                            "mm","mm","mm","mm",&
                            "W m-2","W m-2","W m-2","W m-2","W m-2",&
                            "W m-2","W m-2","W m-2","W m-2",&
                            "W m-2","W m-2","W m-2","W m-2","W m-2",&
                            "K","K","K","K","K","K","K","K",&
                            "kg/kg","kg/kg","Pa","fraction",&
                            "m","mm","mm","K","m3 m-3","K","m3 m-3",&
                            "m","kg m-2","mm s-1","count","-","mm","mm",&
                            "-","-","m s-1","m s-1","m s-1","m s-1","m s-1","m s-1",&
                            "g m-2","g m-2","g m-2","g m-2","g m-2","g m-2","g m-2s-1 CO2",&
                            "g m-2s-1 C","g m-2s-1 C","umol CO m-2 s-1","W m-2",&
                            "mm","mm","-","-","-","K"/)
   ldasOutDict%scaleFactor(:) = [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                 0.1,0.1,0.1,0.1,0.1,1.0,1.0,1.0,1.0,0.01,&
                                 0.001,1.0,1.0,1.0,1.0,1.0,1.0,0.01,0.01,0.01,&
                                 1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                 1.0,1.0,1.0,1.0,0.1,1.0,1.0,1.0,1.0,1.0,&
                                 1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.1,0.1,&
                                 1.0,1.0,0.01,0.001,0.1,1.0,1.0,0.001,1.0,0.1,&
                                 1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                 1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.01,&
                                 0.01,0.01,0.001,0.001,0.1] 
   ldasOutDict%addOffset(:) = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0,0.0,0.0,0.0]
   ! Note that output flags will be set in the the output routine, and will vary
   ! by the IOC flag specified in hydro.namelist.
   ldasOutDict%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
   ldasOutDict%numLev(:) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,4,&
                            4,3,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
   ldasOutDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0]
   ldasOutDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0]
   ldasOutDict%validMinReal(:) = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0]
   ldasOutDict%validMaxReal(:) = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0]
   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numLdasVars
      ldasOutDict%fillComp(i) = int((ldasOutDict%fillReal(i)+ldasOutDict%addOffset(i))/ldasOutDict%scaleFactor(i))
      ldasOutDict%missingComp(i) = int((ldasOutDict%missingReal(i)+ldasOutDict%addOffset(i))/ldasOutDict%scaleFactor(i))
      ldasOutDict%validMinComp(i) = int((ldasOutDict%validMinReal(i)+ldasOutDict%addOffset(i))/ldasOutDict%scaleFactor(i))
      ldasOutDict%validMaxComp(i) = int((ldasOutDict%validMaxReal(i)+ldasOutDict%addOffset(i))/ldasOutDict%scaleFactor(i))
   end do

end subroutine initLdasDict

subroutine initRtDomainDict(rtDomainDict)
   implicit none

   type(rtDomainMeta), intent(inout) :: rtDomainDict
   ! RT_DOMAIN files

   ! First establish global attributes.
   rtDomainDict%initTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   rtDomainDict%validTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   rtDomainDict%decimation = 1
   rtDomainDict%conventions = "CF-1.6"

   ! Next establish time attributes
   rtDomainDict%timeLName = "valid output time"
   rtDomainDict%timeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   rtDomainDict%timeStName = "time"
   rtDomainDict%rTimeLName = "model initialization time"
   rtDomainDict%rTimeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   rtDomainDict%rTimeStName = "forecast_reference_time"

   ! Establish attributes for coordinate variables.
   rtDomainDict%xLName = "x coordinate of projection"
   rtDomainDict%xStName = "projection_x_coordinate"
   rtDomainDict%xAxisType = "GeoX"
   rtDomainDict%xUnits = "m"
   rtDomainDict%xRes = 250.0
   rtDomainDict%yLName = "y coordinate of projection"
   rtDomainDict%yStName = "projection_y_coordinate"
   rtDomainDict%yAxisType = "GeoY"
   rtDomainDict%yUnits = "m"
   rtDomainDict%yRes = 250.0

   ! Establish attributes for the ProjectionCoordinateSystem variable.
   rtDomainDict%projTransform = "Projection"
   rtDomainDict%projTransformName = "lambert_conformal_conic"
   rtDomainDict%projGridMappingName= "lambert_conformal_conic"
   rtDomainDict%projCoordAxes = "y x"
   rtDomainDict%projEsri = 'PROJCS[\"Sphere_Lambert_Conformal_Conic\",GEOGCS[\"GCS_Sphere\",&
                            &DATUM[\"D_Sphere\",SPHEROID[\"Sphere\",6370000.0,0.0]]&
                            &,PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],&
                            &PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"false_easting\",0.0],&
                            &PARAMETER[\"false_northing\",0.0],PARAMETER[\"central_meridian\",-97.0],&
                            &PARAMETER[\"standard_parallel_1\",30.0],PARAMETER[\"standard_parallel_2\",60.0],&
                            &PARAMETER[\"latitude_of_origin\",40.0000076294],UNIT[\"Meter\",1.0]];&
                            &-35691800 -29075200 126180232.640845;-100000 10000;-100000&
                            & 10000;0.001;0.001;0.001;IsHighPrecision'
   rtDomainDict%stParallel = [30.0,60.0]
   rtDomainDict%lonCtrlMer = -97.0
   rtDomainDict%latOrig = 40.0000076294
   rtDomainDict%falseEast = 0.0
   rtDomainDict%falseNorth = 0.0
   rtDomainDict%radius = 6370000.0
   rtDomainDict%proj4 = '+proj=lcc +lat_1=30 +lat_2=60 +lat_0=40 +lon_0=-97 +x_0=0&
                         & +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs'
   rtDomainDict%gridMapping = "ProjectionCoordinateSystem"

   rtDomainDict%varNames(:) = (/"zwattablrt","sfcheadsubrt","QSTRMVOLRT",&
                                "QBDRYRT"/)
   rtDomainDict%longName(:) = (/"water table depth","surface head",&
                                 "channel inflow",&
                                 "accumulated value of the boundary flux, + into domain - out of domain"/)
   rtDomainDict%units(:) = (/"m","mm","mm","mm"/)
   rtDomainDict%scaleFactor(:) = [0.1,1.0,1.0,1.0]
   rtDomainDict%addOffset(:) = [0.0,0.0,0.0,0.0]
   rtDomainDict%outFlag(:) = [0,0,0,0]
   rtDomainDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0]
   rtDomainDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0]
   rtDomainDict%validMinReal(:) = [0.0,0.0,0.0,0.0]
   rtDomainDict%validMaxReal(:) = [10.0,1000000.0,1000.0,1000.0]

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numRtDomainVars
      rtDomainDict%fillComp(i) = int((rtDomainDict%fillReal(i)+rtDomainDict%addOffset(i))/rtDomainDict%scaleFactor(i))
      rtDomainDict%missingComp(i) = int((rtDomainDict%missingReal(i)+rtDomainDict%addOffset(i))/rtDomainDict%scaleFactor(i))
      rtDomainDict%validMinComp(i) = int((rtDomainDict%validMinReal(i)+rtDomainDict%addOffset(i))/rtDomainDict%scaleFactor(i))
      rtDomainDict%validMaxComp(i) = int((rtDomainDict%validMaxReal(i)+rtDomainDict%addOffset(i))/rtDomainDict%scaleFactor(i))
   end do

end subroutine initRtDomainDict

subroutine initLakeDict(lakeOutDict)
   implicit none

   type(lakeMeta), intent(inout) :: lakeOutDict
   ! LAKE FILES
   ! NOTE !!!!! If you see PLC, this means OWP has no desire to output these,
   !            which means meta-data standards have yet to be determined 
   !            for these variables. Fill in if it's desired to output....
   ! First establish global attributes for the channel output files 
   lakeOutDict%fType = 'timeSeries'
   lakeOutDict%proj4 = '+proj=longlat +datum=NAD83 +no_defs'
   lakeOutDict%initTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   lakeOutDict%validTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   lakeOutDict%stDim = 'lake_id'
   lakeOutDict%cdm = 'PLACEHOLDER'
   lakeOutDict%esri = 'GEOGCS[GCS_North_American_1983,DATUM[D_North_American_1983,&
                      &SPHEROID[GRS_1980,6378137.0,298.257222101]],&
                      &PRIMEM[Greenwich,0.0],UNIT[Degree,0.017453292519943295]]'
   lakOutDict%conventions = 'CF-1.6'

   ! Next establish time attribues
   lakOutDict%timeLName = 'valid output time'
   lakOutDict%timeUnits = 'minutes since 1970-01-01 00:00:00 UTC'
   lakeOutDict%timeStName = 'time'
   lakeOutDict%rTimeLName = 'model initialization time'
   lakeOutDict%rTimeStName = 'forecast_reference_time'
   lakeOutDict%rTimeUnits = 'minutes since 1970-01-01 00:00:00 UTC'

   ! Establish elevation variable attributes
   lakeOutDict%elevLName = "Water Surface Elevation"
   lakeOutDict%elevUnits = "meters"

   ! Establish feature_id attributes
   lakeOutDict%featureIdLName = "Lake COMMOND ID"
   lakeOutDict%featureIdComment = "ComID from NHDPlusV2 waterbody layer"

   ! Esatablish lat/lon attributes
   lakeOutDict%latLName = "Lake latitude"
   lakeOutDict%latUnits = "degrees_north"
   lakeOutDict%latStName = "latitude"
   lakeOutDict%lonLName = "Lake longitude"
   lakeOutDict%lonUnits = "degrees_east"
   lakeOutDict%lonStName = "longitude"

   lakeOutDict%varNames(:) = (/'inflow','outflow'/)
   lakeOutDict%longNames(:) = (/'Lake Inflow','Lake Outflow'/)
   lakeOutDict%units(:) = (/'m3 s-1','m3 s-1'/)
   lakeOutDict%coordNames(:) = (/'latitude longitude','latitude longitude'/)
   lakeOutDict%scaleFactor(:) = [0.01,0.01]
   lakeOutDict%addOffset(:) = [0.0,0.0]
   lakeOutDict%outFlag(:) = [0,0]
   lakeOutDict%fillReal(:) = [-9999.0,-9999.0]
   lakeOutDict%missingReal(:) = [-9999.0,-9999.0]
   lakeOutDict%validMinReal(:) = [-10000.0,-10000.0]
   lakeOutDict%validMaxreal(:) = [10000.0,10000.0]

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numLakeVars
      lakeOutDict%fillComp(i) = int((lakeOutDict%fillReal(i)+lakeOutDict%addOffset(i))/lakeOutDict%scaleFactor(i))
      lakeOutDict%missingComp(i) = int((lakeOutDict%missingReal(i)+lakeOutDict%addOffset(i))/lakeOutDict%scaleFactor(i))
      lakeOutDict%validMinComp(i) = int((lakeOutDict%validMinReal(i)+lakeOutDict%addOffset(i))/lakeOutDict%scaleFactor(i))
      lakeOutDict%validMaxComp(i) = int((lakeOutDict%validMaxReal(i)+lakeOutDict%addOffset(i))/lakeOutDict%scaleFactor(i))
   end do

end subroutine initLakeDict

end module module_NWM_io_dict

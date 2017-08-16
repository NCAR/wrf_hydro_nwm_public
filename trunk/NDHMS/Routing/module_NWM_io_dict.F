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
integer, parameter :: numChGrdVars = 1
integer, parameter :: numLsmVars = 14
integer :: i

! Declare public types that will hold metadata
public :: chrtMeta ! Public CHRTOUT metadata for NWM output.
public :: ldasMeta ! Public LDASOUT metadata for NWM output.
public :: rtDomainMeta ! Public RT_DOMAIN metadata for NWM output. 
public :: lakeMeta ! Public lake metadata for NWM output
public :: chrtGrdMeta ! Public CHRTOUT_GRID metadata for NWM output.
public :: lsmMeta ! Public metadata for LSMOUT output.

! Establish types for each output type
type chrtMeta
   ! Variable names
   character (len=64), dimension(numChVars) :: varNames
   integer :: numVars = numChVars
   character (len=512) :: outVersion = "NWM 1.2"
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
   integer, dimension(numChVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output 
                                                 ! the actual data. This was done because time 0 
                                                 ! output does not all contain valid data. 
   real :: modelNdv ! NDV value represented within the model code
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
   character (len=64) :: cfRole ! cf_role attribute
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
   character (len=512) :: outVersion = "NWM 1.2"
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
   integer, dimension(numLdasVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output 
                                                   ! the actual data. This was done because time 0 
                                                   ! output does not all contain valid data. 
   real :: modelNdv ! NDV value represented within the model code
   real :: modelNdv2 ! Alternative NDV value in NoahMP
   real :: modelNdv3 ! Alternative NDV value in NoahMP
   integer :: modelNdvInt ! NDV value represented in model as integer
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
   character (len=512) :: outVersion = "NWM 1.2"
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
   integer, dimension(numRtDomainVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output 
                                                       ! the actual data. This was done because time 0 
                                                       ! output does not all contain valid data. 
   real :: modelNdv ! NDV value represented within the model code
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
   character (len=512) :: outVersion = "NWM 1.2"
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
   integer, dimension(numLakeVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output 
                                                   ! the actual data. This was done because time 0 
                                                   ! output does not all contain valid data. 
   real :: modelNdv ! NDV value represented within the model code
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
   character (len=64) :: cfRole ! cf_role attribute
   ! latitude variable attributes
   character (len=64) :: latLName ! long_name
   character (len=64) :: latUnits ! units
   character (len=64) :: latStName ! Standard Name
   ! longitude variable attributes
   character (len=64) :: lonLName ! long_name
   character (len=64) :: lonUnits ! units
   character (len=64) :: lonStName ! Standard Name
   ! elevation variable attributes
   character (len=64) :: elevLName ! long_name
   character (len=64) :: elevUnits ! units
   character (len=64) :: elevStName ! Standard Name
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

type chrtGrdMeta
   ! Variable names
   character (len=64), dimension(numChGrdVars) :: varNames
   integer :: numVars = numChGrdVars
   character (len=512) :: outVersion = "NWM v1.2"
   ! Output variable attributes
   real, dimension(numChGrdVars) :: scaleFactor ! scale_factor values for each
                                                ! variable to convert from 
                                                ! real to integer.
   real, dimension(numChGrdVars) :: addOffset ! add_offset values for each variable.
   character (len=64), dimension(numChGrdVars) :: longName ! longname for each variable
   character (len=64), dimension(numChGrdVars) :: units ! Units for each variable.
   character (len=64), dimension(numChGrdVars) :: coordNames ! Coordinate names for each variable. 
   integer*8, dimension(numChGrdVars) :: validMinComp ! Valid min (after conversion to integer)
   integer*8, dimension(numChGrdVars) :: validMaxComp ! Valid max (after conversion to integer)
   real, dimension(numChGrdVars) :: validMinReal ! Valid minimum (before conversion to integer)
   real, dimension(numChGrdVars) :: validMaxReal ! Valid maximum (before converstion to integer)
   integer*8, dimension(numChGrdVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numChGrdVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numChGrdVars) :: fillReal ! Fill value (before conversion to integer)
   integer*8, dimension(numChGrdVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numChGrdVars) :: outFlag ! 0/1 flag to turn outputting off/on 
   integer, dimension(numChGrdVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output 
                                                       ! the actual data. This
                                                       ! was done because time 0 
                                                       ! output does not all
                                                       ! contain valid data. 
   real :: modelNdv ! NDV value represented within the model code
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

end type chrtGrdMeta

type lsmMeta
   ! Variable names
   character (len=64), dimension(numLsmVars) :: varNames
   integer :: numVars = numLsmVars
   integer :: numSnowLayers = 3
   integer :: numSoilLayers = 4
   character (len=512) :: outVersion = "NWM 1.2"
   ! Output variable attributes
   real, dimension(numLsmVars) :: scaleFactor ! scale_factor values used for each
                                                  ! variable to converte from
                                                  ! real to
                                                  ! integer.
   real, dimension(numLsmVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numLsmVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numLsmVars) :: units ! Units for each variable.
   integer, dimension(numLsmVars) :: numLev ! Number of levels for each variable.
   integer*8, dimension(numLsmVars) :: validMinComp ! Valid min (after conversion to integer)
   integer*8, dimension(numLsmVars) :: validMaxComp ! Valid max (after conversion to integer)
   real, dimension(numLsmVars) :: validMinReal ! Valid minimum (before conversion to integer)
   real, dimension(numLsmVars) :: validMaxReal ! Valid maximum (before converstion to integer)
   integer*8, dimension(numLsmVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numLsmVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numLsmVars) :: fillReal ! Fill value (before conversion to integer)
   integer*8, dimension(numLsmVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numLsmVars) :: outFlag ! 0/1 flag to turn outputting off/on 
   integer, dimension(numLsmVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output 
                                                   ! the actual data. This was
                                                   ! done because time 0 
                                                   ! output does not all contain
                                                   ! valid data. 
   real :: modelNdv ! NDV value represented within the model code
   integer :: modelNdvInt ! NDV value represented in model as integer
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

end type lsmMeta

contains

subroutine initChrtDict(chrtOutDict)
   !use module_namelist, only: nlst_rt
   !use netcdf
   implicit none

   type(chrtMeta), intent(inout) :: chrtOutDict

   chrtOutDict%modelNdv = -9.E15
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
   chrtOutDict%orderStName = "order"

   ! Establish point elevation attributes
   chrtOutDict%elevLName = "Feature Elevation"
   chrtOutDict%elevUnits = "meters"
   chrtOutDict%elevStName = "Elevation"
 
   ! Next establish feature_id attributes
   chrtOutDict%featureIdLName = 'Reach ID'
   chrtOutDict%featureIdComment = 'NHDPlusv2 ComIDs within CONUS, arbitrary Reach IDs outside of CONUS'
   chrtOutDict%cfRole = 'timeseries_id'   

   ! Now establish attributes for output variables.
   !chrtOutDict%varNames(:) = (/"streamflow","nudge","q_lateral","velocity",&
   !                            "Head","qSfcLatRunoff","qBucket",&
   !                            "qBtmVertRunoff","AccSfcLatRunoff","accBucket"/)
   chrtOutDict%varNames(:) = [character(len=64) :: "streamflow","nudge","q_lateral","velocity",&
                              "Head","qSfcLatRunoff","qBucket",&
                              "qBtmVertRunoff","AccSfcLatRunoff","accBucket"]
   chrtOutDict%longName(:) = [character(len=64) :: "River Flow","Amount of stream flow alteration",&
                              "Runoff into channel reach","River Velocity",&
                              "River Stage","Runoff from terrain routing",&
                              "Flux from gw bucket",&
                              "Runoff from bottom of soil to bucket",&
                              "Accumulated runoff from terrain routing",&
                              "Accumulated runoff from gw bucket"]
   chrtOutDict%units(:) = [character(len=64) :: "m3 s-1","m3 s-1","m3 s-1","ms-1",&
                                                "meter","m3","m3",&
                                                "m3","m3","m3"]
   chrtOutDict%units(:) = [character(len=64) :: "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude"]
   chrtOutDict%scaleFactor(:) = [0.01,0.01,0.1,0.01,0.01,0.001,0.001,0.001,&
                                 0.01,0.01]
   chrtOutDict%addOffset(:) = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0]
   ! Initialize all output flags to 0. Modify (if absolutely necessary) in the
   ! output subroutine. 
   chrtOutDict%outFlag(:) = [0,0,0,0,0,0,0,0,0,0]
   chrtOutDict%timeZeroFlag(:) = [1,1,1,1,1,1,1,1,1,1]
   chrtOutDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0]
   chrtOutDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0]
   chrtOutDict%validMinReal(:) = [0.0,-500000.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0]
   chrtOutDict%validMaxReal(:) = [500000.0,500000.0,500000.0,500000.0,500000.0,&
                                  500000.0,500000.0,500000.0,500000.0,500000.0]
   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numChVars
      chrtOutDict%fillComp(i) = int((chrtOutDict%fillReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
      chrtOutDict%missingComp(i) = int((chrtOutDict%missingReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
      chrtOutDict%validMinComp(i) = int((chrtOutDict%validMinReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
      chrtOutDict%validMaxComp(i) = int((chrtOutDict%validMaxReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
   end do
end subroutine initChrtDict

subroutine initLdasDict(ldasOutDict,procId,diagFlag)
   use module_namelist, only: nlst_rt
   use netcdf
   implicit none

   type(ldasMeta), intent(inout) :: ldasOutDict
   integer, intent(inout) :: procId
   integer, intent(inout) :: diagFlag
   integer :: ftnMeta,projVarId,xVarId,yVarId
   integer :: iret

   ! LDASOUT FILES

   ldasOutDict%modelNdv = 9.9692099683868690E36
   ldasOutDict%modelNdv2 = -1.E33
   ldasOutDict%modelNdv3 = -1.E36
   ldasOutDict%modelNdvInt = -2147483647

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

   ! Pull spatial metadata information about the modeling domain from the land
   ! spatial metadata file. 
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst_rt(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid.
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open LAND spatial metadata file.')
      else
         ! First pull metadata on coordinate system.
         iret = nf90_inq_varid(ftnMeta,'ProjectionCoordinateSystem',projVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find ProjectionCoordinateSystem in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'_CoordinateTransformType',ldasOutDict%projTransform)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find _CoordinateTransformType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'transform_name',ldasOutDict%projTransformName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find transform_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'grid_mapping_name',ldasOutDict%projGridMappingName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find grid_mapping_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'_CoordinateAxes',ldasOutDict%projCoordAxes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find _CoordinateAxes in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'standard_parallel',ldasOutDict%stParallel)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find standard_parallel in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'longitude_of_central_meridian',ldasOutDict%lonCtrlMer)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find longitude_of_central_meridian in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'latitude_of_projection_origin',ldasOutDict%latOrig)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find latitude_of_projection_origin in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'false_easting',ldasOutDict%falseEast)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find false_easting in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'false_northing',ldasOutDict%falseNorth)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find false_northing in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'earth_radius',ldasOutDict%radius)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find earth_radius in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'esri_pe_string',ldasOutDict%projEsri)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find esri_pe_string in LAND spatial metadata file.')
         iret = nf90_get_att(ftnMeta,projVarId,'proj4',ldasOutDict%proj4)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find proj4 in LAND spatial metadata file.')
         ! Next pull metadata on x coordinate.
         iret = nf90_inq_varid(ftnMeta,'x',xVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'standard_name',ldasOutDict%xStName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x standard_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'long_name',ldasOutDict%xLName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x long_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'_CoordinateAxisType',ldasOutDict%xAxisType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x _CoordinateAxisType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'units',ldasOutDict%xUnits)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x units in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'resolution',ldasOutDict%xRes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x resolution in LAND spatial metadata file')
         ! Next pull metadata on y coordinates.
         iret = nf90_inq_varid(ftnMeta,'y',yVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find y variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'standard_name',ldasOutDict%yStName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y standard_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'long_name',ldasOutDict%yLName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y long_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'_CoordinateAxisType',ldasOutDict%yAxisType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y _CoordinateAxisType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'units',ldasOutDict%yUnits)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y units in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'resolution',ldasOutDict%yRes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y resolution in LAND spatial metadata file')
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')
      endif
   endif

   ldasOutDict%gridMapping = "ProjectionCoordinateSystem"

   ! Now establish metadata attributes for variables. ESRI string is defined
   ! above and applies to all gridded variables for LDASOUT.
   ldasOutDict%varNames(:) = [character(len=64) :: "IVGTYP","ISLTYP","FVEG","LAI","SAI","SWFORC",&
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
                               "SOILSAT","SNOWT_AVG"]
   ldasOutDict%longName(:) = [character(len=64) :: "Dominant vegetation category",&
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
                              "average snow temperature (by layer mass)"]
   ldasOutDict%units(:) = [character(len=64) :: "category","category","-","-","-",&
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
                            "mm","mm","fraction","fraction","fraction","K"]
   ldasOutDict%scaleFactor(:) = [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                 0.1,0.1,0.1,0.1,0.1,1.0,1.0,1.0,1.0,0.01,&
                                 0.001,1.0,1.0,1.0,1.0,1.0,1.0,0.01,0.01,0.01,&
                                 1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                 1.0,1.0,1.0,1.0,0.1,1.0,1.0,1.0,1.0,1.0,&
                                 1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.1,0.1,&
                                 0.01,1.0,0.01,0.001,0.1,1.0,1.0,0.001,1.0,0.1,&
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
   ldasOutDict%timeZeroFlag(:) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
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
   ldasOutDict%validMinReal(:) = [0.0,0.0,0.0,0.0,0.0,-1000.0,-1500.0,-1500.0,0.0,-1500.0,&
                                  -1500.0,-1500.0,-1500.0,-1500.0,-1500.0,-100.0,-100.0,0.0,-100.0,-5.0,&
                                  0.0,-5.0,-5.0,-1000.0,-1000.0,-1000.0,0.0,-5.0,-5.0,-5.0,&
                                  -1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,&
                                  -1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,&
                                  -1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-5.0,-5.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,-10.0,0.0,0.0,0.0,&
                                  -1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-1000.0,&
                                  -5.0,0.0,0.0,0.0,0.0]
   ldasOutDict%validMaxReal(:) = [100.0,100.0,100.0,100.0,100.0,3000.0,1500.0,1500.0,1000.0,1500.0,&
                                  1500.0,1500.0,1500.0,1500.0,1500.0,1500.0,1500.0,100.0,1500.0,30000.0,&
                                  30000.0,30000.0,30000.0,1000.0,1000.0,1000.0,100000.0,30000.0,30000.0,30000.0,&
                                  1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,&
                                  1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,&
                                  1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,30000.0,30000.0,400.0,&
                                  1.0,400.0,1.0,100000.0,100000.0,100000.0,10.0,1.0,100000.0,100000.0,&
                                  1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,&
                                  1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,10000.0,&
                                  30000.0,1.0,1.0,1.0,400.0]
   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numLdasVars
      ldasOutDict%fillComp(i) = NINT((ldasOutDict%fillReal(i)+ldasOutDict%addOffset(i))/ldasOutDict%scaleFactor(i))
      ldasOutDict%missingComp(i) = NINT((ldasOutDict%missingReal(i)+ldasOutDict%addOffset(i))/ldasOutDict%scaleFactor(i))
      ldasOutDict%validMinComp(i) = NINT((ldasOutDict%validMinReal(i)+ldasOutDict%addOffset(i))/ldasOutDict%scaleFactor(i))
      ldasOutDict%validMaxComp(i) = NINT((ldasOutDict%validMaxReal(i)+ldasOutDict%addOffset(i))/ldasOutDict%scaleFactor(i))
   end do

end subroutine initLdasDict

subroutine initRtDomainDict(rtDomainDict,procId,diagFlag)
   use module_namelist, only: nlst_rt
   use netcdf
   implicit none

   type(rtDomainMeta), intent(inout) :: rtDomainDict
   integer, intent(inout) :: procId,diagFlag
   integer :: ftnMeta,projVarId,xVarId,yVarId,ftnGeo
   integer :: xDimId,numColLand,numColHydro
   real :: resLand,resHydro,aggFactor
   integer :: iret
   ! RT_DOMAIN files

   rtDomainDict%modelNdv = -9.E15

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

   ! Pull spatial metadata information about the modeling domain from the land
   ! spatial metadata file. 
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst_rt(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid.
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open LAND spatial metadata file.')
      else
         ! First pull metadata on coordinate system.
         iret = nf90_inq_varid(ftnMeta,'ProjectionCoordinateSystem',projVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find ProjectionCoordinateSystem in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'_CoordinateTransformType',rtDomainDict%projTransform)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find _CoordinateTransformType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'transform_name',rtDomainDict%projTransformName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find transform_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'grid_mapping_name',rtDomainDict%projGridMappingName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find grid_mapping_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'_CoordinateAxes',rtDomainDict%projCoordAxes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find _CoordinateAxes in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'standard_parallel',rtDomainDict%stParallel)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find standard_parallel in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'longitude_of_central_meridian',rtDomainDict%lonCtrlMer)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find longitude_of_central_meridian in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'latitude_of_projection_origin',rtDomainDict%latOrig)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find latitude_of_projection_origin in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'false_easting',rtDomainDict%falseEast)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find false_easting in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'false_northing',rtDomainDict%falseNorth)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find false_northing in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'earth_radius',rtDomainDict%radius)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find earth_radius in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'esri_pe_string',rtDomainDict%projEsri)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find esri_pe_string in LAND spatial metadata file.')
         iret = nf90_get_att(ftnMeta,projVarId,'proj4',rtDomainDict%proj4)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find proj4 in LAND spatial metadata file.')
         ! Next, get the number of columns on the land grid
         iret = nf90_inq_dimid(ftnMeta,'x',xDimId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find the x dimension in the LAND spatial metadata file')
         iret = nf90_inquire_dimension(ftnMeta,xDimId,len=numColLand)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to retrieve the x-dimension value in the LAND spatial metadata file')
         ! Next pull metadata on x coordinate.
         iret = nf90_inq_varid(ftnMeta,'x',xVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'standard_name',rtDomainDict%xStName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x standard_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'long_name',rtDomainDict%xLName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x long_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'_CoordinateAxisType',rtDomainDict%xAxisType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x _CoordinateAxisType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'units',rtDomainDict%xUnits)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x units in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'resolution',resLand)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x resolution in LAND spatial metadata file')
         ! Next pull metadata on y coordinates.
         iret = nf90_inq_varid(ftnMeta,'y',yVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find y variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'standard_name',rtDomainDict%yStName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y standard_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'long_name',rtDomainDict%yLName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y long_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'_CoordinateAxisType',rtDomainDict%yAxisType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y _CoordinateAxisType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'units',rtDomainDict%yUnits)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y units in LAND spatial metadata file')
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')
      endif

      ! Next get the number of columns on the high-resolution routing grid.
      ! This will be used to calculate the resolution of the routing grid in
      ! meters.
      iret = nf90_open(trim(nlst_rt(1)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnGeo)
      if(iret .ne. 0) then
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open Fulldom file')
      else
         iret = nf90_inq_dimid(ftnGeo,'x',xDimId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x dimension in Fulldom file')
         iret = nf90_inquire_dimension(ftnGeo,xDimId,len=numColHydro)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to retrieve number of columns in the Fulldom file')
         iret = nf90_close(ftnGeo)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close the Fulldom file')
      endif

      ! Calculate the aggregation factor and resolution of the hydro routing
      ! grid.
      aggFactor = float(numColHydro/numColLand)
      resHydro = resLand/aggFactor

      rtDomainDict%xRes = resHydro
      rtDomainDict%yRes = resHydro

   endif
   rtDomainDict%gridMapping = "ProjectionCoordinateSystem"

   rtDomainDict%varNames(:) = [character(len=64) :: "zwattablrt","sfcheadsubrt","QSTRMVOLRT",&
                               "QBDRYRT"]
   rtDomainDict%longName(:) = [character(len=64) :: "water table depth","surface head",&
                               "channel inflow",&
                               "accumulated value of the boundary flux, + into domain - out of domain"]
   rtDomainDict%units(:) = [character(len=64) :: "m","mm","mm","mm"]
   rtDomainDict%scaleFactor(:) = [0.1,1.0,1.0,1.0]
   rtDomainDict%addOffset(:) = [0.0,0.0,0.0,0.0]
   rtDomainDict%outFlag(:) = [0,0,0,0]
   rtDomainDict%timeZeroFlag(:) = [1,1,1,1]
   rtDomainDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0]
   rtDomainDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0]
   rtDomainDict%validMinReal(:) = [0.0,0.0,0.0,0.0]
   rtDomainDict%validMaxReal(:) = [10.0,1000000.0,1000.0,1000.0]

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numRtDomainVars
      rtDomainDict%fillComp(i) = NINT((rtDomainDict%fillReal(i)+rtDomainDict%addOffset(i))/rtDomainDict%scaleFactor(i))
      rtDomainDict%missingComp(i) = NINT((rtDomainDict%missingReal(i)+rtDomainDict%addOffset(i))/rtDomainDict%scaleFactor(i))
      rtDomainDict%validMinComp(i) = NINT((rtDomainDict%validMinReal(i)+rtDomainDict%addOffset(i))/rtDomainDict%scaleFactor(i))
      rtDomainDict%validMaxComp(i) = NINT((rtDomainDict%validMaxReal(i)+rtDomainDict%addOffset(i))/rtDomainDict%scaleFactor(i))
   end do

end subroutine initRtDomainDict

subroutine initLakeDict(lakeOutDict)
   implicit none

   type(lakeMeta), intent(inout) :: lakeOutDict

   lakeOutDict%modelNdv = -9.E15
   ! LAKE FILES
   ! NOTE !!!!! If you see PLC, this means OWP has no desire to output these,
   !            which means meta-data standards have yet to be determined 
   !            for these variables. Fill in if it's desired to output....
   ! First establish global attributes for the channel output files 
   lakeOutDict%fType = 'timeSeries'
   lakeOutDict%proj4 = '+proj=longlat +datum=NAD83 +no_defs'
   lakeOutDict%initTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   lakeOutDict%validTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   lakeOutDict%lakeDim = 'lake_id'
   lakeOutDict%cdm = 'PLACEHOLDER'
   lakeOutDict%esri = 'GEOGCS[GCS_North_American_1983,DATUM[D_North_American_1983,&
                      &SPHEROID[GRS_1980,6378137.0,298.257222101]],&
                      &PRIMEM[Greenwich,0.0],UNIT[Degree,0.017453292519943295]]'
   lakeOutDict%conventions = 'CF-1.6'

   ! Next establish time attribues
   lakeOutDict%timeLName = 'valid output time'
   lakeOutDict%timeUnits = 'minutes since 1970-01-01 00:00:00 UTC'
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
   lakeOutDict%cfRole = 'timeseries_id'

   ! Esatablish lat/lon attributes
   lakeOutDict%latLName = "Lake latitude"
   lakeOutDict%latUnits = "degrees_north"
   lakeOutDict%latStName = "latitude"
   lakeOutDict%lonLName = "Lake longitude"
   lakeOutDict%lonUnits = "degrees_east"
   lakeOutDict%lonStName = "longitude"

   lakeOutDict%varNames(:) = [character(len=64) :: 'inflow','outflow']
   lakeOutDict%longName(:) = [character(len=64) :: 'Lake Inflow','Lake Outflow']
   lakeOutDict%units(:) = [character(len=64) :: 'm3 s-1','m3 s-1']
   lakeOutDict%coordNames(:) = [character(len=64) :: 'latitude longitude','latitude longitude']
   lakeOutDict%scaleFactor(:) = [0.01,0.01]
   lakeOutDict%addOffset(:) = [0.0,0.0]
   lakeOutDict%outFlag(:) = [0,0]
   lakeOutDict%timeZeroFlag(:) = [1,1]
   lakeOutDict%fillReal(:) = [-9999.0,-9999.0]
   lakeOutDict%missingReal(:) = [-9999.0,-9999.0]
   lakeOutDict%validMinReal(:) = [-10000.0,-10000.0]
   lakeOutDict%validMaxreal(:) = [10000.0,10000.0]

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numLakeVars
      lakeOutDict%fillComp(i) = NINT((lakeOutDict%fillReal(i)+lakeOutDict%addOffset(i))/lakeOutDict%scaleFactor(i))
      lakeOutDict%missingComp(i) = NINT((lakeOutDict%missingReal(i)+lakeOutDict%addOffset(i))/lakeOutDict%scaleFactor(i))
      lakeOutDict%validMinComp(i) = NINT((lakeOutDict%validMinReal(i)+lakeOutDict%addOffset(i))/lakeOutDict%scaleFactor(i))
      lakeOutDict%validMaxComp(i) = NINT((lakeOutDict%validMaxReal(i)+lakeOutDict%addOffset(i))/lakeOutDict%scaleFactor(i))
   end do

end subroutine initLakeDict

subroutine initChrtGrdDict(chrtGrdDict,procId,diagFlag)
   use module_namelist, only: nlst_rt
   use netcdf
   implicit none

   type(chrtGrdMeta), intent(inout) :: chrtGrdDict
   integer, intent(inout) :: procId,diagFlag
   integer :: ftnMeta,projVarId,xVarId,yVarId,ftnGeo
   integer :: xDimId,numColLand,numColHydro
   real :: resLand,resHydro,aggFactor
   integer :: iret
   !CHRTOUT_GRID files

   chrtGrdDict%modelNdv = -9.E15

   ! First establish global attributes.
   chrtGrdDict%initTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   chrtGrdDict%validTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   chrtGrdDict%decimation = 1
   chrtGrdDict%conventions = "CF-1.6"

   ! Next establish time attributes
   chrtGrdDict%timeLName = "valid output time"
   chrtGrdDict%timeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   chrtGrdDict%timeStName = "time"
   chrtGrdDict%rTimeLName = "model initialization time"
   chrtGrdDict%rTimeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   chrtGrdDict%rTimeStName = "forecast_reference_time"

   ! Pull spatial metadata information about the modeling domain from the land
   ! spatial metadata file. 
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst_rt(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid.
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open LAND spatial metadata file.')
      else
         ! First pull metadata on coordinate system.
         iret = nf90_inq_varid(ftnMeta,'ProjectionCoordinateSystem',projVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find ProjectionCoordinateSystem in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'_CoordinateTransformType',chrtGrdDict%projTransform)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find _CoordinateTransformType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'transform_name',chrtGrdDict%projTransformName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find transform_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'grid_mapping_name',chrtGrdDict%projGridMappingName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find grid_mapping_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'_CoordinateAxes',chrtGrdDict%projCoordAxes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find _CoordinateAxes in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'standard_parallel',chrtGrdDict%stParallel)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find standard_parallel in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'longitude_of_central_meridian',chrtGrdDict%lonCtrlMer)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find longitude_of_central_meridian in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'latitude_of_projection_origin',chrtGrdDict%latOrig)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find latitude_of_projection_origin in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'false_easting',chrtGrdDict%falseEast)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find false_easting in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'false_northing',chrtGrdDict%falseNorth)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find false_northing in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'earth_radius',chrtGrdDict%radius)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find earth_radius in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'esri_pe_string',chrtGrdDict%projEsri)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find esri_pe_string in LAND spatial metadata file.')
         iret = nf90_get_att(ftnMeta,projVarId,'proj4',chrtGrdDict%proj4)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find proj4 in LAND spatial metadata file.')
         ! Next, get the number of columns on the land grid
         iret = nf90_inq_dimid(ftnMeta,'x',xDimId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find the x dimension in the LAND spatial metadata file')
         iret = nf90_inquire_dimension(ftnMeta,xDimId,len=numColLand)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to retrieve the x-dimension value in the LAND spatial metadata file')
         ! Next pull metadata on x coordinate.
         iret = nf90_inq_varid(ftnMeta,'x',xVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'standard_name',chrtGrdDict%xStName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x standard_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'long_name',chrtGrdDict%xLName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x long_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'_CoordinateAxisType',chrtGrdDict%xAxisType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x _CoordinateAxisType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'units',chrtGrdDict%xUnits)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x units in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'resolution',resLand)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x resolution in LAND spatial metadata file')
         ! Next pull metadata on y coordinates.
         iret = nf90_inq_varid(ftnMeta,'y',yVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find y variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'standard_name',chrtGrdDict%yStName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y standard_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'long_name',chrtGrdDict%yLName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y long_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'_CoordinateAxisType',chrtGrdDict%yAxisType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y _CoordinateAxisType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'units',chrtGrdDict%yUnits)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y units in LAND spatial metadata file')
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')
      endif

      ! Next get the number of columns on the high-resolution routing grid.
      ! This will be used to calculate the resolution of the routing grid in
      ! meters.
      iret = nf90_open(trim(nlst_rt(1)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnGeo)
      if(iret .ne. 0) then
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open Fulldom file')
      else
         iret = nf90_inq_dimid(ftnGeo,'x',xDimId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x dimension in Fulldom file')
         iret = nf90_inquire_dimension(ftnGeo,xDimId,len=numColHydro)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to retrieve number of columns in the Fulldom file')
         iret = nf90_close(ftnGeo)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close the Fulldom file')
      endif

      ! Calculate the aggregation factor and resolution of the hydro routing
      ! grid.
      aggFactor = float(numColHydro/numColLand)
      resHydro = resLand/aggFactor

      chrtGrdDict%xRes = resHydro
      chrtGrdDict%yRes = resHydro

   endif

   chrtGrdDict%gridMapping = "ProjectionCoordinateSystem"

   chrtGrdDict%varNames(:) = [character(len=64) :: "streamflow"]
   chrtGrdDict%longName(:) = [character(len=64) :: "River Flow"]
   chrtGrdDict%units(:) = [character(len=64) :: "m3 s-1"]
   chrtGrdDict%scaleFactor(:) = [0.1]
   chrtGrdDict%addOffset(:) = [0.0]
   chrtGrdDict%outFlag(:) = [0]
   chrtGrdDict%timeZeroFlag(:) = [1]
   chrtGrdDict%missingReal(:) = [-9999.0]
   chrtGrdDict%fillReal(:) = [-9999.0]
   chrtGrdDict%validMinReal(:) = [0.0]
   chrtGrdDict%validMaxReal(:) = [500000.0]

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numChGrdVars
      chrtGrdDict%fillComp(i) = NINT((chrtGrdDict%fillReal(i)+chrtGrdDict%addOffset(i))/chrtGrdDict%scaleFactor(i))
      chrtGrdDict%missingComp(i) = NINT((chrtGrdDict%missingReal(i)+chrtGrdDict%addOffset(i))/chrtGrdDict%scaleFactor(i))
      chrtGrdDict%validMinComp(i) = NINT((chrtGrdDict%validMinReal(i)+chrtGrdDict%addOffset(i))/chrtGrdDict%scaleFactor(i))
      chrtGrdDict%validMaxComp(i) = NINT((chrtGrdDict%validMaxReal(i)+chrtGrdDict%addOffset(i))/chrtGrdDict%scaleFactor(i))
   end do

end subroutine initChrtGrdDict

subroutine initLsmOutDict(lsmOutDict,procId,diagFlag)
   use module_namelist, only: nlst_rt
   use netcdf
   implicit none

   type(lsmMeta), intent(inout) :: lsmOutDict
   integer, intent(inout) :: procId
   integer, intent(inout) :: diagFlag
   integer :: ftnMeta,projVarId,xVarId,yVarId
   integer :: iret
   !LSMOUT files

   lsmOutDict%modelNdv = 9.9692099683868690E36
   lsmOutDict%modelNdvInt = -2147483647

   ! First establish global attributes.
   lsmOutDict%title = "OUTPUT FROM WRF-Hydro v4.0"
   lsmOutDict%initTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   lsmOutDict%validTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   lsmOutDict%conventions = "CF-1.6"

   ! Next establish time attributes
   lsmOutDict%timeLName = "valid output time"
   lsmOutDict%timeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   lsmOutDict%timeStName = "time"
   lsmOutDict%rTimeLName = "model initialization time"
   lsmOutDict%rTimeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   lsmOutDict%rTimeStName = "forecast_reference_time"

   ! Pull spatial metadata information about the modeling domain from the land
   ! spatial metadata file. 
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst_rt(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid.
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open LAND spatial metadata file.')
      else
         ! First pull metadata on coordinate system.
         iret = nf90_inq_varid(ftnMeta,'ProjectionCoordinateSystem',projVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find ProjectionCoordinateSystem in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'_CoordinateTransformType',lsmOutDict%projTransform)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find _CoordinateTransformType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'transform_name',lsmOutDict%projTransformName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find transform_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'grid_mapping_name',lsmOutDict%projGridMappingName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find grid_mapping_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'_CoordinateAxes',lsmOutDict%projCoordAxes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find _CoordinateAxes in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'standard_parallel',lsmOutDict%stParallel)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find standard_parallel in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'longitude_of_central_meridian',lsmOutDict%lonCtrlMer)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find longitude_of_central_meridian in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'latitude_of_projection_origin',lsmOutDict%latOrig)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find latitude_of_projection_origin in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'false_easting',lsmOutDict%falseEast)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find false_easting in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'false_northing',lsmOutDict%falseNorth)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find false_northing in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'earth_radius',lsmOutDict%radius)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find earth_radius in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,projVarId,'esri_pe_string',lsmOutDict%projEsri)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find esri_pe_string in LAND spatial metadata file.')
         iret = nf90_get_att(ftnMeta,projVarId,'proj4',lsmOutDict%proj4)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find proj4 in LAND spatial metadata file.')
         ! Next pull metadata on x coordinate.
         iret = nf90_inq_varid(ftnMeta,'x',xVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'standard_name',lsmOutDict%xStName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x standard_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'long_name',lsmOutDict%xLName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x long_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'_CoordinateAxisType',lsmOutDict%xAxisType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x _CoordinateAxisType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'units',lsmOutDict%xUnits)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x units in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,xVarId,'resolution',lsmOutDict%xRes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x resolution in LAND spatial metadata file')
         ! Next pull metadata on y coordinates.
         iret = nf90_inq_varid(ftnMeta,'y',yVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find y variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'standard_name',lsmOutDict%yStName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y standard_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'long_name',lsmOutDict%yLName)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y long_name in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'_CoordinateAxisType',lsmOutDict%yAxisType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y _CoordinateAxisType in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'units',lsmOutDict%yUnits)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y units in LAND spatial metadata file')
         iret = nf90_get_att(ftnMeta,yVarId,'resolution',lsmOutDict%yRes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get y resolution in LAND spatial metadata file')
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')
      endif
   endif

   lsmOutDict%gridMapping = "ProjectionCoordinateSystem"

   lsmOutDict%varNames(:) = [character(len=64) :: "stc1","smc1","sh2ox1","stc2",&
                             "smc2","sh2ox2","stc3","smc3","sh2ox3","stc4",&
                             "smc4","sh2ox4","infxsrt","sfcheadrt"]
   lsmOutDict%longName(:) = [character(len=64) :: "Soil temperature in the top layer",&
                             "Soil moisture in the top layer",&
                             "Volumetric soil moisture in the top layer",&
                             "Soil temperature in the second layer",&
                             "Soil moisture in the second layer",&
                             "Volumetric soil moisture in the second layer",&
                             "Soil temperature in the third layer",&
                             "Soil moisture in the third layer",&
                             "Volumetric soil moisture in the third layer",&
                             "Soil temperature in the fourth layer",&
                             "Soil moisture in the fourth layer",&
                             "Volumetric soil moisture in the fourth layer",&
                             "Infiltration excess","Surface head"]
   lsmOutDict%units(:) = [character(len=64) :: "K","fraction","fraction",&
                          "K","fraction","fraction","K","fraction",&
                          "fraction","K","fraction","fraction",&
                          "mm","mm"]
   lsmOutDict%scaleFactor(:) = [0.1,0.01,0.01,0.1,0.01,0.01,0.1,0.01,0.01,&
                                0.1,0.01,0.01,1.0,1.0]
   lsmOutDict%addOffset(:) = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                              0.0,0.0,0.0,0.0]
   lsmOutDict%timeZeroFlag(:) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1]
   lsmOutDict%numLev(:) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1]
   lsmOutDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                -9999.0,-9999.0,-9999.0,-9999.0]
   lsmOutDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                             -9999.0,-9999.0,-9999.0,-9999.0]
   lsmOutDict%validMinReal(:) = [150.0,0.0,0.0,150.0,0.0,0.0,150.0,0.0,0.0,&
                                 150.0,0.0,0.0,0.0,0.0]
   lsmOutDict%validMaxReal(:) = [400.0,1.0,1.0,400.0,1.0,1.0,400.0,1.0,1.0,&
                                 400.0,1.0,1.0,100000.0,100000.0]
   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numLsmVars
      lsmOutDict%fillComp(i) = NINT((lsmOutDict%fillReal(i)+lsmOutDict%addOffset(i))/lsmOutDict%scaleFactor(i))
      lsmOutDict%missingComp(i) = NINT((lsmOutDict%missingReal(i)+lsmOutDict%addOffset(i))/lsmOutDict%scaleFactor(i))
      lsmOutDict%validMinComp(i) = NINT((lsmOutDict%validMinReal(i)+lsmOutDict%addOffset(i))/lsmOutDict%scaleFactor(i))
      lsmOutDict%validMaxComp(i) = NINT((lsmOutDict%validMaxReal(i)+lsmOutDict%addOffset(i))/lsmOutDict%scaleFactor(i))
   end do

end subroutine initLsmOutDict

subroutine nwmCheck(diagFlag,iret,msg)
   implicit none

   ! Subroutine arguments.
   integer, intent(in) :: diagFlag,iret
   character(len=*), intent(in) :: msg

   ! Check status. If status of command is not 0, then post the error message
   ! if WRF_HYDRO_D was set to be 1.
   if (iret .ne. 0) then
      call hydro_stop(trim(msg))
   end if

end subroutine nwmCheck

end module module_NWM_io_dict

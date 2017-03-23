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
integer, parameter :: numChVars=14
integer :: i

! Declare public types that will hold metadata
public :: chrtMeta ! Public CHRTOUT metadata for NWM output.

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
   integer, dimension(numChVars) :: validMinComp ! Valid min (after conversion to integer)
   integer, dimension(numChVars) :: validMaxComp ! Valid max (after conversion to integer)
   real, dimension(numChVars) :: validMinReal ! Valid minimum (before conversion to integer)
   real, dimension(numChVars) :: validMaxReal ! Valid maximum (before converstion to integer)
   integer, dimension(numChVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numChVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numChVars) :: fillReal ! Fill value (before conversion to integer)
   integer, dimension(numChVars) :: fillComp ! Fill value (after conversion to integer)
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
   chrtOutDict%stDim = 'station'
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
   
   ! Next establish feature_id attributes
   chrtOutDict%featureIdLName = 'Reach ID'
   chrtOutDict%featureIdComment = 'NHDPlusv2 ComIDs within CONUS, arbitrary Reach IDs outside of CONUS'
   
   ! Now establish attributes for output variables.
   chrtOutDict%varNames(:) = (/"streamflow","nudge","q_lateral","velocity",&
                               "Latitude","Longitude","Altitude","Order",&
                               "Head","QSfcLatRunoff","QBucket",&
                               "QBtmVertRunoff","AccSfcLatRunoff","accBucket"/)
   chrtOutDict%longName(:) = (/"River Flow","Amount of stream flow alteration",&
                               "Runoff into channel reach","River Velocity",&
                               "PLC","PLC","PLC","PLC","PLC","PLC","PLC",&
                               "PLC","PLC","PLC"/)
   chrtOutDict%units(:) = (/"m3 s-1","m3 s-1","m3 s-1","m s-1",&
                            "PLC","PLC","PLC","PLC","PLC","PLC","PLC",&
                            "PLC","PLC","PLC"/)
   chrtOutDict%coordNames(:) = (/"latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude"/)
   chrtOutDict%scaleFactor(:) = [0.0001,0.1,0.1,0.01,-99.0,-99.0,-99.0,-99.0,&
                                 -99.0,-99.0,-99.0,-99.0,-99.0,-99.0]
   chrtOutDict%addOffset(:) = [0.0,0.0,0.0,0.0,-99.0,-99.0,-99.0,-99.0,-99.0,&
                               -99.0,-99.0,-99.0,-99.0,-99.0]
   ! Specify the outputflags. 0 - Do not output, 1 - Output. DO NOT MODIFY
   ! WITHOUT CONSULTING WITH OWP AND NCAR!!!!!!!!!!!!!!
   chrtOutDict%outFlag(:) = [1,0,1,1,0,0,0,0,0,0,0,0,0,0] 
   chrtOutDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0]
   chrtOutDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0]
   chrtOutDict%validMinReal(:) = [0.0,-500000.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0]
   chrtOutDict%validMaxReal(:) = [500000.0,500000.0,500000.0,10000.0,0.0,&
                                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numChVars
      chrtOutDict%fillComp(i) = int((chrtOutDict%fillReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
      chrtOutDict%missingComp(i) = int((chrtOutDict%missingReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
      chrtOutDict%validMinComp(i) = int((chrtOutDict%validMinReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
      chrtOutDict%validMaxComp(i) = int((chrtOutDict%validMaxReal(i)+chrtOutDict%addOffset(i))/chrtOutDict%scaleFactor(i))
   end do
end subroutine initChrtDict


end module module_NWM_io_dict

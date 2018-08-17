md5sum: f0f38dcd20c2cce5296f97694d9456da  /glade/work/jamesmcc/domains/private/HAWAII/NWM/RESTART/nudgingLastObs.2018-08-01_00:00:00.nc
ncdump -h: netcdf nudgingLastObs.2018-08-01_00\:00\:00 {
dimensions:
	timeStrLen = 19 ;
	timeInd = 480 ;
	stationIdStrLen = 15 ;
	stationIdInd = UNLIMITED ; // (58 currently)
	feature_id = 13637 ;
variables:
	char stationId(stationIdInd, stationIdStrLen) ;
		stationId:long_name = "USGS station identifer of length 15" ;
	char time(stationIdInd, timeInd, timeStrLen) ;
		time:units = "UTC" ;
		time:long_name = "YYYY-mm-dd_HH:MM:SS UTC" ;
	float discharge(stationIdInd, timeInd) ;
		discharge:units = "m^3/s" ;
		discharge:long_name = "Discharge.cubic_meters_per_second" ;
	float model_discharge(stationIdInd, timeInd) ;
		model_discharge:units = "m^3/s" ;
		model_discharge:long_name = "modelDischarge.cubic_meters_per_second" ;
	short discharge_quality(stationIdInd, timeInd) ;
		discharge_quality:units = "-" ;
		discharge_quality:long_name = "Discharge quality 0 to 100 to be scaled by 100." ;
	float nudge(feature_id) ;
		nudge:units = "m3 s-1" ;
		nudge:long_name = "Amount of stream flow alteration" ;

// global attributes:
		:modelTimeAtOutput = "2018-08-01_00:00:00" ;
}

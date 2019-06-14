md5sum: ad71fd9bb0271f8867828b939043fa99  /glade/work/jamesmcc/domains/private/CONUS/NWM/DOMAIN/nudgingParams_NWMv2.0.nc
ncdump -h: netcdf nudgingParams_NWMv2.0 {
dimensions:
	stationIdStrLen = 15 ;
	stationIdInd = UNLIMITED ; // (7541 currently)
	threshInd = 1 ;
	monthInd = 12 ;
	threshCatInd = 2 ;
variables:
	char stationId(stationIdInd, stationIdStrLen) ;
		stationId:units = "-" ;
		stationId:long_name = "USGS station identifier" ;
	float R(stationIdInd) ;
		R:units = "meters" ;
		R:long_name = "Radius of influence in meters" ;
	float G(stationIdInd) ;
		G:units = "-" ;
		G:long_name = "Amplitude of nudging" ;
	float tau(stationIdInd) ;
		tau:units = "minutes" ;
		tau:long_name = "Time tapering parameter hald window size in minutes" ;
	float qThresh(stationIdInd, monthInd, threshInd) ;
		qThresh:units = "m^3/s" ;
		qThresh:long_name = "Discharge threshold category" ;
	float expCoeff(stationIdInd, monthInd, threshCatInd) ;
		expCoeff:units = "minutes" ;
		expCoeff:long_name = "Coefficient b in denominator e^(-dt/b)" ;
}

md5sum: 24fc3027e78031ce0bd05b0e691681c0  /glade/work/jamesmcc/domains/private/HAWAII/NWM/DOMAIN/GWBUCKPARM.nc
ncdump -h: netcdf GWBUCKPARM {
dimensions:
	BasinDim = 13296 ;
variables:
	int Basin(BasinDim) ;
		Basin:long_name = "Basin monotonic ID (1...n)" ;
	float Coeff(BasinDim) ;
		Coeff:long_name = "Coefficient" ;
	float Expon(BasinDim) ;
		Expon:long_name = "Exponent" ;
	float Zmax(BasinDim) ;
		Zmax:long_name = "Zmax" ;
	float Zinit(BasinDim) ;
		Zinit:long_name = "Zinit" ;
	float Area_sqkm(BasinDim) ;
		Area_sqkm:long_name = "Basin area in square kilometers" ;
	int ComID(BasinDim) ;
		ComID:long_name = "NHDCatchment FEATUREID (NHDFlowline ComID)" ;

// global attributes:
		:featureType = "point" ;
		:history = "Created Fri Jan 12 11:00:09 2018" ;
}

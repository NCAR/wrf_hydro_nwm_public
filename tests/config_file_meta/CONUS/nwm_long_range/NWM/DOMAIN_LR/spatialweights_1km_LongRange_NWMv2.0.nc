md5sum: 3b555961880f3f439fb4a4cded1e93cf  /glade/work/jamesmcc/domains/private/CONUS/NWM/DOMAIN_LR/spatialweights_1km_LongRange_NWMv2.0.nc
ncdump -h: netcdf spatialweights_1km_LongRange_NWMv2.0 {
dimensions:
	polyid = 2677135 ;
	data = 21508911 ;
variables:
	int polyid(polyid) ;
		polyid:long_name = "ID of polygon" ;
	int overlaps(polyid) ;
		overlaps:long_name = "Number of intersecting polygons" ;
	double weight(data) ;
		weight:long_name = "fraction of polygon(polyid) intersected by polygon identified by poly2" ;
	double regridweight(data) ;
		regridweight:long_name = "fraction of intersecting polyid(overlapper) intersected by polygon(polyid)" ;
	int IDmask(data) ;
		IDmask:long_name = "Polygon ID (polyid) associated with each record" ;
	int i_index(data) ;
		i_index:long_name = "Index in the x dimension of the raster grid (starting with 1,1 in LL corner)" ;
	int j_index(data) ;
		j_index:long_name = "Index in the y dimension of the raster grid (starting with 1,1 in LL corner)" ;
}

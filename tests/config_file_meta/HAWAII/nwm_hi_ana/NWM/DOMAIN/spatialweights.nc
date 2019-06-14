md5sum: b8839928ee59b1b3270b28be9158b936  /glade/work/jamesmcc/domains/private/HAWAII/NWM/DOMAIN/spatialweights.nc
ncdump -h: netcdf spatialweights {
dimensions:
	polyid = 13296 ;
	data = 1432428 ;
variables:
	int polyid(polyid) ;
		polyid:long_name = "ID of polygon" ;
	int IDmask(data) ;
		IDmask:long_name = "Polygon ID (polyid) associated with each record" ;
	int overlaps(polyid) ;
		overlaps:long_name = "Number of intersecting polygons" ;
	double weight(data) ;
		weight:long_name = "fraction of polygon(polyid) intersected by polygon identified by poly2" ;
	double regridweight(data) ;
		regridweight:long_name = "fraction of intersecting polyid(overlapper) intersected by polygon(polyid)" ;
	int i_index(data) ;
		i_index:long_name = "Index in the x dimension of the raster grid (starting with 1,1 in LL corner)" ;
	int j_index(data) ;
		j_index:long_name = "Index in the y dimension of the raster grid (starting with 1,1 in LL corner)" ;

// global attributes:
		:history = "Created Fri Jan 12 22:55:18 2018" ;
}

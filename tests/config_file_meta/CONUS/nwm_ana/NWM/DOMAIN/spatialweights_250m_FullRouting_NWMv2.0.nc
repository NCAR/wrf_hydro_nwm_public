md5sum: 5f265b510101ec88915458d78e8809e0  /glade/work/jamesmcc/domains/private/CONUS/NWM/DOMAIN/spatialweights_250m_FullRouting_NWMv2.0.nc
ncdump -h: netcdf spatialweights_250m_FullRouting_NWMv2.0 {
dimensions:
	polyid = 2677135 ;
	data = 171773479 ;
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

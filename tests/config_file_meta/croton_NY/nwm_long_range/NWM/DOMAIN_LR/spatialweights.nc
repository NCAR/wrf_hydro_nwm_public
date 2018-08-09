md5sum: 60eba1d67985d77ca5cc776f005dd88a  /glade/work/jamesmcc/domains/public/croton_NY/NWM/DOMAIN_LR/spatialweights.nc
ncdump -h: netcdf spatialweights {
dimensions:
	data = 812 ;
	polyid = 183 ;
variables:
	int IDmask(data) ;
		IDmask:long_name = "Polygon ID (polyid) associated with each record" ;
	int i_index(data) ;
		i_index:long_name = "Index in the x dimension of the raster grid (starting with 1,1 in LL corner)" ;
	int j_index(data) ;
		j_index:long_name = "Index in the y dimension of the raster grid (starting with 1,1 in LL corner)" ;
	int overlaps(polyid) ;
		overlaps:long_name = "Number of intersecting polygons" ;
	int polyid(polyid) ;
		polyid:long_name = "ID of polygon" ;
	double regridweight(data) ;
		regridweight:long_name = "fraction of intersecting polyid(overlapper) intersected by polygon(polyid)" ;
	double weight(data) ;
		weight:long_name = "fraction of polygon(polyid) intersected by polygon identified by poly2" ;

// global attributes:
		:history = "Mon Aug  6 15:07:55 2018: ncks -O -d polyid,1,183 -d data,1,812 /glade/u/home/arezoo/scratch/for/for_Joe/0137462010/spatialweights.nc /glade/u/home/arezoo/scratch/for/for_Joe/0137462010/spatialweights.nc" ;
		:NCO = "netCDF Operators version 4.7.4 (http://nco.sf.net)" ;
}

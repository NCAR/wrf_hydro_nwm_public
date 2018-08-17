md5sum: 9c20e484ffb614f5157dbe84e49e4b37  /glade/work/jamesmcc/domains/private/HAWAII/NWM/DOMAIN/GEOGRID_LDASOUT_Spatial_Metadata.nc
ncdump -h: netcdf GEOGRID_LDASOUT_Spatial_Metadata {
dimensions:
	y = 390 ;
	x = 590 ;
variables:
	double y(y) ;
		y:standard_name = "projection_y_coordinate" ;
		y:long_name = "y coordinate of projection" ;
		y:units = "m" ;
		y:_CoordinateAxisType = "GeoY" ;
		y:resolution = 1000. ;
	double x(x) ;
		x:standard_name = "projection_x_coordinate" ;
		x:long_name = "x coordinate of projection" ;
		x:units = "m" ;
		x:_CoordinateAxisType = "GeoX" ;
		x:resolution = 1000. ;
	char crs ;
		crs:transform_name = "lambert_conformal_conic" ;
		crs:grid_mapping_name = "lambert_conformal_conic" ;
		crs:esri_pe_string = "PROJCS[\"Lambert_Conformal_Conic\",GEOGCS[\"GCS_Sphere\",DATUM[\"D_Sphere\",SPHEROID[\"Sphere\",6370000.0,0.0]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Lambert_Conformal_Conic_2SP\"],PARAMETER[\"false_easting\",0.0],PARAMETER[\"false_northing\",0.0],PARAMETER[\"central_meridian\",-157.42],PARAMETER[\"standard_parallel_1\",10.0],PARAMETER[\"standard_parallel_2\",30.0],PARAMETER[\"latitude_of_origin\",20.6],UNIT[\"Meter\",1.0]];-38489800 -26559400 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision" ;
		crs:spatial_ref = "PROJCS[\"Lambert_Conformal_Conic\",GEOGCS[\"GCS_Sphere\",DATUM[\"D_Sphere\",SPHEROID[\"Sphere\",6370000.0,0.0]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Lambert_Conformal_Conic_2SP\"],PARAMETER[\"false_easting\",0.0],PARAMETER[\"false_northing\",0.0],PARAMETER[\"central_meridian\",-157.42],PARAMETER[\"standard_parallel_1\",10.0],PARAMETER[\"standard_parallel_2\",30.0],PARAMETER[\"latitude_of_origin\",20.6],UNIT[\"Meter\",1.0]];-38489800 -26559400 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision" ;
		crs:long_name = "CRS definition" ;
		crs:longitude_of_prime_meridian = 0. ;
		crs:GeoTransform = "-295000.474687 1000.0 0 195000.081451 0 -1000.0 " ;
		crs:_CoordinateAxes = "y x" ;
		crs:_CoordinateTransformType = "Projection" ;
		crs:standard_parallel = 10., 30. ;
		crs:longitude_of_central_meridian = -157.42 ;
		crs:latitude_of_projection_origin = 20.6 ;
		crs:false_easting = 0. ;
		crs:false_northing = 0. ;
		crs:earth_radius = 6370000. ;
		crs:semi_major_axis = 6370000. ;
		crs:inverse_flattening = 0. ;

// global attributes:
		:Conventions = "CF-1.5" ;
		:GDAL_DataType = "Generic" ;
		:Source_Software = "WRF-Hydro GIS Pre-processor v5 (01/2018)" ;
		:proj4 = "+proj=lcc +units=m +a=6370000.0 +b=6370000.0 +lat_1=10.0 +lat_2=30.0 +lat_0=20.6 +lon_0=-157.42 +x_0=0 +y_0=0 +k_0=1.0 +nadgrids=@null +wktext  +no_defs " ;
		:history = "Created Fri Mar 16 13:47:42 2018" ;
		:processing_notes = "Created: Fri Mar 16 10:03:32 2018" ;
}

md5sum: 732b39412306e6cc824009310f115e1a  /glade/work/jamesmcc/domains/public/croton_NY_v5.0.1/NWM/DOMAIN/LAKEPARM.nc
ncdump -h: netcdf LAKEPARM {
dimensions:
	nlakes = 1 ;
variables:
	double LkArea(nlakes) ;
		LkArea:long_name = "Gridded lake area (sq. km)" ;
		LkArea:coordinates = "lat lon" ;
	double LkMxE(nlakes) ;
		LkMxE:long_name = "Maximum lake elevation (m ASL)" ;
		LkMxE:coordinates = "lat lon" ;
	double OrificeA(nlakes) ;
		OrificeA:long_name = "Orifice cross-sectional area (sq. m)" ;
		OrificeA:coordinates = "lat lon" ;
	double OrificeC(nlakes) ;
		OrificeC:long_name = "Orifice coefficient" ;
		OrificeC:coordinates = "lat lon" ;
	double OrificeE(nlakes) ;
		OrificeE:long_name = "Orifice elevation (m ASL)" ;
		OrificeE:coordinates = "lat lon" ;
	double WeirC(nlakes) ;
		WeirC:long_name = "Weir coefficient" ;
		WeirC:coordinates = "lat lon" ;
	double WeirE(nlakes) ;
		WeirE:units = "m" ;
		WeirE:long_name = "Weir Height (m ASL)" ;
		WeirE:coordinates = "lat lon" ;
	double WeirL(nlakes) ;
		WeirL:long_name = "Weir length (m)" ;
		WeirL:coordinates = "lat lon" ;
	int ascendingIndex(nlakes) ;
		ascendingIndex:long_name = "Index to use for sorting IDs (ascending)" ;
	double ifd(nlakes) ;
		ifd:long_name = "Initial Fractional Depth" ;
		ifd:units = "ratio" ;
	int lake_id(nlakes) ;
		lake_id:long_name = "Lake ID" ;
		lake_id:cf_role = "timeseries_id" ;
	float lat(nlakes) ;
		lat:units = "degrees_north" ;
		lat:long_name = "latitude of the lake centroid" ;
		lat:standard_name = "latitude" ;
	float lon(nlakes) ;
		lon:units = "degrees_east" ;
		lon:long_name = "longitude of the lake centroid" ;
		lon:standard_name = "longitude" ;
	double time(nlakes) ;
		time:units = "days since 2000-01-01 00:00:00" ;
		time:long_name = "time of measurement" ;
		time:standard_name = "time" ;

// global attributes:
		:featureType = "timeSeries" ;
		:nco_openmp_thread_number = 1 ;
		:history = "Sun Aug  5 17:31:54 2018: ncks -O -d nlakes,1,1 /glade/u/home/arezoo/scratch/for/for_Joe/0137462010/LAKEPARM.nc /glade/u/home/arezoo/scratch/for/for_Joe/0137462010/LAKEPARM.nc\n",
			"Tue Sep 12 11:35:56 2017: ncatted -O -a units,ifd,c,c,ratio LAKEPARM_2017_4.nc\n",
			"Tue Sep 12 11:35:50 2017: ncatted -O -a long_name,ifd,c,c,Initial Fractional Depth LAKEPARM_2017_4.nc\n",
			"Tue Sep 12 11:35:17 2017: ncap2 -s ifd[$nlakes]=0.9 LAKEPARM_2017_3.nc LAKEPARM_2017_4.nc\n",
			"Tue Sep 12 11:28:46 2017: ncatted -O -a long_name,ifd,c,c,Initial Fractional Depth LAKEPARM_2017_3.nc\n",
			"Tue Sep 12 11:28:17 2017: ncatted -O -a units,ifd,c,c,ratio LAKEPARM_2017_3.nc\n",
			"Tue Sep 12 11:22:13 2017: ncap2 -s ifd=0.9 LAKEPARM_2017_2.nc LAKEPARM_2017_3.nc\n",
			"Tue Sep 12 11:17:55 2017: ncks -O -x -v alt LAKEPARM_2017.nc LAKEPARM_2017_2.nc\n",
			"Tue Sep 12 11:17:20 2017: ncks -O -x -v Discharge LAKEPARM_2017_04_14_subset_LK.nc LAKEPARM_2017.nc\n",
			"Tue Sep 12 11:14:16 2017: ncrename -O -v LkMxH,LkMxE LAKEPARM_2017_04_14_subset_LK.nc\n",
			"Tue Sep 12 11:13:45 2017: ncrename -O -v WeirH,WeirE LAKEPARM_2017_04_14_subset_LK.nc\n",
			"Created Fri Apr 14 07:50:13 2017" ;
		:NCO = "netCDF Operators version 4.7.4 (http://nco.sf.net)" ;
}

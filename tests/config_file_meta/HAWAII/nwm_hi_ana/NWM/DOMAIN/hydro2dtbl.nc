md5sum: da7abdd2cac7b2d9df686ab6611f7fe3  /glade/work/jamesmcc/domains/private/HAWAII/NWM/DOMAIN/hydro2dtbl.nc
ncdump -h: netcdf hydro2dtbl {
dimensions:
	south_north = 390 ;
	west_east = 590 ;
variables:
	float LKSAT(south_north, west_east) ;
		LKSAT:_FillValue = -9999.f ;
	float OV_ROUGH2D(south_north, west_east) ;
		OV_ROUGH2D:_FillValue = -9999.f ;
	float SMCMAX1(south_north, west_east) ;
		SMCMAX1:_FillValue = -9999.f ;
	float SMCREF1(south_north, west_east) ;
		SMCREF1:_FillValue = -9999.f ;
	float SMCWLT1(south_north, west_east) ;
		SMCWLT1:_FillValue = -9999.f ;

// global attributes:
		:TITLE = "OUTPUT FROM GEOGRID V3.9" ;
		:SIMULATION_START_DATE = "0000-00-00_00:00:00" ;
		:WEST-EAST_GRID_DIMENSION = 591 ;
		:SOUTH-NORTH_GRID_DIMENSION = 391 ;
		:BOTTOM-TOP_GRID_DIMENSION = 0 ;
		:WEST-EAST_PATCH_START_UNSTAG = 1 ;
		:WEST-EAST_PATCH_END_UNSTAG = 590 ;
		:WEST-EAST_PATCH_START_STAG = 1 ;
		:WEST-EAST_PATCH_END_STAG = 591 ;
		:SOUTH-NORTH_PATCH_START_UNSTAG = 1 ;
		:SOUTH-NORTH_PATCH_END_UNSTAG = 390 ;
		:SOUTH-NORTH_PATCH_START_STAG = 1 ;
		:SOUTH-NORTH_PATCH_END_STAG = 391 ;
		:GRIDTYPE = "C" ;
		:DX = 1000.f ;
		:DY = 1000.f ;
		:DYN_OPT = 2 ;
		:CEN_LAT = 20.60001f ;
		:CEN_LON = -157.42f ;
		:TRUELAT1 = 10.f ;
		:TRUELAT2 = 30.f ;
		:MOAD_CEN_LAT = 20.60001f ;
		:STAND_LON = -157.42f ;
		:POLE_LAT = 90.f ;
		:POLE_LON = 0.f ;
		:corner_lats = 18.80074f, 22.35243f, 22.35243f, 18.80074f, 18.80066f, 22.35237f, 22.35237f, 18.80066f, 18.79617f, 22.35703f, 22.35703f, 18.79617f, 18.7961f, 22.35693f, 22.35693f, 18.7961f ;
		:corner_lons = -160.2608f, -160.3262f, -154.5138f, -154.5792f, -160.2656f, -160.3312f, -154.5088f, -154.5744f, -160.2607f, -160.3263f, -154.5137f, -154.5793f, -160.2655f, -160.3313f, -154.5087f, -154.5744f ;
		:MAP_PROJ = 1 ;
		:MMINLU = "USGS" ;
		:NUM_LAND_CAT = 24LL ;
		:ISWATER = 16LL ;
		:ISLAKE = -1LL ;
		:ISICE = 24LL ;
		:ISURBAN = 1LL ;
		:ISOILWATER = 14LL ;
		:grid_id = 1 ;
		:parent_id = 1 ;
		:i_parent_start = 1 ;
		:j_parent_start = 1 ;
		:i_parent_end = 591 ;
		:j_parent_end = 391 ;
		:parent_grid_ratio = 1 ;
		:sr_x = 1 ;
		:sr_y = 1 ;
		:FLAG_MF_XY = 1 ;
		:FLAG_LAI12M = 1 ;
		:FLAG_LAKE_DEPTH = 1 ;
		:history = "Mon Sep 18 16:39:37 2017: ncks -O -x -v HGT_M hydro2dtbl.nc.ALL hydro2dtbl.nc.ALL\nMon Sep 18 16:39:37 2017: ncks -O -4 -v HGT_M geo_em_CCAP2011.d01.nc hydro2dtbl.nc.ALL" ;
		:NCO = "\"4.5.5\"" ;
}

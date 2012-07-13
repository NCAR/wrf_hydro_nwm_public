.IGNORE:
RM		=	rm -f
COMPILER90=	gfortran
F90FLAGS  =       -w -c -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 
MODFLAG	=	
LDFLAGS	=	
CPP	=       /lib/cpp
CPPFLAGS	=       -C -P -traditional
LIBS 	=	
NETCDFINC       =       /raid/weiyu/netcdf/netcdf-4.1.2/include
NETCDFLIB       =       -L/raid/weiyu/netcdf/netcdf-4.1.2/lib -lnetcdff -lnetcdf

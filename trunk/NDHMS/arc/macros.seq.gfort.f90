.IGNORE:
RMD		=	rm -f
COMPILER90=	gfortran
F90FLAGS  =       -w -c -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 
MODFLAG	=	-I./ -I../mod
LDFLAGS	=	
CPP	=       /lib/cpp
CPPFLAGS	=       -C -P -traditional -I ../Data_Rec
LIBS 	=	
NETCDFINC       =       $(HOME)/netcdf/include
NETCDFLIB       =       -L($HOME)/netcdf/lib -lnetcdff -lnetcdf

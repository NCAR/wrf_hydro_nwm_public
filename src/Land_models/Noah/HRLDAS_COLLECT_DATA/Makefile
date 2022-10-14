SHELL=/bin/sh
.SUFFIXES:	
.SUFFIXES:	.F .c .o .exe

include ../user_build_options

OBJS=	consolidate_grib.o

CMD=	consolidate_grib.exe

default:
	(cd lib; make)
	(make -f Makefile all)

all:	lib/libsmda.a $(CMD)
	(cd lib; make)

lib/libsmda.a:
	(cd lib; make)

.F.o:
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(FREESOURCE) $(F90FLAGS) -c $(NETCDFMOD) -I./lib $(MODFLAG)./lib $(*).F

$(CMD):	lib/libsmda.a $(OBJS)
	(cd lib; make)
	$(COMPILERF90) -o $(@) -I./lib $(F90FLAGS) $(MODFLAG)./lib $(OBJS) \
		-L./lib -lsmda $(NETCDFLIB) $(HDF5LIB) $(BZIP2_LIB) $(LIBJASPER)

clean:
	$(RM) *.o *~ *.exe *.mod
	(cd lib; make clean)
#






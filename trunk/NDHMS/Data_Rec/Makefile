# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	module_RT_data.o \
	module_gw_gw2d_data.o

	# module_namelist.o \

all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I../mod $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)
	cp *.mod ../mod

# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ *.f

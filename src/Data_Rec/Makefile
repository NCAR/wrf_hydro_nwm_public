# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	module_namelist_inc.o \
	module_namelist.o \
	module_rt_inc.o	\
	module_RT_data.o \
	module_gw_gw2d_data.o

all:	$(OBJS)
	ar -cr ../lib/libHYDRO.a $(OBJS)

%.o: %.F
	@echo ""
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -I$(NETCDFINC) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I../mod $<
	@echo ""
	@cp *.mod ../mod

# Dependencies:
#
module_namelist.o: module_namelist_inc.o
module_RT_data.o: module_rt_inc.o

clean:
	rm -f *.o *.mod *.stb *~

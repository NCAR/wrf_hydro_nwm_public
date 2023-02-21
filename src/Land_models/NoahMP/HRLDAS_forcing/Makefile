 SHELL=/bin/sh
.SUFFIXES:
.SUFFIXES:	.F .c .o .exe

include ../user_build_options

OBJS=	create_forcing.o

CMD=	create_forcing.exe

default:
	(cd lib; $(MAKE))
	($(MAKE) -f Makefile all)

all:	lib/libsmda.a $(CMD)
	(cd lib; $(MAKE))

lib/libsmda.a:
	(cd lib; $(MAKE))

%.o: %.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPP_NETCDF4_COMPRESS) $(FREESOURCE) $(F90FLAGS) $(LDFLAGS) -c $(NETCDFMOD) -I./lib -I./lib $<

$(CMD):	lib/libsmda.a $(OBJS)
	(cd lib; $(MAKE))
	$(COMPILERF90) -o $(@) -I./lib $(F90FLAGS) $(LDFLAGS) -I./lib $(OBJS) \
		-L./lib -lsmda $(NETCDFLIB) $(BZIP2_LIB) $(LIBJASPER)

clean:
	$(RM) *.o *~ *.exe *.mod
	(cd lib; $(MAKE) clean)
#






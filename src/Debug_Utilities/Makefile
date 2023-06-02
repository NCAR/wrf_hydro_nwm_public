# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	debug_dump_variable.o

all:	$(OBJS)
	ar -r ../lib/libHYDRO.a $(OBJS)

%.o: %.F
	@echo ""
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -I$(NETCDFINC) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I../mod $<
	@echo ""
	@cp *.mod ../mod

# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~

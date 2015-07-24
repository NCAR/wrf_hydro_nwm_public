objects = \
          HRAP.o   XMRG.o xmrgtoasc.o endian.o write_grid2.o fill_miss2d.o

cppsources = \
          HRAP.cpp XMRG.cpp xmrgtoasc.cpp

csources = endian.c  fill_miss2d.c
 
fsources = write_grid2.f

BOOSTDIR = /fs/hsmb5/hydro/users/zcui/boost_1_36_0Build
BOOSTINCLUDEDIR = /fs/hsmb5/hydro/users/zcui/boost_1_36_0Build/include/boost-1_36

.SUFFIXES: .cpp .o .c .f

CXXFLAGS = -Wall -DLINX -I$(BOOSTINCLUDEDIR)
CFLAGS = -Wall -DLINX
F90FLAGS = -fno-second-underscore 

LIBS = -L$(BOOSTDIR)/lib                                     \
                  $(BOOSTDIR)/lib/libboost_system-gcc.a     \
                  $(BOOSTDIR)/lib/libboost_date_time-gcc.a       \
                  $(BOOSTDIR)/lib/libboost_filesystem-gcc.a           \
                  $(BOOSTDIR)/lib/libboost_program_options-gcc.a      \
                  $(BOOSTDIR)/lib/libboost_regex-gcc.a                \
                  /usr/lib/libg2c.so.0.0.0                            \
                   -lstdc++    -lz -lm                                 \
                  -lc -lgcc  -lgfortran                                \
                   -lrt

#F90 = pgf90
F90 = gfortran
FCC = g++
FC  = gcc

xmrgtoasc: $(objects)
	$(FCC) -o xmrgtoasc $(objects) $(LIBS)

.f.o :
	$(F90) $(F90FLAGS) -c $(fsources)
.cpp.o :
	$(FCC) $(CXXFLAGS) -c $(cppsources)
.c.o :
	$(FC) $(CFLAGS) -c $(csources)



.PHONY : clean
clean : 
	rm -f xmrgtoasc $(objects)

debug :
	$(FCC) -gDD -o xmrgtoasc $(objects)

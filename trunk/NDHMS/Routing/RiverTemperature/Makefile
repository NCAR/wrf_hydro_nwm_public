#FC=gfortran
FC=ifort

srcs=$(wildcard *.F)
objs=$(patsubst %.F,%.o,$(srcs))

mods=$(wildcard mod*.F)

mod=module_channel_routing_local.F module_lsm_forcing_local.F \
    module_sf_noahmpdrv_local.F weather_disaggrgate.F \
    module_river_temperature.F

mod_objs=$(patsubst %.F,%.o,$(mod))

FCFLAGS=-c -g
FFLAGS=-g

all: build

%.o: %.F
	$(FC) $(FCFLAGS) $^

%.mod: %.F
	$(FC) $(FCFLAGS) $^

build: $(mod_objs)
	$(FC) main.F $(FFLAGS) -o runMe.exe $(mod_objs)

debug:
	gdb ./runMe.exe

r: run
run:
	./runMe.exe


clean:
	rm -f *.o *.mod *.exe a.out *~

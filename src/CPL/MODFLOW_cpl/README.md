# MODFLOW Coupling


## Dependecies
In addition to the WRF-Hydro dependencies the following is needed to build and couple with MODFLOW.

| Libraries/Software | Version |
|--------------------|---------|
| Conda              |         |
| CMake              | 3.12+   |

### Dependecy Build Instructions
Make sure to first load the set of modules that are used to build WRF-Hydro.

```
$ cd [user_conda_env_path]
$ conda create --prefix ./envs
$ conda activate [user_conda_env_path]/envs
$ conda install meson
```

## Build
### Compilers
| Compiler           | Version |
|--------------------|---------|
| Classic Intel      | 2023+   |
| oneAPI Intel       | None    |
| GNU                | 12.4+   |

Note: PETSc, as of 07.02.2025 does not build with oneAPI Intel's icx and ifx.
The table above shows the compilers that have been tested, others may work.

#### Derecho Modules
```
ml ncarenv/24.12
ml conda
ml gcc
ml openmpi
ml netcdf
ml cmake
ml esmf
ml fftw
# the following modules can be found with
# $ ml use /glade/campaign/ral/hap/common/modflow/lmods
ml petsc/3.23.4-gnu
ml modflow/gnu-12.4.0
```

### Software
| Libraries/Software | Version |
|--------------------|---------|
| MODFLOW            | 6.6+    |
| PETSc              | 3.22.5  |

Obtain MODFLOW release or develop branch if desired. Note: release binaries will not work with coupling because Fortran modules are needed, not just the shared libraries.

Note: PETSc 3.23.4 was having issues building with MODFLOW using the GNU compilers.

```
Build PETSc
$ wget https://web.cels.anl.gov/projects/petsc/download/release-snapshots/petsc-3.22.tar.gz
$ tar zxf petsc-3.22.tar.gz
$ cd petsc-3.22.5
or
$ git clone -b release https://gitlab.com/petsc/petsc.git petsc
$ cd petsc

then to configure, build and install

$ ./configure \
  --with-debugging=1 \
  --with-cc=mpicc --with-cxx=mpicxx --with-fc=mpif90 \
  --download-fblaslapack=1 \
  --prefix=/path/to/install
$ make -j 4
$ make install
```

The meson install step does not currently the modules files that are needed
so an in-source installation is used.
```
Build MODFLOW
$ cd [MODFLOW base directory]
$ meson setup builddir -Dparallel=true -Dextended=true -Ddebug=true
$ meson compile -C builddir
$ export MODFLOW_BMI=[MODFLOW base directory]/builddir/srcbmi/

Build WRF-Hydro with MODFLOW Coupling
$ cd [WRF-Hydro base directory]
$ mkdir build
$ cd build
$ cmake .. -DMODFLOW=1
$ make -j 4
```

<!-- ### Prepare parallel case -->
<!-- Do domain decomposition on a serial MODFLOW domain to prepare for parallel -->
<!-- runs. -->
<!-- The dependencies of the `create_modflow_domain_decomposition.py` tool are -->
<!-- listed in `environment.yml`. -->

<!-- The tool will take the MODFLOW domain from the current directory and write -->
<!-- the new domain in the `split_domain_${np}np` directory. -->
<!-- The variable `np` is the number of processors the case will be run on and -->
<!-- is provided as a command line argument. -->

<!-- ``` -->
<!-- $ ./create_modflow_domain_decomposition.py -np 4 -->
<!-- ``` -->

### Run
Serial run:
```
$ ./wrfhydro_modflow
```

<!-- Parallel run, using the decomposed domain from above: -->
<!-- ``` -->
<!-- $ mpiexec -np 4 ./wrfhydro_modflow_parallel -->
<!-- ``` -->

#### NetCDF Output
In the `*.nam` files there is [an option](https://modflow6.readthedocs.io/en/latest/_mf6io/gwf-nam.html)
to enable NetCF output.

```
BEGIN options
  NETCDF_STRUCTURE FILEOUT modflow_subset_4.nc
END options
```

Build
===========


Requirements
~~~~~~~~~~~~

+------------+---------+
|Compiler    | Version |
+============+=========+
| GNU        | 7.4+    |
+------------+---------+
| Intel      | 2018+   |
+------------+---------+
| NVidia/PGI | 20.4+   |
+------------+---------+
| Cray       | 8+      |
+------------+---------+

+--------------------+---------+
| Libraries/Software | Version |
+====================+=========+
| MPI                | 3.x+    |
+--------------------+---------+
| Fortran NetCDF     | 4.4+    |
+--------------------+---------+
| CMake              | 3.12+   |
+--------------------+---------+


Install dependencies for Debain/Ubuntu
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. code-block:: bash

    $ apt install -y git cmake libnetcdff-dev mpi-default-dev


Install/activate dependencies for Red Hat/Fedora
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. code-block:: bash

    $ dnf install -y git cmake netcdf-fortran-mpich-devel
    <log out and back in to activate environment modules>
    $ module load mpi


CMake Build
~~~~~~~~~~~

.. code-block:: bash

    $ mkdir build
    $ cd build
    $ cmake ..
    $ make -j 4

The executables, namelists and tables are now in the ``build/Run`` directory.
Testcases with domain setups can be found `here <https://ral.ucar.edu/projects/wrf_hydro/testcases>`_.
To build with additional functionality, enter ``cmake .. -DFOO=1`` where the
available options are described in the following table.

+------------------------------------+-------------------------------------------------------------------------------+
| CMake WRF-Hydro Specific Options   | Functionality                                                                 |
+====================================+===============================================================================+
| ``-DWRF_HYDRO=1``                  | Default: turn on WRF-Hydro                                                    |
+------------------------------------+-------------------------------------------------------------------------------+
| ``-DHYDRO_D=1``                    | Enhanced diagnostic output for debugging                                      |
+------------------------------------+-------------------------------------------------------------------------------+
| ``-DSPATIAL_SOIL=1``               | Spatially distributed parameters for NoahMP                                   |
+------------------------------------+-------------------------------------------------------------------------------+
| ``-DWRF_HYDRO_NUDGING=1``          | Enable the streamflow nudging routines for Muskingum-Cunge Routing            |
+------------------------------------+-------------------------------------------------------------------------------+
| ``-DNWM_META=1``                   | Output NWM Metadata                                                           |
+------------------------------------+-------------------------------------------------------------------------------+
| ``-DOUTPUT_CHAN_CONN=1``           | For gridded channel routing, write the channel connectivity to a netcdf file  |
+------------------------------------+-------------------------------------------------------------------------------+
| ``-DPRECIP_DOUBLE=1``              | Double precipitation from hydro forcing                                       |
+------------------------------------+-------------------------------------------------------------------------------+
| ``-DNCEP_WCOSS=1``                 | Do not use unless working on the WCOSS machines                               |
+------------------------------------+-------------------------------------------------------------------------------+

+------------------------------------+-------------------------------------------------------------------------------+
| Unsupported Functionality          |                                                                               |
+====================================+===============================================================================+
| ``-DWRF_HYDRO_NUOPC=1``            | Coupling with NUOPC, this option is not currently supported                   |
+------------------------------------+-------------------------------------------------------------------------------+
| ``-DWRF_HYDRO_RAPID=1``            | Coupling with the RAPID routing model, this option is not currently supported |
+------------------------------------+-------------------------------------------------------------------------------+


CMake Testcase
~~~~~~~~~~~~~~

To download and setup the Croton testcase in ``build/Run`` use one of the
following commands.
The first time the ``croton.tar.gz`` file will be downloaded, extracted, and configured.
Future commands will reconfigure the ``Run`` directory.

+---------------------------------+
| Make Command                    |
+=================================+
| make croton                     |
+---------------------------------+
| makep croton-gridded            |
+---------------------------------+
| make croton-gridded-no-lakes    |
+---------------------------------+
| make croton-nwm                 |
+---------------------------------+
| make croton-nwm_ana             |
+---------------------------------+
| make croton-nwm_longe_range     |
+---------------------------------+
| make croton-reach               |
+---------------------------------+
| make croton-reach-lakes         |
+---------------------------------+

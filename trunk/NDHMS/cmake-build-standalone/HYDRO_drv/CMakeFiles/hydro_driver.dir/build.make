# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 2.8

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list

# Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The program to use to edit the cache.
CMAKE_EDIT_COMMAND = /usr/bin/ccmake

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone

# Include any dependencies generated for this target.
include HYDRO_drv/CMakeFiles/hydro_driver.dir/depend.make

# Include the progress variables for this target.
include HYDRO_drv/CMakeFiles/hydro_driver.dir/progress.make

# Include the compile flags for this target's objects.
include HYDRO_drv/CMakeFiles/hydro_driver.dir/flags.make

HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o: HYDRO_drv/CMakeFiles/hydro_driver.dir/flags.make
HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o: ../HYDRO_drv/module_HYDRO_drv.F
	$(CMAKE_COMMAND) -E cmake_progress_report /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o"
	cd /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/HYDRO_drv && /apps/intel/compilers_and_libraries_2018/linux/mpi/intel64/bin/mpif90  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/HYDRO_drv/module_HYDRO_drv.F -o CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o

HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o.requires:
.PHONY : HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o.requires

HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o.provides: HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o.requires
	$(MAKE) -f HYDRO_drv/CMakeFiles/hydro_driver.dir/build.make HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o.provides.build
.PHONY : HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o.provides

HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o.provides.build: HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o

# Object files for target hydro_driver
hydro_driver_OBJECTS = \
"CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o"

# External object files for target hydro_driver
hydro_driver_EXTERNAL_OBJECTS =

lib/libhydro_driver.a: HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o
lib/libhydro_driver.a: HYDRO_drv/CMakeFiles/hydro_driver.dir/build.make
lib/libhydro_driver.a: HYDRO_drv/CMakeFiles/hydro_driver.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking Fortran static library ../lib/libhydro_driver.a"
	cd /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/HYDRO_drv && $(CMAKE_COMMAND) -P CMakeFiles/hydro_driver.dir/cmake_clean_target.cmake
	cd /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/HYDRO_drv && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/hydro_driver.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
HYDRO_drv/CMakeFiles/hydro_driver.dir/build: lib/libhydro_driver.a
.PHONY : HYDRO_drv/CMakeFiles/hydro_driver.dir/build

HYDRO_drv/CMakeFiles/hydro_driver.dir/requires: HYDRO_drv/CMakeFiles/hydro_driver.dir/module_HYDRO_drv.F.o.requires
.PHONY : HYDRO_drv/CMakeFiles/hydro_driver.dir/requires

HYDRO_drv/CMakeFiles/hydro_driver.dir/clean:
	cd /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/HYDRO_drv && $(CMAKE_COMMAND) -P CMakeFiles/hydro_driver.dir/cmake_clean.cmake
.PHONY : HYDRO_drv/CMakeFiles/hydro_driver.dir/clean

HYDRO_drv/CMakeFiles/hydro_driver.dir/depend:
	cd /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/HYDRO_drv /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/HYDRO_drv /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/HYDRO_drv/CMakeFiles/hydro_driver.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : HYDRO_drv/CMakeFiles/hydro_driver.dir/depend


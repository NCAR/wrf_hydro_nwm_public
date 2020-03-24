# Install script for directory: /scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/usr/local")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

# Install shared libraries without execute permission?
IF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  SET(CMAKE_INSTALL_SO_NO_EXE "0")
ENDIF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/MPP/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/utils/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/IO/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/OrchestratorLayer/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/Debug_Utilities/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/Routing/Overland/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/Routing/Subsurface/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/Routing/Reservoirs/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/Data_Rec/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/Routing/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/HYDRO_drv/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/nudging/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/Land_models/NoahMP/cmake_install.cmake")
  INCLUDE("/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/CPL/NoahMP_cpl/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

IF(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
ELSE(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
ENDIF(CMAKE_INSTALL_COMPONENT)

FILE(WRITE "/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/${CMAKE_INSTALL_MANIFEST}" "")
FOREACH(file ${CMAKE_INSTALL_MANIFEST_FILES})
  FILE(APPEND "/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/${CMAKE_INSTALL_MANIFEST}" "${file}\n")
ENDFOREACH(file)

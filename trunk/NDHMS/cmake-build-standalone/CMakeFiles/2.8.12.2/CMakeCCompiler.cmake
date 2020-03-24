set(CMAKE_C_COMPILER "/apps/intel/compilers_and_libraries_2018/linux/mpi/intel64/bin/mpicc")
set(CMAKE_C_COMPILER_ARG1 "")
set(CMAKE_C_COMPILER_ID "Intel")
set(CMAKE_C_COMPILER_VERSION "18.0.0.20180823")
set(CMAKE_C_PLATFORM_ID "Linux")

set(CMAKE_AR "/usr/bin/ar")
set(CMAKE_RANLIB "/usr/bin/ranlib")
set(CMAKE_LINKER "/usr/bin/ld")
set(CMAKE_COMPILER_IS_GNUCC )
set(CMAKE_C_COMPILER_LOADED 1)
set(CMAKE_C_COMPILER_WORKS TRUE)
set(CMAKE_C_ABI_COMPILED TRUE)
set(CMAKE_COMPILER_IS_MINGW )
set(CMAKE_COMPILER_IS_CYGWIN )
if(CMAKE_COMPILER_IS_CYGWIN)
  set(CYGWIN 1)
  set(UNIX 1)
endif()

set(CMAKE_C_COMPILER_ENV_VAR "CC")

if(CMAKE_COMPILER_IS_MINGW)
  set(MINGW 1)
endif()
set(CMAKE_C_COMPILER_ID_RUN 1)
set(CMAKE_C_SOURCE_FILE_EXTENSIONS c)
set(CMAKE_C_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_C_LINKER_PREFERENCE 10)

# Save compiler ABI information.
set(CMAKE_C_SIZEOF_DATA_PTR "8")
set(CMAKE_C_COMPILER_ABI "ELF")
set(CMAKE_C_LIBRARY_ARCHITECTURE "")

if(CMAKE_C_SIZEOF_DATA_PTR)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_C_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_C_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_C_COMPILER_ABI}")
endif()

if(CMAKE_C_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "")
endif()




set(CMAKE_C_IMPLICIT_LINK_LIBRARIES "mpifort;mpi;mpigi;dl;rt;pthread;imf;svml;irng;m;ipgo;decimal;cilkrts;stdc++;irc;svml;c;irc_s;dl;c")
set(CMAKE_C_IMPLICIT_LINK_DIRECTORIES "/apps/intel/compilers_and_libraries_2018/linux/mpi/intel64/lib/release_mt;/apps/intel/compilers_and_libraries_2018/linux/mpi/intel64/lib;/apps/intel/parallel_studio_xe_2018.4.057/compilers_and_libraries_2018/linux/compiler/lib/intel64_lin;/apps/intel/parallel_studio_xe_2018.4.057/compilers_and_libraries_2018/linux/mkl/lib/intel64_lin;/apps/intel/parallel_studio_xe_2018.4.057/compilers_and_libraries_2018/linux/tbb/lib/intel64/gcc4.7;/apps/intel/parallel_studio_xe_2018.4.057/compilers_and_libraries_2018/linux/daal/lib/intel64_lin;/apps/intel/parallel_studio_xe_2018.4.057/compilers_and_libraries_2018/linux/tbb/lib/intel64_lin/gcc4.4;/apps/intel/compilers_and_libraries_2018.5.274/linux/compiler/lib/intel64_lin;/usr/lib/gcc/x86_64-redhat-linux/4.8.5;/usr/lib64;/lib64;/usr/lib;/lib")
set(CMAKE_C_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")




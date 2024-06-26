##################################################
# DMFTED PROJECT
##################################################
CMAKE_MINIMUM_REQUIRED(VERSION 3.0.0)
PROJECT(edipack2 Fortran)
SET(VERSION 0.4.7)


MESSAGE(STATUS "OS: ${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_VERSION}")
SET(BUILD_TYPE "RELEASE" CACHE STRING "Build type")
STRING(TOUPPER "${BUILD_TYPE}" BUILD_TYPE)
MESSAGE(STATUS "BUILD_TYPE=${BUILD_TYPE}")
SET(CMAKE_BUILD_TYPE ${BUILD_TYPE} CACHE INTERNAL "Build type" FORCE)

# Add our local CMAKE modules to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
INCLUDE(${CMAKE_MODULE_PATH}/ColorsMsg.cmake)

# Set some options the user may choose
OPTION(USE_MPI "Set MPI pre-compilation flag" ON)
OPTION(VERBOSE "Build a verbose Makefile" OFF)
OPTION(WITH_BLAS_LAPACK "Use internal Blas/Lapack" OFF)
IF(VERBOSE)
  SET(CMAKE_VERBOSE_MAKEFILE ON)
ELSE(VERBOSE)
  SET(CMAKE_VERBOSE_MAKEFILE OFF)
ENDIF(VERBOSE)



############################################################
# Look for MPI if needed
############################################################
INCLUDE(${CMAKE_MODULE_PATH}/MpiConfig.cmake)



############################################################
# Look for required libraries: scifor
# (includes Blas/Lapack/Scalapack/Quadpack/Minpack/FFTpack/Arpack/...) 
############################################################
# Find Scifor
FIND_PACKAGE(PkgConfig)
PKG_SEARCH_MODULE(SCIFOR REQUIRED scifor)
IF(${SCIFOR_FOUND})
  MESSAGE(STATUS "SCIFOR found at: ${SCIFOR_PREFIX}")
ELSE()
  MESSAGE(FATAL_ERROR "
Can not find SciFortran in this system. Check SciFor installation. Try adding the provided PKG-CONFIG file to the environment:
$ export PKG_CONFIG_PATH=<path_to_scifor_dir>/etc:$PKG_CONFIG_PATH")
ENDIF()
# #THIS MAY BE REMOVED I GUESS. IT IS A WAY TOO MUCH JUDICIOUS CROSSCHECK OF SCIFOR CONFIGURATION
# IF("${SCIFOR_LDFLAGS}" MATCHES "blas" OR "lapack")
#   FIND_PACKAGE(BLAS REQUIRED)
#   FIND_PACKAGE(LAPACK REQUIRED)
# ELSEIF("${SCIFOR_LDFLAGS}" MATCHES "mkl")
#   FIND_PACKAGE(MKL QUIET)
#   IF( ${MKL_FOUND} )
#     MESSAGE(STATUS "MKL detected in SciFortan. Found.")
#   ELSE()
#     MESSAGE(WARNING "MKL detected in SciFortran. Not found. Check installation/loading") 
#   ENDIF()
# ENDIF()
INCLUDE_DIRECTORIES(BEFORE ${SCIFOR_INCLUDE_DIRS})


############################################################
# Configure Fortran Compiler Main options
############################################################
INCLUDE(${CMAKE_MODULE_PATH}/FortranCompilerConfig.cmake)




############################################################
# Check for .git and extract the current SHA1 revision.
############################################################
INCLUDE(${CMAKE_MODULE_PATH}/GitConfig.cmake)




############################################################
# Define the actual files and folders that make up the build
############################################################
INCLUDE(${CMAKE_MODULE_PATH}/MainConfig.cmake)



############################################################
############################################################
############################################################
# DEFINE HERE THE DETAILS OF THE LIBRARY CONFIGURATION 

# Define some directories
SET(LIB_SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB_ETC ${CMAKE_SOURCE_DIR}/etc)
SET(LIB_ENV ${CMAKE_SOURCE_DIR}/etc/environment_modules)
SET(LIB_TMP_INC ${CMAKE_BINARY_DIR}/include)
SET(LIB_TMP_ETC ${CMAKE_BINARY_DIR}/etc)
SET(LIB_TMP_VER ${CMAKE_BINARY_DIR}/version)
SET(LIB_TARGET_LIB ${CMAKE_INSTALL_PREFIX}/lib)
SET(LIB_TARGET_INC ${CMAKE_INSTALL_PREFIX}/include)
SET(LIB_TARGET_ETC ${CMAKE_INSTALL_PREFIX}/etc)
SET(LIB_TARGET_BIN ${CMAKE_INSTALL_PREFIX}/bin)
SET(LIB_TARGET_DIR ${CMAKE_INSTALL_PREFIX})

# Have the .mod files placed in the INC folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB_TMP_INC})
INCLUDE_DIRECTORIES(BEFORE ${CMAKE_Fortran_MODULE_DIRECTORY})


ADD_SUBDIRECTORY(${LIB_SRC}/ED_BATH)

ADD_SUBDIRECTORY(${LIB_SRC}/ED_IO)

ADD_SUBDIRECTORY(${LIB_SRC}/ED_NORMAL)

ADD_SUBDIRECTORY(${LIB_SRC}/ED_SUPERC)

ADD_SUBDIRECTORY(${LIB_SRC}/ED_NONSU2)

ADD_SUBDIRECTORY(${LIB_SRC})

ADD_LIBRARY(edipack2 STATIC
  $<TARGET_OBJECTS:ED_INIT_LIB>
  $<TARGET_OBJECTS:ED_BATH_LIB>
  $<TARGET_OBJECTS:ED_IO_LIB>
  $<TARGET_OBJECTS:ED_NORMAL_LIB>
  $<TARGET_OBJECTS:ED_SUPERC_LIB>
  $<TARGET_OBJECTS:ED_NONSU2_LIB>
  $<TARGET_OBJECTS:ED_GLOB_LIB>
  )


ADD_DEPENDENCIES(ED_BATH_LIB ED_INIT_LIB)
ADD_DEPENDENCIES(ED_IO_LIB ED_BATH_LIB)
ADD_DEPENDENCIES(ED_NORMAL_LIB ED_INIT_LIB ED_BATH_LIB ED_IO_LIB)
ADD_DEPENDENCIES(ED_SUPERC_LIB ED_INIT_LIB ED_BATH_LIB ED_IO_LIB)
ADD_DEPENDENCIES(ED_NONSU2_LIB ED_INIT_LIB ED_BATH_LIB ED_IO_LIB)


ADD_DEPENDENCIES(ED_GLOB_LIB
  ED_INIT_LIB
  ED_BATH_LIB
  ED_IO_LIB
  ED_NORMAL_LIB
  ED_SUPERC_LIB
  ED_NONSU2_LIB
  )


SET(PERMISSION_777
  OWNER_WRITE OWNER_READ OWNER_EXECUTE
  GROUP_WRITE GROUP_READ GROUP_EXECUTE
  WORLD_WRITE WORLD_READ WORLD_EXECUTE)



#Build the PKG-CONFIG file
SET(TMP_PKCONFIG_FILE ${LIB_TMP_ETC}/${PROJECT_NAME}.pc)
CONFIGURE_FILE( ${LIB_ETC}/${PROJECT_NAME}.pc.in ${TMP_PKCONFIG_FILE} @ONLY)

############################################################
############################################################
############################################################


INCLUDE(${CMAKE_MODULE_PATH}/PostConfig.cmake)

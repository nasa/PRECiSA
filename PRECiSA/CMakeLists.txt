cmake_minimum_required(VERSION 3.5)

project(PRECiSA)

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
MESSAGE(STATUS "$CMAKE_MODULE_PATH: " ${CMAKE_MODULE_PATH})

find_package(Haskell REQUIRED)

IF (DEFINED ENV{KODIAK_LIB})
  MESSAGE(STATUS "$KODIAK_LIB environment defined: " $ENV{KODIAK_LIB})
ELSE (DEFINED ENV{KODIAK_LIB})
  MESSAGE(FATAL_ERROR "$KODIAK_LIB environment variable is not defined")
ENDIF (DEFINED ENV{KODIAK_LIB})

FIND_PATH(KODIAK_LIBRARY_DIR
  NAMES
  libkodiak.so
  libkodiak.dylib
  PATHS
  $ENV{KODIAK_LIB}
  )

IF (NOT KODIAK_LIBRARY_DIR)
  MESSAGE(FATAL_ERROR "Kodiak library directory could not be found")
ENDIF ()

set(PRECISA_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

set(haskell_sources
  src/AbsPVSLang.hs
  src/AbsSpecLang.hs
  src/AbstractDomain.hs
  src/AbstractSemantics.hs
  src/Common/ControlFlow.hs
  src/Common/DecisionPath.hs
  src/ErrM.hs
  src/FPrec.hs
  src/Kodiak/Kodiak.hs
  src/Kodiak/KodiakRunnable.hs
  src/Kodiak/KodiakRunner.hs
  src/Main.hs
  src/Parser/LexDecisionPaths.hs
  src/Parser/LexPVSLang.hs
  src/Parser/LexSpecLang.hs
  src/Parser/ParDecisionPaths.hs
  src/Parser/ParPVSLang.hs
  src/Parser/ParSpecLang.hs
  src/PPExt.hs
  src/Utils.hs
  )

set(PRECISA_EXECUTABLE precisa)
set(CABAL_STAMP precisa_cabal.stamp)

set(haskell_artifacts ${CABAL_STAMP} ${PRECISA_EXECUTABLE})
# Adding *.hi files so that any missing file triggers the build
foreach(SRC ${haskell_sources})
  get_filename_component(EX ${SRC} EXT)
  if(${EX} STREQUAL ".hs")
    file(RELATIVE_PATH REL ${PRECISA_SOURCE_DIR}/src ${CMAKE_CURRENT_SOURCE_DIR}/${SRC})
    get_filename_component(DIR ${REL} DIRECTORY)
    get_filename_component(BASE ${REL} NAME_WE)
    if("${DIR}" STREQUAL "")
      list(APPEND haskell_artifacts ${CMAKE_CURRENT_BINARY_DIR}/dist/build/precisa/precisa-tmp/${BASE}.hi)
    else()
      list(APPEND haskell_artifacts ${CMAKE_CURRENT_BINARY_DIR}/dist/build/precisa/precisa-tmp/${DIR}/${BASE}.hi)
    endif()
  endif()
endforeach()

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(hs_optimize -O0)
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
  set(hs_optimize -O1)
endif()

set(hs_extra_libraries
  "--ghc-option=-optl-Wl,-rpath,${KODIAK_LIBRARY_DIR}"
  "--extra-lib-dirs=${KODIAK_LIBRARY_DIR}"
  )

MESSAGE(STATUS "hs_extra_libraries: " ${hs_extra_libraries})
add_custom_command(
  OUTPUT ${haskell_artifacts}
  COMMAND ${CABAL} update
  # Build dependencies first without --builddir, otherwise it fails.
  COMMAND ${CABAL} install --only-dependencies ${hs_enable_test}
  COMMAND ${CABAL} configure ${hs_optimize} ${hs_enable_test} ${hs_extra_libraries} --builddir=${CMAKE_CURRENT_BINARY_DIR}/dist
  COMMAND ${CABAL} build --builddir=${CMAKE_CURRENT_BINARY_DIR}/dist
  COMMAND ${CABAL} install ${hs_extra_libraries} --builddir=${CMAKE_CURRENT_BINARY_DIR}/dist --bindir=${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_BINARY_DIR}/${CABAL_STAMP}
  WORKING_DIRECTORY ${PRECISA_SOURCE_DIR}
  DEPENDS ${haskell_sources}
  COMMENT "Building PRECiSA Executable"
  )

add_custom_target(
  PRECiSA ALL
  DEPENDS ${haskell_artifacts}
  )

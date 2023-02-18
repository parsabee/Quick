##===----------------------------------------------------------------------===##
# This file finds and/or installs Google Test
#
# Copyright (c) 2023 Parsa Bagheri
##===----------------------------------------------------------------------===##

include(ExternalProject)

# Install gtest version 'gtest_version' in 'install_path'
function(quick_install_gtest gtest_version install_path)
  message(STATUS "installing gtest ${gtest_version} in ${install_path}")

  # Set the download URL
  set(GTEST_URL "https://github.com/google/googletest/archive/refs/tags/release-${gtest_version}.tar.gz")

  set(GTEST_TARBALL ${CMAKE_BINARY_DIR}/gtest-${gtest_version}.tar.gz)
  set(GTEST_UNZIPPED ${CMAKE_BINARY_DIR}/googletest-release-${gtest_version})

  # Download and install GTest using ExternalProject
  add_custom_target(GTEST_TARBALL_TARGET DEPENDS ${GTEST_TARBALL})
  if (NOT EXISTS ${GTEST_TARBALL})
    file(DOWNLOAD
      ${GTEST_URL}
      ${GTEST_TARBALL}
      SHOW_PROGRESS)
  endif()
  add_custom_target(GTEST_UNZIPPED_TARGET DEPENDS ${GTEST_UNZIPPED})
  add_dependencies(GTEST_UNZIPPED_TARGET GTEST_TARBALL_TARGET)
  file(MAKE_DIRECTORY ${install_path})
  execute_process(COMMAND ${CMAKE_COMMAND} -E tar xf ${GTEST_TARBALL}
      RESULT_VARIABLE result
      OUTPUT_VARIABLE output
      ERROR_VARIABLE error
      WORKING_DIRECTORY ${install_path})
  if(result)
    message(FATAL_ERROR "failed to extract ${GTEST_TARBALL}: ${result} ${output} ${error}")
  endif()

  # Add GTest files to this project
  set(GTEST_INSTALL_DIR ${install_path}/googletest-release-${gtest_version}/)
  set(GTEST_BIN_DIR ${GTEST_INSTALL_DIR}/build)
  file(MAKE_DIRECTORY ${GTEST_BIN_DIR})
  add_subdirectory(
      ${GTEST_INSTALL_DIR}
      ${GTEST_BIN_DIR}
      EXCLUDE_FROM_ALL)

  # Set the install directory
  set(QUICK_GTEST_INSTALL_DIR ${GTEST_INSTALL_DIR} PARENT_SCOPE)
  set(QUICK_GTEST_INCLUDE_DIR "${GTEST_INSTALL_DIR}/googletest/include" PARENT_SCOPE)
  include_directories(${GTEST_INSTALL_DIR}/googletest/include)
  set(QUICK_GTEST_LIBRARY "${CMAKE_BINARY_DIR}/lib/libgtestd.a" PARENT_SCOPE)
  message(STATUS "GTest installed in ${install_path}/googletest-release-${gtest_version}/")
endfunction()
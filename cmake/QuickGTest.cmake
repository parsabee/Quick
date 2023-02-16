##===----------------------------------------------------------------------===##
# This file finds and/or installs Google Test
#
# Copyright (c) 2023 Parsa Bagheri
##===----------------------------------------------------------------------===##

include(ExternalProject)

# Check gtest version 'gtest_version' exists in 'install_path'
function(quick_check_gtest gtest_version install_path)
  set(GTEST_INCLUDE_DIR "${QUICK_GTEST_INSTALL_DIR}/include")
  set(GTEST_LIBRARY "${QUICK_GTEST_INSTALL_DIR}/lib/libgtest.a")
  set(GTEST_MAIN_LIBRARY "${QUICK_GTEST_INSTALL_DIR}/lib/libgtest_main.a")
  set(GTEST_FOUND ON)
  if (NOT EXISTS ${QUICK_GTEST_INCLUDE_DIR})
    set(GTEST_FOUND OFF)
  endif()
  if (NOT EXISTS ${QUICK_GTEST_LIBRARY})
    set(GTEST_FOUND OFF)
  endif()
  if (NOT EXISTS ${QUICK_GTEST_MAIN_LIBRARY})
    set(GTEST_FOUND OFF)
  endif()
  if (GTEST_FOUND)
    set(QUICK_GTEST_INCLUDE_DIR ${GTEST_INCLUDE_DIR} PARENT_SCOPE)
    set(QUICK_GTEST_LIBRARY ${GTEST_LIBRARY} PARENT_SCOPE)
    set(QUICK_GTEST_MAIN_LIBRARY ${GTEST_MAIN_LIBRARY} PARENT_SCOPE)
    message(STATUS "found gtest ${QUICK_GTEST_INSTALL_DIR}")
  endif()
endfunction()

# Install gtest version 'gtest_version' in 'install_path'
function(quick_install_gtest gtest_version install_path)
  message(STATUS "installing gtest ${gtest_version} in ${install_path}")

  # Set the download URL
  set(GTEST_URL "https://github.com/google/googletest/archive/refs/tags/release-${gtest_version}.tar.gz")

  # Set the install directory
  set(QUICK_GTEST_INSTALL_DIR ${install_path} PARENT_SCOPE)

  # Download and install GTest using ExternalProject
  ExternalProject_Add(
      QUICK_GTEST_TARGET
      PREFIX ${QUICK_GTEST_INSTALL_DIR}
      URL ${GTEST_URL}
      CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${QUICK_GTEST_INSTALL_DIR}
  )

  # Add GTest include and library directories to the project
  set(QUICK_GTEST_INCLUDE_DIR "${QUICK_GTEST_INSTALL_DIR}/include" PARENT_SCOPE)
  set(QUICK_GTEST_LIBRARY "${QUICK_GTEST_INSTALL_DIR}/lib/libgtest.a" PARENT_SCOPE)
  set(QUICK_GTEST_MAIN_LIBRARY "${QUICK_GTEST_INSTALL_DIR}/lib/libgtest_main.a" PARENT_SCOPE)
  include_directories(${QUICK_GTEST_INCLUDE_DIR})
  link_libraries(${QUICK_GTEST_LIBRARY} ${QUICK_GTEST_MAIN_LIBRARY})
  message(STATUS "GTest installed in ${QUICK_GTEST_INSTALL_DIR}")
  add_dependencies(${QUICK_EXECUTABLE} QUICK_GTEST_TARGET)
endfunction()
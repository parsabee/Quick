##===----------------------------------------------------------------------===##
# Root cmake file, sets up the project
#
# Copyright (c) 2023 Parsa Bagheri
##===----------------------------------------------------------------------===##

# Configuring the project
project(Quick)
cmake_minimum_required(VERSION 3.18)
set(CMAKE_CXX_VISIBILITY_PRESET default)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti -fno-exceptions")
set(QUICK_BIN_DIR ${CMAKE_BINARY_DIR}/bin)
set(QUICK_LIB_DIR ${CMAKE_BINARY_DIR}/lib)
file(MAKE_DIRECTORY ${QUICK_BIN_DIR})
file(MAKE_DIRECTORY ${QUICK_LIB_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${QUICK_BIN_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${QUICK_LIB_DIR})
set(QUICK_EXECUTABLE qk)

# Adding Python3
set(quick_python_virtual_env ${CMAKE_BINARY_DIR}/venv)
include(cmake/QuickPython.cmake)

# Bison dependency
set(quick_bison_version "3.7.1")
set(quick_bison_install_dir ${CMAKE_BINARY_DIR}/third-party/bison)
include(cmake/QuickBison.cmake)

# Flex dependency
set(quick_flex_version "2.6.4")
set(quick_flex_install_dir ${CMAKE_BINARY_DIR}/third-party/flex)
include(cmake/QuickFlex.cmake)

# LLVM dependency
set(quick_llvm_version "10.0.0")
set(quick_llvm_install_dir ${CMAKE_BINARY_DIR}/third-party/llvm)
include(cmake/QuickLLVM.cmake)

# GTest dependency
set(quick_gtest_version "1.11.0")
set(quick_gtest_install_dir ${CMAKE_BINARY_DIR}/third-party/gtest)
include(cmake/QuickGTest.cmake)

# Configuring the system
# Platform OS
if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
  message(STATUS "Platform: '${CMAKE_SYSTEM_NAME}'")
  set(quick_llvm_platform "apple-darwin")
elseif (CMAKE_SYSTEM_NAME MATCHES "Linux")
  message(STATUS "Platform: '${CMAKE_SYSTEM_NAME}'")
  set(quick_llvm_platform "linux")
else ()
  message(FATAL_ERROR "Platform '${CMAKE_SYSTEM_NAME}' is not supported yet!")
endif ()

# Architecture
if (CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64")
  message(STATUS "Architecture: '${CMAKE_SYSTEM_PROCESSOR}'")
  set(quick_llvm_target "x86_64")
else ()
  message(FATAL_ERROR "Architecture '${CMAKE_SYSTEM_PROCESSOR}' is not supported yet!")
endif ()

# Check Bison
quick_check_bison(${quick_bison_version} ${quick_bison_install_dir})
if (NOT QUICK_BISON_EXECUTABLE)
  quick_install_bison(${quick_bison_version} ${quick_bison_install_dir})
endif ()

# Check Flex
quick_check_flex(${quick_flex_version} ${quick_flex_install_dir})
if (NOT QUICK_FLEX_EXECUTABLE)
  quick_install_flex(${quick_flex_version} ${quick_flex_install_dir})
endif ()

# Check LLVM
quick_check_llvm_toolchain(${quick_llvm_target} ${quick_llvm_version} ${quick_llvm_install_dir})
if (NOT QUICK_LLVM_EXECUTABLE)
  quick_install_prebuilt_llvm(${quick_llvm_platform} ${quick_llvm_target} ${quick_llvm_version} ${quick_llvm_install_dir})
endif ()

# Install GTest
quick_install_gtest(${quick_gtest_version} ${quick_gtest_install_dir})

# Create virtual env and install pip packages
quick_create_python_virtual_env(${quick_python_virtual_env})
quick_pip_install_executable("lit")
quick_pip_install_executable("filecheck")

# Include paths
include_directories(
    ${QUICK_LLVM_INCLUDE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR}/lib)

link_directories(
    ${QUICK_LLVM_LIB_DIR})

# Compiling the project
add_subdirectory(lib)

# Tests
add_subdirectory(test)
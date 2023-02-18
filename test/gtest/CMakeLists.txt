##===----------------------------------------------------------------------===##
# GTest setup
#
# Copyright (c) 2023 Parsa Bagheri
##===----------------------------------------------------------------------===##

file(MAKE_DIRECTORY ${QUICK_BIN_DIR}/test)

# Setting bin directory to bin/test
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${QUICK_BIN_DIR}/test)

# Compiling tests
add_subdirectory(AST)

# Resetting bin directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${QUICK_BIN_DIR})


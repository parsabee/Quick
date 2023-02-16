##===----------------------------------------------------------------------===##
# This file finds Python3 and defines functions to create virtual environments
# and install packages with pip
#
# Copyright (c) 2023 Parsa Bagheri
##===----------------------------------------------------------------------===##

find_package(Python3 REQUIRED COMPONENTS Interpreter Development)
if (Python3_FOUND)
  message(STATUS "Python3 found: ${Python3_EXECUTABLE}")
else()
  message(FATAL_ERROR "Python3 not found")
endif()

# Creates a python3 virtual environment
function(quick_create_python_virtual_env env_dir)
  execute_process(
      COMMAND ${Python3_EXECUTABLE} -m venv ${env_dir}
      RESULT_VARIABLE result
      OUTPUT_VARIABLE output
      ERROR_VARIABLE error
  )

  if (result)
    message(FATAL_ERROR "Error creating virtual environment: ${error}")
  else()
    message(STATUS "Virtual environment created at ${env_dir}")
  endif()
  add_custom_target(QUICK_PYTHON_ENV_TARGET DEPENDS ${env_dir})
  set(QUICK_VENV_BIN_DIR ${env_dir}/bin PARENT_SCOPE)
  set(QUICK_PIP_EXECUTABLE ${env_dir}/bin/pip3 PARENT_SCOPE)
endfunction()

# Installs tools with pip
function(quick_pip_install_executable package)
  execute_process(
      COMMAND ${QUICK_PIP_EXECUTABLE} install ${package}
      RESULT_VARIABLE result
      OUTPUT_VARIABLE output
      ERROR_VARIABLE error
  )
  if(result)
    message(FATAL_ERROR "Failed to install ${package}: ${error}")
  endif()
  message(STATUS "Installed ${package}")
  set(QUICK_${package}_EXECUTABLE ${QUICK_VENV_BIN_DIR}/${package} PARENT_SCOPE)
  add_custom_target(QUICK_${package}_TARGET
      DEPENDS
      ${QUICK_${package}_EXECUTABLE}
      QUICK_PYTHON_ENV_TARGET
      )
endfunction()
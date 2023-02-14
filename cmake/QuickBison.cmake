include(ExternalProject)

# Checks if `bison_version` is installed in `install_path`
# sets `QUICK_BISON_FOUND` accordingly, and `QUICK_BISON_EXECUTABLE` to the binary
# if `bison_version` is found
function(quick_check_bison bison_version install_path)
  message(STATUS "checking bison version ${bison_version}...")
  if (NOT EXISTS ${install_path}/bison-${bison_version}/bin/bison)
    message(STATUS "bison executable not found!")
  else()
    set(bison_executable ${install_path}/bison-${bison_version}/bin/bison)
    message(STATUS "found ${bison_executable}")
    execute_process(COMMAND "${bison_executable}" --version
      OUTPUT_VARIABLE QUICK_BISON_VERSION_OUTPUT
      ERROR_QUIET)
    string(REGEX MATCH "([0-9]+.[0-9]+.[0-9]+)" QUICK_BISON_VERSION "${QUICK_BISON_VERSION_OUTPUT}")
    if("${QUICK_BISON_VERSION}" STREQUAL "${bison_version}")
      message(STATUS "bison ${bison_version} executable found!")
      set(QUICK_BISON_EXECUTABLE ${bison_executable} PARENT_SCOPE)
      set(QUICK_BISON_EXECUTABLE_DIR "${install_path}/bison-${bison_version}/bin" PARENT_SCOPE)
      add_custom_target(QUICK_BISON_TARGET DEPENDS ${QUICK_BISON_EXECUTABLE})
    else()
      message(STATUS "bison ${bison_version} not found!")
    endif()
  endif()
endfunction()

# Downloads and installs `bison_version` in `install_path`
function(quick_install_bison bison_version install_path)
  set(bison_url "https://ftp.gnu.org/gnu/bison/bison-${bison_version}.tar.gz")
  message(STATUS "installing bison version ${bison_version}...")
  ExternalProject_Add(bison_external
    URL ${bison_url}
    PREFIX ${install_path}/bison-${bison_version}
    BUILD_IN_SOURCE 1
    CONFIGURE_COMMAND ./configure --prefix=${install_path}/bison-${bison_version}
    BUILD_COMMAND make
    INSTALL_COMMAND make install
  )
  add_custom_target(QUICK_BISON_TARGET DEPENDS bison_external)
  set(QUICK_BISON_EXECUTABLE "${install_path}/bison-${bison_version}/bin/bison" PARENT_SCOPE)
  set(QUICK_BISON_EXECUTABLE_DIR "${install_path}/bison-${bison_version}/bin" PARENT_SCOPE)
endfunction()

# Generates ${bison_input_file}.cpp and ${bison_input_file}.hpp,
# then assigns macros QUICK_BISON_GENERATED_FILE and QUICK_BISON_GENERATED_HEADER
# to each file respectively
function(quick_run_bison bison_input_file)
  message(STATUS "generating bison target ...")
  get_filename_component(input_file_name ${bison_input_file} NAME)
  set(bison_output_file ${CMAKE_BINARY_DIR}/${input_file_name}.cpp)
  set(bison_output_header ${CMAKE_BINARY_DIR}/${input_file_name}.hpp)
  add_custom_command(
    OUTPUT ${bison_output_file}
    COMMAND ${QUICK_BISON_EXECUTABLE}
    ARGS -o ${bison_output_file} --defines=${bison_output_header} ${bison_input_file}
    DEPENDS ${bison_input_file} QUICK_BISON_TARGET
  )
  add_custom_target(generated_bison_file DEPENDS "${bison_output_file}")
  add_dependencies(generated_bison_file QUICK_BISON_TARGET)
  set(QUICK_BISON_GENERATED_FILE ${bison_output_file} PARENT_SCOPE)
  set(QUICK_BISON_GENERATED_HEADER ${bison_output_header} PARENT_SCOPE)
  message(STATUS "bison target generated!")
endfunction()

# Links with bison libraries
function(quick_link_bison_libraries target bison_version install_path)
  target_link_directories(${target} PRIVATE ${install_path}/bison-${bison_version}/lib)
endfunction()
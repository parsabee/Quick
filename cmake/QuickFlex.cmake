include(ExternalProject)

# Installs flex 'version' in 'path'
function(quick_install_flex flex_version path)
  set(flex_url "https://github.com/westes/flex/releases/download/v${flex_version}/flex-${flex_version}.tar.gz")
  message(STATUS "installing flex version ${flex_version}...")
  ExternalProject_Add(flex
    PREFIX ${path}
    URL ${flex_url}
    CONFIGURE_COMMAND ${path}/src/flex/configure --prefix=${path}
    BUILD_COMMAND make
    INSTALL_COMMAND make install
  )
  set(QUICK_FLEX_EXECUTABLE "${path}/bin/flex" PARENT_SCOPE)
  set(QUICK_FLEX_EXECUTABLE_DIR "${path}/bin" PARENT_SCOPE)
  add_custom_target(QUICK_FLEX_TARGET DEPENDS flex)
endfunction()

# Checks if flex exists in 'path'
function(quick_check_flex flex_version path)
  set(flex_bin ${path}/bin/flex)
  message(STATUS "checking flex version ${flex_version}...")
  if(NOT EXISTS ${flex_bin})
    message(STATUS "flex executable not found!")
  else()
    message(STATUS "found ${flex_bin}")
    execute_process(COMMAND "${flex_bin}" --version
      OUTPUT_VARIABLE FLEX_VERSION_OUTPUT
      ERROR_QUIET)
    string(REGEX MATCH "([0-9]+.[0-9]+.[0-9]+)" FLEX_VERSION "${FLEX_VERSION_OUTPUT}")
    if("${FLEX_VERSION}" STREQUAL "${flex_version}")
      add_custom_target(QUICK_FLEX_TARGET DEPENDS ${flex_bin})
      message(STATUS "flex ${flex_version} executable found!")
      set(QUICK_FLEX_EXECUTABLE ${flex_bin} PARENT_SCOPE)
    else()
      message(STATUS "flex ${flex_version} not found!")
    endif()
  endif()
endfunction()

# Generates QUICK_FLEX_GENERATED_FILE ${input_file_name}.cpp
function(quick_run_flex flex_input_file)
  message(STATUS "generating flex target with: ${QUICK_FLEX_EXECUTABLE} ...")
  get_filename_component(input_file_name ${flex_input_file} NAME)
  set(flex_output_file ${CMAKE_BINARY_DIR}/${input_file_name}.cpp)
  add_custom_command(
    OUTPUT ${flex_output_file}
    COMMAND ${QUICK_FLEX_EXECUTABLE}
    ARGS -o ${flex_output_file} ${flex_input_file}
    DEPENDS ${flex_input_file} QUICK_FLEX_TARGET
  )
  add_custom_target(flex_generated_file DEPENDS ${flex_output_file})
  add_dependencies(flex_generated_file QUICK_FLEX_TARGET)
  set(QUICK_FLEX_GENERATED_FILE ${flex_output_file} PARENT_SCOPE)
  message(STATUS "flex target generated!")
endfunction()

# Links with flex libraries
function(quick_link_flex_libraries target flex_version install_path)
  target_link_directories(${target} PRIVATE ${install_path}/lib)
endfunction()
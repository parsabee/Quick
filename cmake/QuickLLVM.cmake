##===----------------------------------------------------------------------===##
# This file finds and/or installs LLVM
#
# Copyright (c) 2023 Parsa Bagheri
##===----------------------------------------------------------------------===##

# Sets macros for llvm link flags and libraries
macro(quick_llvm_config llvm_config)
  message(STATUS "getting llvm linker flags")
  execute_process(COMMAND ${llvm_config} --ldflags
                  OUTPUT_VARIABLE LLVM_LDFLAGS
                  RESULT_VARIABLE result
                  ERROR_VARIABLE error
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                  )
  if(result)
    message(FATAL_ERROR "failed to run ${llvm_config} --ldflags")
  endif()
  execute_process(COMMAND ${llvm_config} --system-libs
    OUTPUT_VARIABLE LLVM_SYSTEMLIBS
    RESULT_VARIABLE result
    ERROR_VARIABLE error
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  if(result)
    message(FATAL_ERROR "failed to run ${llvm_config} --system-libs")
  endif()
  execute_process(COMMAND ${llvm_config} --libs
    OUTPUT_VARIABLE LLVM_LIBS
    RESULT_VARIABLE result
    ERROR_VARIABLE error
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  if(result)
    message(FATAL_ERROR "failed to run ${llvm_config} --libs")
  endif()
  set(QUICK_LLVM_LDFLAGS ${LLVM_LDFLAGS} PARENT_SCOPE)
  set(QUICK_LLVM_SYSTEMLIBS ${LLVM_SYSTEMLIBS} PARENT_SCOPE)
  set(QUICK_LLVM_LIBS ${LLVM_LIBS} PARENT_SCOPE)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LLVM_LDFLAGS}" PARENT_SCOPDE)
endmacro()

# Checks if llvm `version` is installed in `install_path`
function(quick_check_llvm_toolchain arch llvm_version install_path)
  set(LLVM_INSTALL_DIR "${install_path}/llvm-${llvm_version}")
  if(EXISTS "${LLVM_INSTALL_DIR}/bin/llvm-config")
    quick_llvm_config(${LLVM_INSTALL_DIR}/bin/llvm-config)
    set(QUICK_LLVM_EXECUTABLE_DIR ${LLVM_INSTALL_DIR}/bin PARENT_SCOPE)
    set(QUICK_LLVM_FOUND ON)
  else()
    message(STATUS "llvm binaries not found!")
    set(QUICK_LLVM_FOUND OFF)
  endif()

  if(EXISTS "${LLVM_INSTALL_DIR}/include")
    set(QUICK_LLVM_INCLUDE_DIR ${LLVM_INSTALL_DIR}/include PARENT_SCOPE)
    set(QUICK_LLVM_FOUND ON)
  else()
    message(STATUS "llvm headers not found!")
    set(QUICK_LLVM_FOUND OFF)
  endif()

  if (EXISTS "${LLVM_INSTALL_DIR}/lib")
    set(QUICK_LLVM_LIB_DIR ${LLVM_INSTALL_DIR}/lib PARENT_SCOPE)
    set(QUICK_LLVM_FOUND ON)
  else()
    message(STATUS "llvm libraries not found!")
    set(QUICK_LLVM_FOUND OFF)
  endif()

  if (QUICK_LLVM_FOUND)
    set(QUICK_LLVM_EXECUTABLE ${LLVM_INSTALL_DIR}/bin/llc PARENT_SCOPE)
    message(STATUS "found llvm ${LLVM_INSTALL_DIR}")
  endif()
endfunction()

# Downloads prebuilt tarball of llvm `version` and extracts it in `install_path`
function(quick_install_prebuilt_llvm platform arch llvm_version install_path)
  set(LLVM_URL "https://github.com/llvm/llvm-project/releases/download/llvmorg-${llvm_version}/clang+llvm-${llvm_version}-${arch}-${platform}.tar.xz")
  set(llvm_untarred "${install_path}/clang+llvm-${llvm_version}-${arch}-${platform}")
  set(llvm_tar_ball "${llvm_untarred}.tar.xz")
  set(LLVM_INSTALL_DIR "${install_path}/llvm-${llvm_version}")
  file(MAKE_DIRECTORY ${install_path})
  add_custom_target(LLVM_TAR_BALL_TARGET DEPENDS ${llvm_tar_ball})
  message(STATUS "downloading from ${LLVM_URL}")
  if (NOT EXISTS ${llvm_tar_ball})
    file(DOWNLOAD 
        ${LLVM_URL} 
        ${llvm_tar_ball}
        SHOW_PROGRESS)
    # TODO: verify checksum
  endif()
  add_custom_target(LLVM_UNZIPPED_TARGET DEPENDS ${LLVM_INSTALL_DIR})
  add_dependencies(LLVM_UNZIPPED_TARGET LLVM_TAR_BALL_TARGET)
  if (NOT EXISTS ${LLVM_INSTALL_DIR}/bin)
    execute_process(COMMAND "${CMAKE_COMMAND}" -E tar xf "${llvm_tar_ball}"
                    RESULT_VARIABLE result
                    OUTPUT_VARIABLE output
                    ERROR_VARIABLE error
                    WORKING_DIRECTORY ${install_path})
    if(result)
      message(FATAL_ERROR "failed to extract ${llvm_tar_ball}: ${result} ${output} ${error}")
    endif()
    file(RENAME ${llvm_untarred} ${LLVM_INSTALL_DIR})
  endif()
  quick_llvm_config(${LLVM_INSTALL_DIR}/bin/llvm-config)
  set(QUICK_LLVM_EXECUTABLE_DIR ${LLVM_INSTALL_DIR}/bin PARENT_SCOPE)
  set(QUICK_LLVM_INCLUDE_DIR ${LLVM_INSTALL_DIR}/include PARENT_SCOPE)
  set(QUICK_LLVM_LIB_DIR ${LLVM_INSTALL_DIR}/lib PARENT_SCOPE)
  set(QUICK_LLVM_EXECUTABLE ${LLVM_INSTALL_DIR}/bin/llc PARENT_SCOPE)
endfunction()

# Clones llvm `version` and builds it in `install_path`
function(quick_install_llvm arch llvm_version install_path)
  set(LLVM_URL "https://github.com/llvm/llvm-project/releases/download/llvmorg-${llvm_version}/llvm-${llvm_version}.src.tar.xz")
  set(LLVM_INSTALL_DIR "${install_path}/llvm-${llvm_version}")
  file(DOWNLOAD ${LLVM_URL} "${CMAKE_BINARY_DIR}/llvm-${llvm_version}.src.tar.xz"
       SHOW_PROGRESS)
  execute_process(COMMAND "${CMAKE_COMMAND}" -E tar xf "${CMAKE_BINARY_DIR}/llvm-${llvm_version}.src.tar.xz")
  execute_process(COMMAND "${CMAKE_COMMAND}" -E make_directory "${LLVM_INSTALL_DIR}")
  execute_process(COMMAND "${CMAKE_COMMAND}" -E chdir "${CMAKE_BINARY_DIR}/llvm-${llvm_version}.src"
                  "${CMAKE_COMMAND}" -DCMAKE_INSTALL_PREFIX:PATH="${LLVM_INSTALL_DIR}"
                  -DLLVM_TARGETS_TO_BUILD:STRING="${arch}"
                  -DLLVM_ENABLE_PROJECTS:STRING="clang;compiler-rt"
                  -G "${CMAKE_GENERATOR}" ..)
  execute_process(COMMAND "${CMAKE_COMMAND}" --build "${CMAKE_BINARY_DIR}/llvm-${llvm_version}.src" --target install)
  quick_llvm_config(${LLVM_INSTALL_DIR}/bin/llvm-config)
  set(QUICK_LLVM_EXECUTABLE_DIR ${LLVM_INSTALL_DIR}/bin PARENT_SCOPE)
  set(QUICK_LLVM_INCLUDE_DIR ${LLVM_INSTALL_DIR}/include PARENT_SCOPE)
  set(QUICK_LLVM_LIB_DIR ${LLVM_INSTALL_DIR}/lib PARENT_SCOPE)
  set(QUICK_LLVM_EXECUTABLE ${LLVM_INSTALL_DIR}/bin/llc PARENT_SCOPE)
endfunction()
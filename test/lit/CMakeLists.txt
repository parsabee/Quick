configure_file(${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
    lit.site.cfg.py @ONLY)

message(STATUS "lit: ${QUICK_lit_EXECUTABLE}")
message(STATUS "filecheck: ${QUICK_filecheck_EXECUTABLE}")

add_custom_target(check-quick
    COMMAND ${QUICK_lit_EXECUTABLE} "${CMAKE_CURRENT_BINARY_DIR}" -v
    DEPENDS ${QUICK_lit_EXECUTABLE} ${QUICK_EXECUTABLE}
    )
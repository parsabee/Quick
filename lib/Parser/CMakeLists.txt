quick_run_bison(${CMAKE_CURRENT_SOURCE_DIR}/Parser.ypp)
quick_run_flex(${CMAKE_CURRENT_SOURCE_DIR}/Lexer.lpp)

message(STATUS "bison gen file " ${QUICK_BISON_GENERATED_FILE})
message(STATUS "flex gen file " ${QUICK_FLEX_GENERATED_FILE})

add_library(QuickParser STATIC
    ${QUICK_BISON_GENERATED_FILE}
    ${QUICK_FLEX_GENERATED_FILE}
    ${CMAKE_CURRENT_SOURCE_DIR}/Driver.cpp)

target_compile_options(QuickParser PRIVATE "-frtti") #Bison/Flex generated files need it

quick_link_bison_libraries(QuickParser ${quick_bison_version} ${quick_bison_install_dir})
quick_link_flex_libraries(QuickParser ${quick_flex_version} ${quick_flex_install_dir})
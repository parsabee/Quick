set(TARGET ASTNodeDownCastTest)
add_executable(${TARGET} ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET}.cpp)
add_dependencies(${TARGET} gtest)
target_link_libraries(${TARGET} ${QUICK_GTEST_LIBRARY} ${QUICK_GTEST_MAIN_LIBRARY})

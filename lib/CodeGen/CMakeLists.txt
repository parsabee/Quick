add_subdirectory(Type)

add_library(QuickCodeGen
    STATIC

    ${CMAKE_CURRENT_SOURCE_DIR}/ClassCodeGen.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/FnCodeGen.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ExprCodeGen.cpp
    )

target_link_libraries(QuickCodeGen

    PRIVATE

    QuickAST
    QuickCodeGenType
    QuickUtils
    QuickSema

    ${QUICK_LLVM_LDFLAGS}
    ${QUICK_LLVM_LIBS}
    ${QUICK_LLVM_SYSTEMLIBS}
    )
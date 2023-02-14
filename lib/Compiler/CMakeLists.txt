add_library(QuickCompiler
    STATIC

    ${CMAKE_CURRENT_SOURCE_DIR}/Compiler.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/JITEngine.cpp
    )

target_link_libraries(QuickCompiler
    PRIVATE

    QuickParser
    QuickSema
    QuickCodeGen
    QuickRuntime
    QuickUtils
    ${QUICK_LLVM_LIBS}
    ${QUICK_LLVM_SYSTEMLIBS}
    )

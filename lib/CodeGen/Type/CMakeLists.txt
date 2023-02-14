add_library(QuickCodeGenType
    STATIC

    ${CMAKE_CURRENT_SOURCE_DIR}/IRType.cpp
    )

target_link_libraries(QuickCodeGenType
    PRIVATE

    QuickUtils
    QuickSemaType
    ${QUICK_LLVM_LIBS}
    ${QUICK_LLVM_SYSTEMLIBS}
    )
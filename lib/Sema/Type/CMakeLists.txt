add_library(QuickSemaType
    STATIC

    ${CMAKE_CURRENT_SOURCE_DIR}/QType.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/QTypeDB.cpp
    )

target_link_libraries(QuickSemaType
    PRIVATE

    QuickUtils
    ${QUICK_LLVM_LIBS}
    ${QUICK_LLVM_SYSTEMLIBS}
    )
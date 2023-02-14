add_subdirectory(Type)
add_subdirectory(TypeChecker)

add_library(QuickSema
    STATIC

    ${CMAKE_CURRENT_SOURCE_DIR}/ClassVerifier.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/SemaVerifier.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/StmtVerifier.cpp
    )

target_link_libraries(QuickSema
    PRIVATE

    QuickAST
    QuickSemaType
    QuickTypeChecker
    QuickUtils
    )

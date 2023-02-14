add_library(QuickTypeChecker
    STATIC

    ${CMAKE_CURRENT_SOURCE_DIR}/TypeChecker.cpp
    )

target_link_libraries(QuickTypeChecker
    PRIVATE

    QuickEnv
    QuickSemaType
    )
add_library(QuickEnv
    STATIC

    ${CMAKE_CURRENT_SOURCE_DIR}/Environment.cpp
    )

target_link_libraries(QuickEnv
    PRIVATE

    QuickSemaType
    )
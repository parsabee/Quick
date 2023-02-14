set_source_files_properties(qk.cpp PROPERTIES COMPILE_FLAGS -fno-rtti)

add_executable(${QUICK_EXECUTABLE} qk.cpp)

target_link_libraries(${QUICK_EXECUTABLE}
    PRIVATE

    QuickCompiler
    ${QUICK_LLVM_LIBS}
    ${QUICK_LLVM_SYSTEMLIBS}
    )
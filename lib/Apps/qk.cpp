//===--- main.cpp -----------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// The main driver of the jit compiler
//
//===----------------------------------------------------------------------===//

#include "Compiler/Pipeline.hpp"
#include "Utils/PrintVisitor.hpp"

#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;
using namespace quick;

// Program options
cl::opt<bool> DumpAST("dump-ast",
                      cl::desc("prints the AST of the input program"),
                      cl::init(false));
cl::opt<bool> EmitLLVMIR("emit-llvm-ir",
                         cl::desc("prints the generated llvm ir to stdout"),
                         cl::init(false));
cl::opt<std::string> Filename(cl::Positional, cl::desc("<input-file>"),
                              cl::NumOccurrencesFlag::Required);

int main(int argc, char **argv) {
  int status = 0;
  cl::ParseCommandLineOptions(argc, argv, "Quick Compiler\n");

  auto parsed = EXIT_ON_ERROR(compiler::ParseFile(Filename));
  if (DumpAST) {
    ast::print(parsed.getTranslationUnit());
    std::exit(status);
  }

  auto typechecked = EXIT_ON_ERROR(compiler::TypeCheck(std::move(parsed)));

  auto codegened = EXIT_ON_ERROR(compiler::CodeGen(std::move(typechecked)));

  if (EmitLLVMIR) {
    std::unique_ptr<Module> module = codegened.releaseModule();
    module->print(llvm::outs(), nullptr);
    std::exit(status);
  }

  compiler::InitializeLLVMBackendTarget();
  status = compiler::Jit(std::move(codegened));
  std::exit(status);
}
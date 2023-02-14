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
  cl::ParseCommandLineOptions(argc, argv, "Quick Compiler\n");

  auto statusOrParsed = compiler::Parse(Filename);
  if (!statusOrParsed.ok())
    std::exit(static_cast<int>(statusOrParsed.status()));

  auto parsed = statusOrParsed.ValueOrDie();
  if (DumpAST) {
    ast::print(parsed.getTranslationUnit());
    std::exit(0);
  }

  auto statusOrTypeChecked = compiler::TypeCheck(std::move(parsed));
  if (!statusOrTypeChecked.ok())
    std::exit(static_cast<int>(statusOrTypeChecked.status()));

  auto typechecked = statusOrTypeChecked.ValueOrDie();
  auto statusOrCodeGened = compiler::CodeGen(std::move(typechecked));
  if (!statusOrCodeGened.ok())
    std::exit(static_cast<int>(statusOrCodeGened.status()));

  auto codeGened = statusOrCodeGened.ValueOrDie();
  if (EmitLLVMIR) {
    std::unique_ptr<Module> module = codeGened.releaseModule();
    module->print(llvm::outs(), nullptr);
    std::exit(0);
  }

  compiler::InitializeLLVMBackendTarget();
  int status = compiler::Jit(std::move(codeGened));
  std::exit(status);
}
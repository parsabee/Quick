//===--- Compiler.cpp - Implements codegen function -------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
#include "Compiler/Pipeline.hpp"
#include "CodeGen/ClassCodeGen.hpp"
#include "CodeGen/FnCodeGen.hpp"

#include "llvm/Support/TargetSelect.h"

namespace quick::compiler {

using namespace llvm;
using namespace ast;
using namespace codegen;
using namespace codegen::type;

void InitializeLLVMBackendTarget() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();
}

StatusOr<CodeGenedObject> CodeGen(TypeCheckedObject typecheckedObject) {
  auto status = Status::OK;
  const TranslationUnit &root = typecheckedObject.getTranslationUnit();
  CodeGenedObject codeGened(std::move(typecheckedObject));
  auto &tdb = codeGened.getTypeDB();

  // Setting up llvm module and builder
  auto *cntx = codeGened.getContext();
  auto module =
      std::make_unique<Module>(codeGened.getSourceName(), *cntx);
  IRBuilder<> builder(*cntx);
  LLVMTypeRegistry tr(*module);

  // Generating code for classes and methods
  for (auto &clss : root.getClasses()) {
    status = ClassCodeGen::generate(tdb, *clss, *module, builder, tr);
    if (!ok(status))
      return status;
  }

  // Generating code for the main function
  LLVMEnv env;
  status = FnCodeGen::generate(tdb, builder, *module, root.getCompoundStmt(),
                               tr, env);
  if (!ok(status))
    return status;

  codeGened.setModule(std::move(module));
  return std::move(codeGened);
}

} // namespace quick::compiler
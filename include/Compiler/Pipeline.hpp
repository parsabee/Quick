//===--- Pipeline.hpp -------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines a lean set of functions to interface with the compiler
//
//===----------------------------------------------------------------------===//
#ifndef QUICK_COMPILER_PIPELINE_HPP
#define QUICK_COMPILER_PIPELINE_HPP

#include "AST/AST.hpp"
#include "Compiler/Object.hpp"
#include "Utils/Status.hpp"

namespace quick::compiler {

//===----------------------------------------------------------------------===//
// Pipeline
//===----------------------------------------------------------------------===//
void InitializeLLVMBackendTarget();
StatusOr<ParsedObject> ParseString(const std::string &source);
StatusOr<ParsedObject> ParseFile(const std::string &source);
StatusOr<TypeCheckedObject> TypeCheck(ParsedObject);
StatusOr<CodeGenedObject> CodeGen(TypeCheckedObject);
int Jit(CodeGenedObject);
//===----------------------------------------------------------------------===//

} // namespace quick::compiler

#endif // QUICK_COMPILER_PIPELINE_HPP
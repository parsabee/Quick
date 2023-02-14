//===--- JITEngine.hpp ------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// the jit engine
//
//===----------------------------------------------------------------------===//
#ifndef QUICK_COMPILER_JITENGINE_HPP
#define QUICK_COMPILER_JITENGINE_HPP

#include "CodeGen/ClassCodeGen.hpp"
#include "CodeGen/FnCodeGen.hpp"
#include "Runtime/Runtime.hpp"
#include "Utils/PrintVisitor.hpp"
#include "Utils/Utils.hpp"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm::orc {

/// ===-------------------------------------------------------------------=== //
/// A Simple just-in-time compilation engine copied from llvm kaleidoscope
/// example
/// ===-------------------------------------------------------------------=== //
class JITEngine {
private:
  ExecutionSession ES;
  RTDyldObjectLinkingLayer ObjectLayer;
  IRCompileLayer CompileLayer;

  DataLayout DL;
  MangleAndInterner Mangle;
  ThreadSafeContext Ctx;

  JITDylib &MainJD;

public:
  JITEngine(JITTargetMachineBuilder JTMB, DataLayout DL);
  static Expected<std::unique_ptr<JITEngine>> Create();
  const DataLayout &getDataLayout() const { return DL; }
  LLVMContext &getContext() { return *Ctx.getContext(); }
  Error addModule(std::unique_ptr<Module> M);
  Expected<JITEvaluatedSymbol> lookup(StringRef Name);
};

} // namespace llvm::orc

#endif // QUICK_COMPILER_JITENGINE_HPP
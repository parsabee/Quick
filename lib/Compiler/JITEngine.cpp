//===--- JITEngine.cpp ------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
#include "Compiler/JITEngine.hpp"
#include "Compiler/Pipeline.hpp"

using namespace llvm;

namespace llvm::orc {
JITEngine::JITEngine(JITTargetMachineBuilder JTMB, DataLayout DL)
      : ObjectLayer(ES,
                    []() { return std::make_unique<SectionMemoryManager>(); }),
        CompileLayer(ES, ObjectLayer,
                     std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
        DL(std::move(DL)), Mangle(ES, this->DL),
        Ctx(std::make_unique<LLVMContext>()),
        MainJD(ES.createJITDylib("<main>")) {
    MainJD.addGenerator(
        cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
            DL.getGlobalPrefix())));
    SymbolMap map;
    registerRuntimeFunctions([&](const std::string &sym, void *addr) {
      map[Mangle(sym)] =
          JITEvaluatedSymbol(pointerToJITTargetAddress(addr), JITSymbolFlags());
    });
    cantFail(MainJD.define(absoluteSymbols(map)));
}

Expected<std::unique_ptr<JITEngine>> JITEngine::Create() {
  auto JTMB = JITTargetMachineBuilder::detectHost();

  if (!JTMB)
    return JTMB.takeError();

  auto DL = JTMB->getDefaultDataLayoutForTarget();
  if (!DL)
    return DL.takeError();

  return std::make_unique<JITEngine>(std::move(*JTMB), std::move(*DL));
}

Error JITEngine::addModule(std::unique_ptr<Module> M) {
  return CompileLayer.add(MainJD, ThreadSafeModule(std::move(M), Ctx));
}

Expected<JITEvaluatedSymbol> JITEngine::lookup(StringRef Name) {
  return ES.lookup({&MainJD}, Mangle(Name.str()));
}
} // namespace llvm::orc

namespace quick::compiler {
int Jit(CodeGenedObject codegenedObject) {
  auto module = codegenedObject.releaseModule();
  ExitOnError ExitOnErr;
  std::unique_ptr<llvm::orc::JITEngine> engine;
  engine = ExitOnErr(orc::JITEngine::Create());
  module->setDataLayout(engine->getDataLayout());
  ExitOnErr(engine->addModule(std::move(module)));
  auto MainFn = ExitOnErr(engine->lookup(codegen::MainFn));
  auto FnPtr = (intptr_t)MainFn.getAddress();
  auto Fn = (int (*)())FnPtr;
  return Fn();
}
} // namespace quick::compiler
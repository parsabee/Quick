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
#include "Utils/Status.hpp"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include <fstream>
#include <memory>
#include <string>

namespace quick::compiler {

//===----------------------------------------------------------------------===//
// Source - source file
//===----------------------------------------------------------------------===//
struct Source {
  std::string name;
  std::fstream file;
  const std::string &getSourceName() const { return name; }
  std::fstream &getFile() { return file; }
  std::string releaseSourceName() { return std::move(name); }
  std::fstream releaseFile() { return std::move(file); }
  Source(std::string n, std::fstream f)
      : name(std::move(n)), file(std::move(f)) {}
  Source(const Source &) = delete;
  Source(Source &&) = default;
  Source() = default;
};

//===----------------------------------------------------------------------===//
// CompilerObject - source with translation unit
//===----------------------------------------------------------------------===//
struct CompilerObject : public Source {
  std::unique_ptr<ast::TranslationUnit> translationUnit = nullptr;
  std::unique_ptr<ast::TranslationUnit> releaseTranslationUnit() {
    return std::move(translationUnit);
  }
  const ast::TranslationUnit &getTranslationUnit() const {
    return *translationUnit;
  }
  CompilerObject(std::unique_ptr<ast::TranslationUnit> tu, Source s)
      : Source(std::move(s)), translationUnit(std::move(tu)) {}
  CompilerObject(const CompilerObject &) = delete;
  CompilerObject(CompilerObject &&) = default;
  CompilerObject() = default;
};

//===----------------------------------------------------------------------===//
// ParsedObject - object returned by `Parse()`, a parsed object is a frontend
// compiler object
//===----------------------------------------------------------------------===//
class ParsedObject : public CompilerObject {
  friend StatusOr<ParsedObject> Parse(const std::string &);
  ParsedObject(std::unique_ptr<ast::TranslationUnit> tu, std::fstream file,
               const std::string &filename)
      : CompilerObject(std::move(tu), Source{filename, std::move(file)}) {}

public:
  ParsedObject(const ParsedObject &) = delete;
  ParsedObject(ParsedObject &&) = default;
  ParsedObject() = default;
};

//===----------------------------------------------------------------------===//
// TypeCheckedObject - object returned by `TypeCheck()`, it is a semantically
// verified compiler object
//===----------------------------------------------------------------------===//
class TypeCheckedObject : public CompilerObject {
  friend StatusOr<TypeCheckedObject> TypeCheck(ParsedObject);
  explicit TypeCheckedObject(ParsedObject parsedObject)
      : CompilerObject(parsedObject.releaseTranslationUnit(),
                       Source{parsedObject.releaseSourceName(),
                              parsedObject.releaseFile()}) {}

public:
  TypeCheckedObject(const TypeCheckedObject &) = delete;
  TypeCheckedObject(TypeCheckedObject &&) = default;
  TypeCheckedObject() = default;
};

//===----------------------------------------------------------------------===//
// CodeGenedObject - object returned by `CodeGen()`, it is an llvm module,
// context, and compiler object
//===----------------------------------------------------------------------===//
class CodeGenedObject : public CompilerObject {
  std::unique_ptr<llvm::LLVMContext> _cntx = nullptr;
  std::unique_ptr<llvm::Module> _module = nullptr;
  friend StatusOr<CodeGenedObject> CodeGen(TypeCheckedObject);
  explicit CodeGenedObject(TypeCheckedObject typechecked)
      : CompilerObject(
            typechecked.releaseTranslationUnit(),
            Source{typechecked.releaseSourceName(), typechecked.releaseFile()}),
        _cntx(new llvm::LLVMContext) {}

public:
  CodeGenedObject() = default;
  CodeGenedObject(const CodeGenedObject &) = delete;
  CodeGenedObject(CodeGenedObject &&) = default;
  llvm::LLVMContext *getContext() { return _cntx ? _cntx.get() : nullptr; }
  void setModule(std::unique_ptr<llvm::Module> module) {
    _module = std::move(module);
  }
  std::unique_ptr<llvm::Module> releaseModule() { return std::move(_module); }
};

//===----------------------------------------------------------------------===//
// Pipeline
//===----------------------------------------------------------------------===//
void InitializeLLVMBackendTarget();
StatusOr<ParsedObject> Parse(const std::string &source);
StatusOr<TypeCheckedObject> TypeCheck(ParsedObject);
StatusOr<CodeGenedObject> CodeGen(TypeCheckedObject);
int Jit(CodeGenedObject);
//===----------------------------------------------------------------------===//

} // namespace quick::compiler

#endif // QUICK_COMPILER_PIPELINE_HPP
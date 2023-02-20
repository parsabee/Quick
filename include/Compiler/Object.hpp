//===--- CompilerObject.hpp -------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// Objects returned by different stages of the compiler
//
//===----------------------------------------------------------------------===//
#ifndef QUICK_COMPILER_OBJECT_HPP
#define QUICK_COMPILER_OBJECT_HPP

#include "AST/AST.hpp"
#include "Sema/QTypeDB.hpp"
#include "Utils/Logger.hpp"
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
class Source {
  std::string name;
  std::fstream file;

public:
  Source(std::string n, std::fstream f)
      : name(std::move(n)), file(std::move(f)) {}
  Source(const Source &) = delete;
  Source(Source &&) = default;
  Source() = default;

  const std::string &getSourceName() const { return name; }
  std::fstream &getFile() { return file; }
  std::string releaseSourceName() { return std::move(name); }
  std::fstream releaseFile() { return std::move(file); }
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
  friend StatusOr<ParsedObject> ParseFile(const std::string &);
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
// verified compiler object, and a database of all of the types
//===----------------------------------------------------------------------===//
class TypeCheckedObject : public CompilerObject {
  friend StatusOr<TypeCheckedObject> TypeCheck(ParsedObject);
  std::unique_ptr<quick::sema::type::QTypeDB> _typeDB;
  explicit TypeCheckedObject(ParsedObject parsedObject,
                             std::unique_ptr<quick::sema::type::QTypeDB> tdb)
      : CompilerObject(parsedObject.releaseTranslationUnit(),
                       Source{parsedObject.releaseSourceName(),
                              parsedObject.releaseFile()}),
        _typeDB(std::move(tdb)) {}

public:
  TypeCheckedObject(const TypeCheckedObject &) = delete;
  TypeCheckedObject(TypeCheckedObject &&) = default;
  TypeCheckedObject() = default;
  auto releaseTypeDB() { return std::move(_typeDB); }
};

//===----------------------------------------------------------------------===//
// CodeGenedObject - object returned by `CodeGen()`, it is an llvm module,
// context, and compiler object
//===----------------------------------------------------------------------===//
class CodeGenedObject : public CompilerObject {
  std::unique_ptr<llvm::LLVMContext> _cntx;
  std::unique_ptr<llvm::Module> _module;
  std::unique_ptr<quick::sema::type::QTypeDB> _typeDB;
  friend StatusOr<CodeGenedObject> CodeGen(TypeCheckedObject);
  explicit CodeGenedObject(TypeCheckedObject typechecked)
      : CompilerObject(
            typechecked.releaseTranslationUnit(),
            Source{typechecked.releaseSourceName(), typechecked.releaseFile()}),
        _cntx(new llvm::LLVMContext), _typeDB(typechecked.releaseTypeDB()) {}

public:
  CodeGenedObject() = default;
  CodeGenedObject(const CodeGenedObject &) = delete;
  CodeGenedObject(CodeGenedObject &&) = default;
  llvm::LLVMContext *getContext() { return _cntx.get(); }
  void setModule(std::unique_ptr<llvm::Module> module) {
    _module = std::move(module);
  }
  auto releaseModule() { return std::move(_module); }
  auto &getModule() { return *_module; }
  auto releaseTypeDB() { return std::move(_typeDB); }
  auto &getTypeDB() { return *_typeDB; }
};

} // namespace quick::compiler

#endif // QUICK_COMPILER_OBJECT_HPP

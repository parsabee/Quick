//===--- ClassCodeGen.hpp ---------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines the nodes in the abstract syntax tree of a Quick program
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_CLASSCODEGEN_H
#define QUICK_CLASSCODEGEN_H

#include "CodeGen/FnCodeGen.hpp"
#include "Env/Environment.hpp"
#include "Utils/Status.hpp"

#include "llvm/IR/IRBuilder.h"

namespace quick {
namespace codegen {

/// ===-------------------------------------------------------------------=== //
/// ClassCodeGen - A visitor that visits a class and generates llvm ir code
/// ===-------------------------------------------------------------------=== //
class ClassCodeGen : public ast::ASTVisitor<ClassCodeGen, bool> {
  const ast::Class &theClass;
  llvm::Module &module;
  llvm::IRBuilder<> &builder;
  type::LLVMTypeRegistry &tr;
  sema::type::QTypeDB &tdb;
  sema::type::QType *qType;
  const std::string &typeName;
  llvm::StructType *superVtable = nullptr;
  type::ComplexType *super = nullptr;
  void generateGetVtableFunction(llvm::Constant *globalVtable);
  bool generateInitFunction(llvm::Function *initFn,
                            llvm::Function *superInitFn);
  bool generateCreateFunction(llvm::Function *createFn,
                              llvm::Function *superInitFn,
                              llvm::StructType *type, llvm::StructType *vtable,
                              llvm::Constant *globalVtable,
                              type::MethodTable &methodTable);
  type::MethodTable generateVTable(type::IRType *, llvm::StructType *);
  sema::type::Table<llvm::Type *> generateMemberTable();

  ClassCodeGen(sema::type::QTypeDB &tdb, const ast::Class &theClass,
               llvm::Module &module, llvm::IRBuilder<> &builder,
               type::LLVMTypeRegistry &tr);

public:
  // only visits classes/methods
  bool visitMethod(const ast::Method &);
  bool visitMethods(const ast::Methods &);
  bool visitClass(const ast::Class &);
  bool visitExpression(const ast::Expression &) = delete;
#define EXPR_NODE_HANDLER(NODE) bool visit##NODE(const ast::NODE &) = delete;
#define STMT_NODE_HANDLER(NODE) bool visit##NODE(const ast::NODE &) = delete;
#include "AST/ASTNodes.def"

  static Status generate(sema::type::QTypeDB &tdb, const ast::Class &theClass,
                         llvm::Module &module, llvm::IRBuilder<> &builder,
                         type::LLVMTypeRegistry &tr);
};

} // namespace codegen
} // namespace quick

#endif // QUICK_CLASSCODEGEN_H

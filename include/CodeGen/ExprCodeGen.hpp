//===--- ExprCodeGen.hpp ----------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines ExprCodeGen, a visitor class that generates llvm ir code
// for an expression.
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_EXPRCODEGEN_HPP
#define QUICK_EXPRCODEGEN_HPP

#include "llvm/IR/IRBuilder.h"

#include "AST/ASTVisitor.hpp"
#include "CodeGen/IRType.hpp"
#include "Env/Environment.hpp"
#include "Sema/QTypeDB.hpp"

namespace quick::codegen {

/// ===-------------------------------------------------------------------=== //
/// ExprCodeGen - An expression visitor that generates code for a given 
/// expression
/// ===-------------------------------------------------------------------=== //
class ExprCodeGen : public ast::ASTVisitor<ExprCodeGen, llvm::Value *> {
  llvm::Module &module;
  llvm::IRBuilder<> &builder;
  llvm::LLVMContext &llvmCntx;
  type::LLVMTypeRegistry &typeRegistery;
  LLVMEnv &llvmEnv;
  sema::type::QTypeDB &tdb;

public:
  ExprCodeGen(llvm::Module &module, llvm::IRBuilder<> &b,
              type::LLVMTypeRegistry &tr, sema::Env &env, LLVMEnv &llvmEnv)
      : module(module), builder(b), llvmCntx(b.getContext()), typeRegistery(tr),
        llvmEnv(llvmEnv), tdb(sema::type::QTypeDB::get()) {}
  llvm::Value *visitTranslationUnit(const ast::TranslationUnit &) = delete;
  llvm::Value *visitStatement(const ast::Statement &) = delete;
#define STMT_NODE_HANDLER(NODE)                                                \
  llvm::Value *visit##NODE(const ast::NODE &) = delete;
#define EXPR_NODE_HANDLER(NODE) llvm::Value *visit##NODE(const ast::NODE &);
#include "AST/ASTNodes.def"
};

} // namespace quick::codegen

#endif // QUICK_EXPRCODEGEN_HPP

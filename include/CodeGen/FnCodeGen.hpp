//===--- FnCodeGen.hpp ------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines FnCodeGen, a visitor class that generates llvm functions
// for a method.
//
//===----------------------------------------------------------------------===//

#ifndef QUACK_FNCODEGEN_HPP
#define QUACK_FNCODEGEN_HPP

#include "AST/ASTVisitor.hpp"
#include "CodeGen/ExprCodeGen.hpp"
#include "CodeGen/IRType.hpp"
#include "Env/Environment.hpp"
#include "Sema/QTypeDB.hpp"
#include "Utils/Status.hpp"

#include "llvm/IR/IRBuilder.h"

namespace quick::codegen {

constexpr char MainFn[] = "main";

/// ===-------------------------------------------------------------------=== //
/// FnCodeGen - A statement visitor that generates code for every given
/// function/method
/// ===-------------------------------------------------------------------=== //
class FnCodeGen : public ast::ASTVisitor<FnCodeGen, bool> {
public:
  using Args = llvm::SmallVector<std::pair<llvm::StringRef, llvm::Value *>, 4>;
  FnCodeGen(sema::type::QTypeDB &db, llvm::IRBuilder<> &builder,
            llvm::Module &module, const ast::CompoundStmt &cmpStmt,
            type::LLVMTypeRegistry &tr, LLVMEnv &llvmEnv,
            sema::type::QType *parentType = nullptr,
            llvm::StringRef fnName = MainFn, type::IRType *returnType = nullptr,
            Args args = {})
      : fnName(fnName), module(module), builder(builder), tr(tr),
        llvmEnv(llvmEnv), exprCG(db, module, builder, tr, env, llvmEnv),
        fnBody(cmpStmt), tdb(db), parentType(parentType), retType(returnType),
        args(std::move(args)) {}

  bool visitTranslationUnit(const ast::TranslationUnit &) = delete;
  bool visitExpression(const ast::Expression &) = delete;
#define EXPR_NODE_HANDLER(NODE)                                                \
  llvm::Value *visit##NODE(const ast::NODE &) = delete;
#define STMT_NODE_HANDLER(NODE) bool visit##NODE(const ast::NODE &);
#include "AST/ASTNodes.def"

  auto &getExprCodeGen() { return exprCG; }
  bool isMain() { return fnName == MainFn; }
  Status generate();
  static Status generate(sema::type::QTypeDB &tdb, llvm::IRBuilder<> &builder,
                         llvm::Module &module, const ast::CompoundStmt &cmpStmt,
                         type::LLVMTypeRegistry &tr, LLVMEnv &llvmEnv,
                         sema::type::QType *parentType = nullptr,
                         llvm::StringRef fnName = MainFn,
                         type::IRType *returnType = nullptr, Args args = {});

private:
  Logger logger;
  llvm::StringRef fnName;
  llvm::Module &module;
  llvm::IRBuilder<> &builder;
  type::LLVMTypeRegistry &tr;
  LLVMEnv &llvmEnv;
  ExprCodeGen exprCG;
  Args args;
  type::IRType *retType;

  const ast::CompoundStmt &fnBody;
  sema::type::QTypeDB &tdb;
  sema::type::QType *parentType; // the type that the compound statement is in
                                 // the environment of
  sema::Env env;
};

llvm::Function *getOrCreateFnSym(const std::string &functionName,
                                 llvm::Module &module, llvm::Type *resultType,
                                 llvm::ArrayRef<llvm::Type *> params = {},
                                 bool isVarArgs = false);

llvm::Function *getOrCreateFnSym(const std::string &functionName,
                                 llvm::Module &module,
                                 llvm::FunctionType *FuncTy,
                                 bool isVarArgs = false);

} // namespace quick::codegen
#endif // QUACK_FNCODEGEN_HPP

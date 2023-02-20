//===--- StmtVerifies.hpp ---------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// Defines a visitor that goes over statements and verifies their legality
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_SEMA_STMTVERIFIER_HPP
#define QUICK_SEMA_STMTVERIFIER_HPP

#include "Env/Environment.hpp"
#include "Sema/TypeChecker.hpp"
#include "Utils/Logger.hpp"
#include "Utils/Status.hpp"

namespace quick::sema {

/// ===-------------------------------------------------------------------=== //
/// StmtVerifier - A visitor that visits every statement and verifies
/// their legality
/// ===-------------------------------------------------------------------=== //
class StmtVerifier : public ast::ASTVisitor<StmtVerifier, bool> {
  SourceLogger &logger;
  const ast::CompoundStmt &cmpStmt;
  Env &env;
  type::QTypeDB &tdb;
  type::QType *parentType; // the type that the compound statement is in the
                           // environment of
  type::QType *returnType;
  bool isConstructor; // If compound statement belongs to a constructor
  TypeChecker typeChecker;
  StmtVerifier(type::QTypeDB &db, SourceLogger &logger,
               const ast::CompoundStmt &cmpStmt, Env &env,
               type::QType *parentType = nullptr,
               type::QType *returnType = nullptr, bool isConstructor = false)
      : logger(logger), cmpStmt(cmpStmt), env(env), tdb(db),
        parentType(parentType), returnType(returnType),
        isConstructor(isConstructor), typeChecker(logger, tdb, env) {}

public:
  // only visits statements
  bool visitExpression(const ast::Expression &) = delete;
#define EXPR_NODE_HANDLER(NODE) bool visit##NODE(const ast::NODE &) = delete;
#define STMT_NODE_HANDLER(NODE) bool visit##NODE(const ast::NODE &);
#include "AST/ASTNodes.def"

  /// returns OK if legal, ERROR otherwise
  static Status verify(type::QTypeDB &db, SourceLogger &logger, const ast::CompoundStmt &cmpStmt,
                       Env &env, type::QType *parentType = nullptr,
                       type::QType *returnType = nullptr,
                       bool isConstructor = false, bool inANewScope = true,
                       bool addThisToScope = false);
};

} // namespace quick::sema
#endif // QUICK_SEMA_STMTVERIFIER_HPP

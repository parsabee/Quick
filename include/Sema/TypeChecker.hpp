//===--- TypeChecker.hpp ----------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines the TypeChecker class.
// TypeChecker is an expression visitor that returns the type of an expression
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_SEMA_TYPECHECKER_HPP
#define QUICK_SEMA_TYPECHECKER_HPP

#include "AST/ASTVisitor.hpp"
#include "Env/Environment.hpp"
#include "Sema/QTypeDB.hpp"

namespace quick::sema {

/// ===-------------------------------------------------------------------=== //
/// TypeChecker - An expression visitor that evaluates the type of an
/// expression.
/// ===-------------------------------------------------------------------=== //
class TypeChecker : public ast::ASTVisitor<TypeChecker, type::QType *> {
  std::fstream &file;
  type::QTypeDB &tdb;
  const Env &env;

public:
  TypeChecker(std::fstream &file, type::QTypeDB &tdb, Env &env)
      : file(file), tdb(tdb), env(env) {}
  type::QType *visitStatement(const ast::Statement &) = delete;
  type::QType *visitTranslationUnit(const ast::TranslationUnit &) = delete;
#define STMT_NODE_HANDLER(NODE) type::QType *visit##NODE(const ast::NODE &) = delete;
#define EXPR_NODE_HANDLER(NODE) type::QType *visit##NODE(const ast::NODE &);
#include "AST/ASTNodes.def"
};


} // namespace quick::sema

#endif // QUICK_SEMA_TYPECHECKER_HPP

//===--- ClassVerifies.hpp --------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// Defines a visitor that goes over classes and verifies their legality
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_CLASSVERIFIER_HPP
#define QUICK_CLASSVERIFIER_HPP

#include "Env/Environment.hpp"
#include "AST/ASTVisitor.hpp"
#include "Sema/QTypeDB.hpp"
#include "Utils/Status.hpp"

namespace quick::sema {

/// ===-------------------------------------------------------------------=== //
/// ClassVerifier - A visitor that visits every class and verifies
/// their legality
/// ===-------------------------------------------------------------------=== //
class ClassVerifier : public ast::ASTVisitor<ClassVerifier, bool> {
  std::fstream &file;
  const ast::Class &theClass;
  type::QTypeDB &tdb;

  bool isSuperInitialized(const std::string &);
  bool verifyConstructor();
  bool hasRecursiveConstructor();

public:
  ClassVerifier(std::fstream &file, const ast::Class &theClass)
      : file(file), theClass(theClass), tdb(type::QTypeDB::get()) {}

  // only visits classes/methods
  bool visitMethod(const ast::Method &);
  bool visitMethods(const ast::Methods &);
  bool visitClass(const ast::Class &);
  bool visitExpression(const ast::Expression &) = delete;
#define EXPR_NODE_HANDLER(NODE) bool visit##NODE(const ast::NODE &) = delete;
#define STMT_NODE_HANDLER(NODE) bool visit##NODE(const ast::NODE &) = delete;
#include "AST/ASTNodes.def"

  Status verify();
};

} // namespace quick::sema

#endif // QUICK_CLASSVERIFIER_HPP

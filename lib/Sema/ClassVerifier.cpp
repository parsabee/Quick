//===--- ClassVerifies.cpp --------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Sema/ClassVerifier.hpp"
#include "Sema/StmtVerifier.hpp"
#include "Utils/Utils.hpp"

namespace quick::sema {

using namespace ast;

bool ClassVerifier::hasRecursiveConstructor() {
  auto isRecursiveCall = [&](const Call &call) {
    if (auto *identExpr = GET_NODE_AS(call.getCallee(), IdentifierExpression)) {
      if (identExpr->getVar().getName() ==
          this->theClass.getClassIdent().getName())
        return true;
    }
    return false;
  };

  auto &constructor = theClass.getConstructor();
  for (auto &stmt : constructor.getBody()) {
    if (auto *valueStmt = GET_NODE_AS(*stmt, ValueStmt)) {
      if (auto *call = GET_NODE_AS(valueStmt->getExpr(), Call)) {
        if (isRecursiveCall(*call))
          return true;
      }
    }
  }
  return false;
}

bool ClassVerifier::isSuperInitialized(const std::string &superName) {
  auto isSuperConstructor = [&](const Call &call) {
    if (auto *identExpr = GET_NODE_AS(call.getCallee(), IdentifierExpression)) {
      if (identExpr->getVar().getName() == superName)
        return true;
    }
    return false;
  };

  auto &constructor = theClass.getConstructor();
  for (auto &stmt : constructor.getBody()) {
    if (auto *valueStmt = GET_NODE_AS(*stmt, ValueStmt)) {
      if (auto *call = GET_NODE_AS(valueStmt->getExpr(), Call)) {
        if (isSuperConstructor(*call))
          return true;
      }
    }
  }
  return false;
}

bool ClassVerifier::verifyConstructor() {
  if (hasRecursiveConstructor()) {
    logError(file, theClass.getConstructor().getLocation(),
             "recursive type constructor detected");
    return false;
  }

  auto &constructor = theClass.getConstructor();
  if (auto superIdent = theClass.getSuper()) {
    auto &superName = superIdent->getName();
    if (type::isPrimitive(superName)) {
      logError(file, superIdent->getLocation(),
               "cannot inherit from a primitive type");
      return false;
    }

    auto *superType = tdb.getType(superName);
    if (!superType) {
      logError(file, superIdent->getLocation(),
               "type <" + superName + "> not found");
      return false;
    }

    if (superName != "Object") {
      if (!isSuperInitialized(superName)) {
        logError(file, theClass.getConstructor().getLocation(),
                 "super class not initialized");
        return false;
      }
    }
  }

  return true;
}

bool ClassVerifier::visitMethod(const ast::Method &m) {
  sema::Env env;
  auto &scope = env.addNewScope();
  auto qtype = tdb.getType(this->theClass.getClassIdent().getName());
  auto retType = tdb.getType(m.getReturnType().getName());
  if (!retType) {
    logError(file, m.getReturnType().getLocation(), "return type not found");
    return false;
  }
  StmtVerifier stmtVerifier(file, m.getBody(), env, qtype, retType);
  scope.insert({"this", qtype});
  for (auto &p : m.getParams()) {
    auto pType = tdb.getType(p->getType().getName());
    if (!pType) {
      logError(file, p->getLocation(),
               "parameter type not found < " + p->getType().getName() + " >");
      return false;
    }
    scope.insert({p->getVar().getName(), pType});
  }
  if (!stmtVerifier.visitCompoundStmt(m.getBody()))
    return false;
  env.popCurrentScope();
  return true;
}

bool ClassVerifier::visitClass(const ast::Class &clss) {
  if (!verifyConstructor())
    return false;

  for (auto &m : clss.getMethods())
    if (!visitMethod(*m))
      return false;

  return true;
}

Status ClassVerifier::verify() {
  return visitClass(theClass) ? Status::OK : Status::ERROR;
}

} // namespace quick::sema
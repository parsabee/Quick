//===--- ClassVerifies.cpp --------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Sema/ClassVerifier.hpp"
#include "Sema/StmtVerifier.hpp"
#include "Utils/Logger.hpp"
#include "Utils/Utils.hpp"

namespace quick::sema {

using namespace ast;

bool ClassVerifier::hasRecursiveConstructor() {
  auto isRecursiveCall = [&](const Call &call) {
    if (auto *identExpr = call.getCallee().as_a<IdentifierExpression>()) {
      if (identExpr->getVar().getName() ==
          this->theClass.getClassIdent().getName())
        return true;
    }
    return false;
  };

  auto &constructor = theClass.getConstructor();
  for (auto &stmt : constructor.getBody()) {
    if (auto *valueStmt = stmt->as_a<ValueStmt>()) {
      if (auto *call = valueStmt->getExpr().as_a<Call>()) {
        if (isRecursiveCall(*call))
          return true;
      }
    }
  }
  return false;
}

bool ClassVerifier::isSuperInitialized(const std::string &superName) {
  auto isSuperConstructor = [&](const Call &call) {
    if (auto *identExpr = call.getCallee().as_a<IdentifierExpression>()) {
      if (identExpr->getVar().getName() == superName)
        return true;
    }
    return false;
  };

  auto &constructor = theClass.getConstructor();
  for (auto &stmt : constructor.getBody()) {
    if (auto *valueStmt = stmt->as_a<ValueStmt>()) {
      if (auto *call = valueStmt->getExpr().as_a<Call>()) {
        if (isSuperConstructor(*call))
          return true;
      }
    }
  }
  return false;
}

bool ClassVerifier::verifyConstructor() {
  if (hasRecursiveConstructor()) {
    logger.log_node(theClass.getConstructor(),
                    "recursive type constructor detected");
    return false;
  }

  auto &constructor = theClass.getConstructor();
  if (auto superIdent = theClass.getSuper()) {
    auto &superName = superIdent->getName();
    if (type::isPrimitive(superName)) {
      logger.log_node(*superIdent, "cannot inherit from a primitive type");
      return false;
    }

    auto *superType = tdb.getType(superName);
    if (!superType) {
      logger.log_node(*superIdent, "type <", superName, "> not found");
      return false;
    }

    if (superName != "Object") {
      if (!isSuperInitialized(superName)) {
        logger.log_node(theClass.getConstructor(),
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
    //    logError(file, m.getReturnType().getLocation(), "return type not
    //    found");
    logger.log_node(m.getReturnType(), "return type not found");
    return false;
  }
  for (auto &p : m.getParams()) {
    auto pType = tdb.getType(p->getType().getName());
    if (!pType) {
      //      logError(file, p->getLocation(),
      //               "parameter type not found < " + p->getType().getName() +
      //               " >");
      logger.log_node(*p, "parameter type not found < ", p->getType().getName(),
                      " >");
      return false;
    }
    scope.insert({p->getVar().getName(), pType});
  }
  if (StmtVerifier::verify(tdb, logger, m.getBody(), env, qtype, retType,
                           /*isConstructor*/ false, /*addANewScope*/ false,
                           /*addThisToScope*/ true) != Status::OK)
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

Status ClassVerifier::verify(type::QTypeDB &db, SourceLogger &logger,
                             const ast::Class &theClass) {
  ClassVerifier cv(db, logger, theClass);
  return cv.visitClass(theClass) ? Status::OK : Status::ERROR;
}

} // namespace quick::sema
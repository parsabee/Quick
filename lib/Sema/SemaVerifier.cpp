//===--- SemaVerifier.cpp ---------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Sema/SemaVerifier.hpp"
#include "Compiler/Pipeline.hpp"
#include "Sema/ClassVerifier.hpp"
#include "Sema/StmtVerifier.hpp"

namespace quick {
namespace sema {

using namespace ast;

static int registerClasses(SourceLogger &logger, type::QTypeDB &tdb,
                           const Classes &clsses) {
  int numErrors = 0;
  // Registering all classes
  for (auto &clss : clsses) {
    auto &className = clss->getClassIdent().getName();
    if (tdb.getType(className)) {
      logger.log(clss->getClassIdent(), "redefinition of class <", className,
                 ">");
      numErrors++;
    } else {
      tdb.registerNewType(className, nullptr);
    }
  }
  return numErrors;
}

static int processMembers(SourceLogger &logger, type::QTypeDB &tdb,
                          const Classes &clsses) {
  int numErrors = 0;
  for (auto &clss : clsses) {
    auto type = tdb.getType(clss->getClassIdent().getName());
    assert(type);
    sema::Env env;
    auto &scope = env.addNewScope();
    for (auto &params : clss->getConstructor().getParams()) {
      auto &pt = params->getType().getName();
      auto &v = params->getVar().getName();
      auto *pQtype = tdb.getType(pt);
      scope.insert({v, pQtype});
    }
    if (StmtVerifier::verify(tdb, logger, clss->getConstructor().getBody(), env,
                             /*parentType*/ type, /*returnType*/ type,
                             /*isConstructor*/ true, /*addANewScope*/ false,
                             /*addThisToScope*/ true) != Status::OK) {
      numErrors++;
    }
  }
  return numErrors;
}

static int processSuperTypes(SourceLogger &logger, type::QTypeDB &tdb,
                             const Classes &clsses) {
  int numErrors = 0;
  for (auto &clss : clsses) {
    auto super = clss->getSuper();
    auto &className = clss->getClassIdent().getName();
    type::QType *superType = nullptr;
    if (super) {
      auto &superName = super->getName();
      superType = tdb.getType(superName);
      if (!superType) {
        numErrors++;
        logger.log(*clss->getSuper(), "type not found <" + superName + ">");
      }
    } else {
      superType = tdb.getType("Object");
    }

    if (superType) {
      auto type = tdb.getType(className);
      type->setParent(superType);
    }
  }
  return numErrors;
}

static int processMethods(SourceLogger &logger, type::QTypeDB &tdb,
                          const Classes &clsses) {
  int numErrors = 0;
  for (auto &clss : clsses) {
    type::QType *super = nullptr;
    if (auto superIdent = clss->getSuper()) {
      super = tdb.getType(superIdent->getName());
      assert(super);
    }
    if (!super)
      super = tdb.getObjectType();

    assert(super);
    auto &className = clss->getClassIdent().getName();
    auto type = tdb.getType(className);
    auto process = [&](const Method *m) -> bool {
      std::vector<type::QVarDecl> formals;
      bool status = true;
      auto methodName = m->getMethodIdent().getName();
      auto tmp = super;
      type::QMethod *override = nullptr;
      int overrideIdx = -1;
      while (tmp) {
        if (tmp->getMethods().count(methodName)) {
          auto &p = tmp->getMethods()[methodName];
          override = p.second.get();
          overrideIdx = p.first;
          break;
        }
        tmp = tmp->getParent();
      }

      if (override) {
        if (m->getParams().size() != override->getFormals().size()) {
          numErrors++;
          logger.log(
              *m,
              "overriding method with wrong number of parameters. Expected ",
              std::to_string(override->getFormals().size()),
              " params, but got ", std::to_string(m->getParams().size()));
          return false;
        }

        for (auto pair : llvm::zip(m->getParams(), override->getFormals())) {
          auto &mParam = std::get<0>(pair);
          auto &formal = std::get<1>(pair);
          if (mParam->getType().getName() != formal.type->getName()) {
            logger.log(*mParam, "expected type <", formal.type->getName(),
                       "> but got <", mParam->getType().getName(), ">");
            numErrors++;
            return false;
          }
        }
      }

      for (auto &p : m->getParams()) {
        auto pType = tdb.getType(p->getType().getName());
        if (!pType) {
          numErrors++;
          logger.log(*p, "parameter type <", p->getType().getName(),
                     "> does not exist.");
          status = false;
        } else {
          formals.push_back({pType, p->getVar().getName()});
        }
      }

      auto retType = tdb.getType(m->getReturnType().getName());
      if (!retType) {
        logger.log(m->getReturnType(), "return does not exist");
        status = false;
        numErrors++;
      }

      if (!status)
        return false;
      type->insertMethod(methodName, retType, formals);
      return true;
    };
    process(&clss->getConstructor());
    for (auto &m : clss->getMethods()) {
      process(m.get());
    }
  }

  return numErrors;
}

static bool hasCircularInheritance(const Class &clss, type::QTypeDB &tdb) {
  auto *superIdent = clss.getSuper();
  if (!superIdent)
    return false;

  auto &superName = superIdent->getName();
  auto *qtype = tdb.getType(superName);
  auto &typeName = clss.getClassIdent().getName();
  while (qtype) {
    if (qtype->getName() == typeName)
      return true;
    qtype = qtype->getParent();
  }
  return false;
}

Status verify(type::QTypeDB &tdb, SourceLogger &logger,
              const ast::TranslationUnit &tu) {
  Env environment;
  int numErrors = 0;
  numErrors += registerClasses(logger, tdb, tu.getClasses());
  numErrors += processSuperTypes(logger, tdb, tu.getClasses());
  numErrors += processMethods(logger, tdb, tu.getClasses());
  numErrors += processMembers(logger, tdb, tu.getClasses());
  for (auto &clss : tu.getClasses()) {
    if (hasCircularInheritance(*clss, tdb)) {
      logger.log(clss->getClassIdent(), "circular inheritance detected");
      return Status::ERROR;
    }
  }

  if (numErrors) {
    std::cerr << "total errors: " << numErrors << "\n";
    return Status::ERROR;
  }

  // Checking classes
  for (auto &clss : tu.getClasses()) {
    if (ClassVerifier::verify(tdb, logger, *clss) != Status::OK)
      numErrors++;
  }

  // Checking main
  if (StmtVerifier::verify(tdb, logger, tu.getCompoundStmt(), environment,
                           /*parentType*/ nullptr,
                           /*returnType*/ tdb.getIntegerType()) != Status::OK)
    numErrors++;

  if (!numErrors)
    return Status::OK;

  std::cerr << "total errors: " << numErrors << "\n";
  return Status::ERROR;
}

} // namespace sema

namespace compiler {
StatusOr<TypeCheckedObject> TypeCheck(ParsedObject parsedObject) {
  auto tdb = std::unique_ptr<quick::sema::type::QTypeDB>(
      new quick::sema::type::QTypeDB());
  SourceLogger logger(parsedObject);
  Status status = sema::verify(*tdb, logger, parsedObject.getTranslationUnit());
  if (!ok(status))
    return status;

  return TypeCheckedObject(std::move(parsedObject), std::move(tdb));
}
} // namespace compiler
} // namespace quick
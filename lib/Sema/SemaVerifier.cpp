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
#include "Utils/Utils.hpp"

namespace quick {
namespace sema {

using namespace ast;

static int registerClasses(std::fstream &file, type::QTypeDB &tdb,
                           const Classes &clsses) {
  int numErrors = 0;
  // Registering all classes
  for (auto &clss : clsses) {
    auto &className = clss->getClassIdent().getName();
    if (tdb.getType(className)) {
      logError(file, clss->getClassIdent().getLocation(),
               "redefinition of class <" + className + ">");
      numErrors++;
    } else {
      tdb.registerNewType(className, nullptr);
    }
  }
  return numErrors;
}

static int processMembers(std::fstream &file, type::QTypeDB &tdb,
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
    if (StmtVerifier::verify(file, clss->getConstructor().getBody(), env,
                             /*parentType*/ type, /*returnType*/ type,
                             /*isConstructor*/ true, /*addANewScope*/ false,
                             /*addThisToScope*/ true) != Status::OK) {
      numErrors++;
    }
  }
  return numErrors;
}

static int processSuperTypes(std::fstream &file, type::QTypeDB &tdb,
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
        logError(file, clss->getSuper()->getLocation(),
                 "type not found <" + superName + ">");
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

static int processMethods(std::fstream &file, type::QTypeDB &tdb,
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
          logError(
              file, m->getLocation(),
              "overriding method with wrong number of parameters. Expected " +
                  std::to_string(override->getFormals().size()) +
                  " params, but got " + std::to_string(m->getParams().size()));
          return false;
        }

        for (auto pair : llvm::zip(m->getParams(), override->getFormals())) {
          auto &mParam = std::get<0>(pair);
          auto &formal = std::get<1>(pair);
          if (mParam->getType().getName() != formal.type->getName()) {
            logError(file, mParam->getLocation(),
                     "expected type <" + formal.type->getName() +
                         "> but got <" + mParam->getType().getName() + ">");
            numErrors++;
            return false;
          }
        }
      }

      for (auto &p : m->getParams()) {
        auto pType = tdb.getType(p->getType().getName());
        if (!pType) {
          numErrors++;
          logError(file, p->getLocation(),
                   "parameter type <" + p->getType().getName() +
                       "> does not exist.");
          status = false;
        } else {
          formals.push_back({pType, p->getVar().getName()});
        }
      }

      auto retType = tdb.getType(m->getReturnType().getName());
      if (!retType) {
        logError(file, m->getReturnType().getLocation(),
                 "return does not exist");
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

Status verify(std::fstream &f, const ast::TranslationUnit &tu) {
  Env environment;
  int numErrors = 0;
  auto &tdb = type::QTypeDB::get();

  numErrors += registerClasses(f, tdb, tu.getClasses());
  numErrors += processSuperTypes(f, tdb, tu.getClasses());
  numErrors += processMethods(f, tdb, tu.getClasses());
  numErrors += processMembers(f, tdb, tu.getClasses());
  for (auto &clss : tu.getClasses()) {
    if (hasCircularInheritance(*clss, tdb)) {
      logError(f, clss->getSuper()->getLocation(),
               "circular inheritance detected");
      return Status::ERROR;
    }
  }

  if (numErrors) {
    std::cerr << "total errors: " << numErrors << "\n";
    return Status::ERROR;
  }

  // Checking classes
  for (auto &clss : tu.getClasses()) {
    if (ClassVerifier::verify(f, *clss) != Status::OK)
      numErrors++;
  }

  // Checking main
  if (StmtVerifier::verify(f, tu.getCompoundStmt(), environment,
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
  Status status = sema::verify(parsedObject.getFile(),
                         parsedObject.getTranslationUnit());
  if (status != Status::OK)
    return status;

  return TypeCheckedObject(std::move(parsedObject));
}
} // namespace compiler
} // namespace quick
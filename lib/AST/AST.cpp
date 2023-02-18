//===--- AST.cpp ------------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
#include "AST/AST.hpp"

namespace quick::ast {

bool CompoundStmt::hasReturn() const {
  for (auto &stmt : *this) {
    // Checking if Compound statement has a return
    if (stmt->getKind() == Statement::Kind::Return)
      return true;

    // Checking if all paths of an If statement return
    if (auto *ifStmt = stmt->as_a<If>()) {
      if (!ifStmt->getElseBlock())
        continue;
      if (ifStmt->getIfBlock().hasReturn() &&
          ifStmt->getElseBlock()->hasReturn())
        return true;
    }
  }
  return false;
}

} // namespace quick::ast
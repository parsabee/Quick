//===--- Utils.hpp ----------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// Utilities
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_UTILS_UTILS_HPP
#define QUICK_UTILS_UTILS_HPP

#include "AST/AST.hpp"
#include "AST/Location.hpp"

#include "llvm/IR/IRBuilder.h"

#include <string>

namespace llvm {
class InsertPointGuard {
  IRBuilderBase &Builder;
  AssertingVH<BasicBlock> Block;
  BasicBlock::iterator Point;
  DebugLoc DbgLoc;

public:
  InsertPointGuard(IRBuilderBase &B)
      : Builder(B), Block(B.GetInsertBlock()), Point(B.GetInsertPoint()),
        DbgLoc(B.getCurrentDebugLocation()) {}

  InsertPointGuard(const InsertPointGuard &) = delete;
  InsertPointGuard &operator=(const InsertPointGuard &) = delete;

  ~InsertPointGuard() {
    Builder.restoreIP(IRBuilderBase::InsertPoint(Block, Point));
    Builder.SetCurrentDebugLocation(DbgLoc);
  }
};
}

namespace quick {
  
/// Seeks location `loc` in the `file` and prints it, as well as the `message`
void logError(std::fstream &file, const ast::Location &loc,
              const std::string &message,
              const std::string &fname = "<unknown-file>");

/// Prints `node`s code as well as `message`
void logError(const ast::ASTNode &node, const std::string &message);

/// Prints `message`
void logError(const std::string &message);

} // namespace quick

#endif // QUICK_UTILS_UTILS_HPP

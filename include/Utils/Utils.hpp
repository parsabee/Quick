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

#include "llvm/IR/IRBuilder.h"
#include <assert.h>

namespace llvm {

//===----------------------------------------------------------------------===//
// FIXME: For some reason, llvm::IRBuilderBase::InsertPointGuard segfaults at
//  destruct time. I have no idea why. I copied the code over and it works
//  fine. Figure out why, reproducible on other machines ...
// This class is copied over from llvm/IR/IRBuilder.h IRBuilderBase::
//===----------------------------------------------------------------------===//
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

#define ASSERT(OBJECT)                                                         \
  [&]() {                                                                      \
    auto *tmp = (OBJECT);                                                      \
    assert(tmp);                                                               \
    return tmp;                                                                \
  }()

} // namespace llvm

#endif // QUICK_UTILS_UTILS_HPP

//===--- SemaVerifier.hpp ----------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines the SemaVerifier class.
// SemaVerifier verifies the semantics of the program.
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_SEMA_SEMAVERIFIER_HPP
#define QUICK_SEMA_SEMAVERIFIER_HPP

#include "AST/AST.hpp"
#include "Utils/Status.hpp"

#include <fstream>

namespace quick::sema {

class SemaVerifier {
  std::fstream &file;
  const ast::TranslationUnit &tu;

public:
  SemaVerifier(std::fstream &f, const ast::TranslationUnit &tu)
      : file(f), tu(tu) {}

  // Returns OK if code checks out, ERROR otherwise
  Status verify();
};

} // namespace quick::sema

#endif // QUICK_SEMA_SEMAVERIFIER_HPP

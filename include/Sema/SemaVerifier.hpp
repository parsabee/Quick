//===--- SemaVerifier.hpp ----------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file declares the seam::verify function.
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_SEMA_SEMAVERIFIER_HPP
#define QUICK_SEMA_SEMAVERIFIER_HPP

#include "AST/AST.hpp"
#include "Utils/Status.hpp"

#include <fstream>

namespace quick::sema {

// Returns OK if code checks out, ERROR otherwise
Status verify(std::fstream &f, const ast::TranslationUnit &tu);

} // namespace quick::sema

#endif // QUICK_SEMA_SEMAVERIFIER_HPP

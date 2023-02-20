//===--- SemaVerifier.hpp ----------------------------------------*- C++
//-*-===//
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
#include "Utils/Logger.hpp"
#include "Utils/Status.hpp"
#include "Sema/QTypeDB.hpp"

namespace quick::sema {
// Returns OK if code checks out, ERROR otherwise
Status verify(type::QTypeDB &tdb, SourceLogger &logger,
              const ast::TranslationUnit &tu);
} // namespace quick::sema

#endif // QUICK_SEMA_SEMAVERIFIER_HPP

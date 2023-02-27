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
#include "Sema/QTypeDB.hpp"
#include "Utils/Logger.hpp"
#include "Utils/Status.hpp"

namespace quick::sema {
/// Returns OK if code checks out, ERROR otherwise
Status verify(type::QTypeDB &, SourceLogger &, const ast::TranslationUnit &);

/// Verifies the signature of all types and their methods in a translation unit
/// and registers them in QTypeDB, it is called by `verify()'.
Status verifyAndRegisterTypesAndMethodSignatures(type::QTypeDB &,
                                                 SourceLogger &,
                                                 const ast::TranslationUnit &);

/// Verifies the definitions of type methods in a translation unit
Status verifyMethodDefinitions(type::QTypeDB &, SourceLogger &,
                               const ast::TranslationUnit &);

/// Verifies the main script
Status verifyMain(type::QTypeDB &, SourceLogger &,
                  const ast::TranslationUnit &);
} // namespace quick::sema

#endif // QUICK_SEMA_SEMAVERIFIER_HPP

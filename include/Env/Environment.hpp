//===--- Environment.hpp ----------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines the Environment type a.k.a scope
//
//===----------------------------------------------------------------------===//

#ifndef QUACK_NESTEDENVIRONMENT_HPP
#define QUACK_NESTEDENVIRONMENT_HPP

#include "Sema/QType.hpp"

namespace quick {

/// ===-------------------------------------------------------------------=== //
/// LocalEnvironment - A scope is a local environment, which is a map of
/// strings to types
/// ===-------------------------------------------------------------------=== //
template <typename T>
class LocalEnvironment : public llvm::DenseMap<llvm::StringRef, T> {
public:
  using llvm::DenseMap<llvm::StringRef, T>::DenseMap;
  LocalEnvironment(const LocalEnvironment &) = default;
  LocalEnvironment &operator=(const LocalEnvironment &) = default;
  LocalEnvironment(LocalEnvironment &&) = default;
  LocalEnvironment &operator=(LocalEnvironment &&) = default;
  void dump() {
    llvm::errs() << "{";
    for (auto &[p, t] : *this) {
      llvm::errs() << p << " ";
    }
    llvm::errs() << "}\n";
  }
};

/// ===-------------------------------------------------------------------=== //
/// NestedEnvironment - A nested environment, which is a stack of
/// scopes(local environments), implemented with a small vector
/// ===-------------------------------------------------------------------=== //
template <typename T>
class NestedEnvironment : public llvm::SmallVector<LocalEnvironment<T>, 3> {
public:
  bool isInCurrentScope(llvm::StringRef) const;
  bool contains(llvm::StringRef) const;
  T lookup(llvm::StringRef) const;
  LocalEnvironment<T> &addNewScope();
  LocalEnvironment<T> &getCurrentScope();
  LocalEnvironment<T> popCurrentScope();
  /// Merges another scope with the current scope
  bool mergeScope(const LocalEnvironment<T> &s);
};

template <typename T>
bool NestedEnvironment<T>::isInCurrentScope(llvm::StringRef name) const {
  return this->back().count(name);
}

template <typename T>
bool NestedEnvironment<T>::contains(llvm::StringRef name) const {
  return lookup(name) != T();
}

template <typename T>
T NestedEnvironment<T>::lookup(llvm::StringRef name) const {
  for (auto it = this->rbegin(); it != this->rend(); it++)
    if (auto t = it->lookup(name))
      return t;
  return T();
}

template <typename T> LocalEnvironment<T> &NestedEnvironment<T>::addNewScope() {
  this->push_back(LocalEnvironment<T>());
  return this->back();
}

template <typename T>
LocalEnvironment<T> &NestedEnvironment<T>::getCurrentScope() {
  return this->back();
}

template <typename T>
LocalEnvironment<T> NestedEnvironment<T>::popCurrentScope() {
  LocalEnvironment<T> s = std::move(this->back());
  this->pop_back();
  return std::move(s);
}

/// Merges scope s with the current scope
template <typename T>
bool NestedEnvironment<T>::mergeScope(const LocalEnvironment<T> &s) {
  auto &curScope = this->back();
  for (auto [k, t1] : s) {
    if (auto t2 = curScope.lookup(k)) {
      if (t2 != t1 && !t1->isDescendentOf(t2))
        return false;
    } else {
      curScope.insert({k, t1});
    }
  }
  return true;
}

namespace sema {
using Env = NestedEnvironment<type::QType *>;
using Scope = LocalEnvironment<type::QType *>;
Scope And(const Scope &first, const Scope &other);
} // namespace sema

namespace codegen {
using LLVMEnv = NestedEnvironment<llvm::Value *>;
using LLVMScope = LocalEnvironment<llvm::Value *>;
} // namespace codegen

} // namespace quick
#endif // QUACK_NESTEDENVIRONMENT_HPP

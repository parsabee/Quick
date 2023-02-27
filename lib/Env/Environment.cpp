//===--- Environment.hpp ----------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Env/Environment.hpp"

namespace quick {
namespace sema {
Scope And(const Scope &first, const Scope &other) {
  Scope s;
  for (const auto &[k, t1] : first) {
    if (auto *t2 = other.lookup(k)) {
      // If the types are the same or if there is a common ancestor,
      // insert it to the 'and' scope
      if (t1 == t2)
        s.insert({k, t1});
      else if (auto lca = t1->lowestCommonAncestor(t2))
        s.insert({k, lca});
    }
  }
  return s;
}
} // namespace sema
} // namespace quick
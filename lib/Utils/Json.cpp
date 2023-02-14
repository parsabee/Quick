//===--- Json.cpp -----------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Utils/Json.hpp"

namespace json {

void DictionaryNode::dump(PrintContext pctx) const {
  pctx.println("{");
  pctx.indent();
  size_t i = 0;
  for (const auto &[key, val] : *this) {
    pctx.printIndent();
    pctx.print("\"", key, "\": ");
    val->dump(pctx);
    if (i == this->size() - 1)
      pctx.println();
    else
      pctx.println(",");
    i++;
  }
  pctx.dedent();
  pctx.printIndent();
  pctx.print("}");
}

void ListNode::dump(PrintContext pctx) const {
  pctx.println("[");
  pctx.indent();
  size_t i = 0;
  for (const auto &val : *this) {
    pctx.printIndent();
    val->dump(pctx);
    if (i == this->size() - 1)
      pctx.println();
    else
      pctx.println(",");
    i++;
  }
  pctx.dedent();
  pctx.printIndent();
  pctx.print("]");
}

void StringNode::dump(PrintContext pctx) const {
  pctx.print("\"", *this, "\"");
}
} // namespace json
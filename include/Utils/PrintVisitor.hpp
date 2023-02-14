//===--- PrintVisitor.hpp ---------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// Defines an ast print utility
//
//===----------------------------------------------------------------------===//

#ifndef QUACK_PRINTVISITOR_HPP
#define QUACK_PRINTVISITOR_HPP

#include "AST/AST.hpp"
#include "AST/ASTVisitor.hpp"
#include <iostream>
#include <unordered_map>
#include <vector>

namespace quick::ast {

//===----------------------------------------------------------------------===//
// PrintVisitor - pretty prints the ast
//===----------------------------------------------------------------------===//
class PrintVisitor : public ASTVisitor<PrintVisitor, void> {
  enum class IndentationType {
    DiagBar, /// " `"
    VertBar, /// " |"
    Space,   /// "  "
    HorzBar  /// " -"
  };

  std::unordered_map<IndentationType, const char *> _indentationTypeLookUp{
      {IndentationType::DiagBar, " \u2514"}, /// unicode box drawing characters
      {IndentationType::VertBar, " \u2502"},
      {IndentationType::HorzBar, "\u2500"},
      {IndentationType::Space, "  "}};

  mutable std::vector<IndentationType> _indentation;

  inline void indent(IndentationType indentationType) const {
    if (!_indentation.empty() &&
        _indentation.back() == IndentationType::DiagBar) {
      if (_indentation.back() == IndentationType::DiagBar) {
        _indentation.pop_back();
        _indentation.push_back(IndentationType::Space);
      }
    }
    _indentation.push_back(indentationType);
  }

  inline void dedent() const {
    if (!_indentation.empty()) {
      _indentation.pop_back();
    }
  }

  /// prints string to out stream
  void emit(const std::string &str) const {
    for (const auto i : _indentation) {
      std::cout << _indentationTypeLookUp.at(i);
    }
    std::cout << _indentationTypeLookUp.at(IndentationType::HorzBar) << str;
  }

  void emitln(const std::string &str) const {
    emit(str);
    std::cout << std::endl;
  }

public:
  PrintVisitor() = default;

#define NODE_HANDLER(NODE) void visit##NODE(const NODE &);
#include "AST/ASTNodes.def"
};

//===----------------------------------------------------------------------===//
// dumps the ast TranslationUnit 
//===----------------------------------------------------------------------===//
void print(const ASTNode &);

} // namespace quick::ast
#endif // QUACK_PRINTVISITOR_HPP

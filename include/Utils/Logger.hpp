//===--- Logger.hpp ---------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// Logging utility
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_UTILS_LOGGER_HPP
#define QUICK_UTILS_LOGGER_HPP

#include "AST/AST.hpp"
#include "llvm/Support/raw_os_ostream.h"

namespace quick {

class Logger {
protected:
  llvm::raw_ostream &out;

public:
  explicit Logger(llvm::raw_ostream &o = llvm::errs()) : out(o) {}
  template <typename... Args>
  void log(Args... args) {
    ([&](const auto &arg) { out << arg; }(args), ...);
    out << "\n";
  }
};

namespace compiler {
class Source; //forward ref;
}

class SourceLogger : public Logger{
  compiler::Source &source;
  void printCodeAtLocation(const ast::Location &);
  void prefix();

public:
  explicit SourceLogger(compiler::Source &s,
                        llvm::raw_ostream &o = llvm::errs())
      : Logger(o), source(s) {}

  template <typename... Args> void log_node(const ast::ASTNode &node, Args... args) {
    prefix();
    out << " @ " << node.getLocation().toString() << " : ";
    Logger::log(args...);
    printCodeAtLocation(node.getLocation());
  }
};

} // namespace quick

#endif // QUICK_UTILS_LOGGER_HPP
//===--- Driver.hpp - Driver for the Parser library -------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines the interface for the front-end parser. This file builds
// with -frtti as Flex and Bison require typeid, therefore this file must not
// be included in sources with -fno-rtti compile flag.
//
//===----------------------------------------------------------------------===//

#ifndef QUACK_PARSER_DRIVER_HPP
#define QUACK_PARSER_DRIVER_HPP

#include "AST/AST.hpp"
#include "Parser.ypp.hpp"

#include <memory>

namespace quick::parser {
class Driver;
}

// Flex needs this macro for our custom driver
#define YY_DECL yy::parser::symbol_type yylex(quick::parser::Driver &drv)
YY_DECL;

using namespace quick::ast;

namespace quick::parser {
class Driver {

  friend yy::parser;

  std::unique_ptr<TranslationUnit> _root;

  /// The token's location used by the scanner.
  yy::location _location;

  /// The name of the file being parsed.
  std::string _curFile;

  /// Whether to generate parser debug traces.
  bool _traceParsing;
  /// Whether to generate scanner debug traces.
  bool _traceScanning;

  /// Handling the scanner.
  void _scanBegin();
  void _scanEnd();
  int parse();

public:
  explicit Driver(bool traceParsing = false, bool traceScanning = false)
      : _traceParsing(traceParsing), _traceScanning(traceScanning) {}

  int parseFile(const std::string &filename);
  void setRoot(std::unique_ptr<TranslationUnit> root) {
    _root = std::move(root);
  }

  TranslationUnit &getRoot() { return *_root; }
  std::unique_ptr<TranslationUnit> releaseRoot() { return std::move(_root); }

  void setTraceParsing(bool traceParsing) { _traceParsing = traceParsing; }
  bool getTraceParsing() const { return _traceParsing; }

  void setTraceScanning(bool traceScanning) { _traceScanning = traceScanning; }
  bool getTraceScanning() const { return _traceScanning; }

  yy::location &getLocation() { return _location; }
};

} // namespace quick::parser

#endif // QUACK_PARSER_DRIVER_HPP

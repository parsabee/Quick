//===--- Driver.cpp - Driver for the Parser library -------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Parser/Driver.hpp"
#include "Compiler/Pipeline.hpp"

namespace quick {
namespace parser {

int Driver::parse(const std::string &filename) {
  _curFile = filename;
  _location.initialize(&_curFile);
  _scanBegin();
  yy::parser parser(*this);
#ifdef YYDEBUG
  parser.set_debug_level(_traceParsing);
#endif // YYDEBUG
  int res = parser.parse();
  _scanEnd();
  return res;
}
} // namespace parser

namespace compiler {
StatusOr<ParsedObject> Parse(const std::string &filename) {
  parser::Driver drv;

  std::fstream file(filename);
  if (!file.is_open()) {
    std::cerr << "can't open file " << filename << "\n";
    return Status::INVALID_SOURCE;
  }

  int programError = drv.parse(filename);
  if (programError)
    return Status::PARSE_ERROR;

  return ParsedObject(drv.releaseRoot(), std::move(file), filename);
}
} // namespace compiler
} // namespace quick
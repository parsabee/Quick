//===--- Driver.cpp - Driver for the Parser library -------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Parser/Driver.hpp"
#include "Compiler/Pipeline.hpp"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FileUtilities.h"
#include "llvm/Support/raw_ostream.h"

namespace quick {
namespace parser {

int Driver::parse() {
  _scanBegin();
  yy::parser parser(*this);
#ifdef YYDEBUG
  parser.set_debug_level(_traceParsing);
#endif // YYDEBUG
  int res = parser.parse();
  _scanEnd();
  return res;
}

int Driver::parseFile(const std::string &filename) {
  _curFile = filename;
  _location.initialize(&_curFile);
  return parse();
}

} // namespace parser

namespace compiler {
StatusOr<ParsedObject> ParseFile(const std::string &filename) {
  parser::Driver drv;

  std::fstream file(filename);
  if (!file.is_open()) {
    std::cerr << "can't open file " << filename << "\n";
    return Status::INVALID_SOURCE;
  }

  int programError = drv.parseFile(filename);
  if (programError)
    return Status::PARSE_ERROR;
  return ParsedObject(drv.releaseRoot(), std::move(file), filename);
}

/// Writes string to a temp file and parses that
StatusOr<ParsedObject> ParseString(const std::string &source) {
  llvm::SmallString<128> temp_file_path;
  llvm::sys::fs::createTemporaryFile("anonymous", "qk", temp_file_path);
  llvm::FileRemover remover(temp_file_path);
  std::error_code err;
  llvm::raw_fd_ostream file(temp_file_path, err);
  file << source;
  file.close();
  return ParseFile(temp_file_path.c_str());
}

} // namespace compiler
} // namespace quick
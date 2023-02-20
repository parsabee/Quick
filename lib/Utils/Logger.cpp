//===--- Logger.cpp ---------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Utils/Logger.hpp"
#include "Compiler/Object.hpp"
#include <algorithm>
#include <fstream>
#include <iostream>

namespace quick {

void SourceLogger::prefix() { out << source.getSourceName() << ": "; }

void SourceLogger::printCodeAtLocation(const ast::Location &loc) {
  auto &file = source.getFile();
  auto curPos = file.tellg();
  file.clear();
  file.seekg(0);
  int curLine = 1;
  for (; curLine < loc.l_from && !file.eof(); curLine++)
    file.ignore(std::numeric_limits<int>::max(), '\n');

  std::string line;
  for (; curLine < loc.l_to + 1 && !file.eof(); curLine++) {
    std::getline(file, line);
    out << "    " << curLine << ")    " << line << "\n";
  }

  out << "\n\n";
  file.clear();
  file.seekg(curPos);
}

}
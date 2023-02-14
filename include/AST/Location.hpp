//===--- Location.hpp - location in the source code metadata ----*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines Location class for holding location in the source code
// information
//
//===----------------------------------------------------------------------===//

#ifndef QUACK_AST_LOCATION_HPP
#define QUACK_AST_LOCATION_HPP

#include <sstream>
#include <string>

namespace quick::ast {

//===----------------------------------------------------------------------===//
// Location - location in the source file, each ASTNode has a location
//===----------------------------------------------------------------------===//
struct Location {
  const unsigned l_from, l_to, c_from, c_to;
  const char *filename;

  Location(unsigned lineFrom, unsigned columnFrom, unsigned lineTo,
           unsigned columnTo, const char *filename = "<anonymous-file>")
      : l_from(lineFrom), l_to(lineTo), c_from(columnFrom), c_to(columnTo),
        filename(filename) {}
  Location()
      : l_from(0), l_to(0), c_from(0), c_to(0), filename("<anonymous-file>") {}
  bool operator==(const Location &o) const {
    return (l_from == o.l_from && l_to == o.l_to && c_from == o.c_from &&
            c_to == o.c_to);
  }
  bool operator!=(const Location &o) const { return !(*this == o); }
  //    const std::string& getFileName() const    { return _fileName; };
  std::string toString() const {
    std::stringstream ss;
    ss << l_from << "." << c_from << "-" << l_to << "." << c_to;
    return ss.str();
  }
};

} // namespace quick::ast
#endif // QUACK_AST_LOCATION_HPP

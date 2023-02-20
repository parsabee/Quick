//===--- QTypeDB.hpp --------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines QTypeDB which is the database and registery for all Quick
// types
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_TYPE_QTYPEDB_HPP
#define QUICK_TYPE_QTYPEDB_HPP

#include "CodeGen/IRType.hpp"
#include "Sema/QType.hpp"
#include "Utils/Status.hpp"

namespace quick {

namespace compiler {
class TypeCheckedObject;
class ParsedObject;
StatusOr<TypeCheckedObject> TypeCheck(ParsedObject parsedObject);
}

namespace sema {
namespace type {

/// ===-------------------------------------------------------------------=== //
/// QTypeDB - All Quick types are registered in this singleton
/// ===-------------------------------------------------------------------=== //
class QTypeDB : public std::unordered_map<std::string, std::unique_ptr<QType>> {
  friend StatusOr<compiler::TypeCheckedObject>
  compiler::TypeCheck(compiler::ParsedObject parsedObject);
  QTypeDB();

public:
  QTypeDB &operator=(const QType &) = delete;
  QTypeDB(const QTypeDB &) = delete;

  /// Adds a new type to the data base
  QType *registerNewType(const std::string &name, QType *parent);

  /// Builtin types
  QType *getObjectType() const;
  QType *getIntegerType() const;
  QType *getFloatType() const;
  QType *getBoolType() const;
  QType *getStringType() const;
  QType *getNothingType() const;
  QType *getType(llvm::StringRef) const;
  std::unique_ptr<json::JSONNode> toJson();
};

} // namespace type
} // namespace sema
} // namespace quick
#endif // QUICK_TYPE_QTYPEDB_HPP

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

namespace quick {
namespace sema {
namespace type {

/// ===-------------------------------------------------------------------=== //
/// QTypeDB - All Quick types are registered in this singleton
/// ===-------------------------------------------------------------------=== //
class QTypeDB : public std::unordered_map<std::string, std::unique_ptr<QType>> {
  QTypeDB();

public:
  QTypeDB &operator=(const QType &) = delete;
  QTypeDB(const QTypeDB &) = delete;

  static QTypeDB &get() {
    static QTypeDB db;
    return db;
  }

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

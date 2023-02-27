//===--- QTypeDB.cpp --------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Sema/QTypeDB.hpp"
#include "Sema/QType.hpp"
#include "Utils/Utils.hpp"

using namespace json;

namespace quick::sema::type {

/// The following functions register builtin types in the type data base upon
/// initialization

/// Registers Nothing
static void buildObjectType(QTypeDB &db) {
  auto *objType = db.getObjectType();
  auto *boolType = db.getBoolType();
  auto *strType = db.getStringType();
  objType->insertMethod(op::ComparisonOperator[op::EQ], boolType,
                        {{objType, "other"}});
  objType->insertMethod(op::ComparisonOperator[op::NE], boolType,
                        {{objType, "other"}});
  objType->insertMethod("__str__", strType, {});
}

/// Registers Boolean
static void buildBoolType(QTypeDB &db) {
  auto *boolType = db.getBoolType();
  auto *strType = db.getStringType();
  boolType->insertMethod(op::ComparisonOperator[op::EQ], boolType,
                         {{boolType, "other"}});
  boolType->insertMethod(op::ComparisonOperator[op::NE], boolType,
                         {{boolType, "other"}});
  boolType->insertMethod("__str__", strType, {});
  boolType->insertMethod(op::UnaryOperator[op::NOT], boolType, {});
}

/// Registers Nothing
static void buildNothingType(QTypeDB &db) {
  auto *objType = db.getObjectType();
  auto *boolType = db.getBoolType();
  auto *nothingType = db.getNothingType();
  auto *strType = db.getStringType();
  nothingType->insertMethod(op::ComparisonOperator[op::EQ], boolType,
                            {{objType, "other"}});
  nothingType->insertMethod(op::ComparisonOperator[op::NE], boolType,
                            {{objType, "other"}});
  nothingType->insertMethod("__str__", strType, {});
}

/// Registers Integer
static void buildIntegerType(QTypeDB &db) {
  auto *boolType = db.getBoolType();
  auto *intType = db.getIntegerType();
  auto *stringType = db.getStringType();

  auto &methodTable = intType->getMethods();

  // Integer supports all arithmetic operations
  for (const auto &op : op::ArithmeticOperator) {
    intType->insertMethod(op, intType, {{intType, "other"}});
  }

  // Integer supports all comparison operations
  for (const auto &op : op::ComparisonOperator) {
    intType->insertMethod(op, boolType, {{intType, "other"}});
  }

  intType->insertMethod("__str__", stringType, {});
  intType->insertMethod(op::UnaryOperator[op::NEG], intType, {});
}

/// Registers Float
static void buildFloatType(QTypeDB &db) {
  auto *floatType = db.getFloatType();
  auto &methodTable = floatType->getMethods();
  auto *stringType = db.getStringType();

  // Float supports all arithmetic operations
  for (const auto &op : op::ArithmeticOperator) {
    floatType->insertMethod(op, floatType, {{floatType, "other"}});
  }

  // Bool type must be registered before int
  auto *boolType = db.getBoolType();

  // Float supports all comparison operations
  for (const auto &op : op::ComparisonOperator) {
    floatType->insertMethod(op, boolType, {{floatType, "other"}});
  }

  floatType->insertMethod("__str__", stringType, {});
  floatType->insertMethod(op::UnaryOperator[op::NEG], floatType, {});
}

/// Registers String
static void buildStringType(QTypeDB &db) {
  auto *objType = db.getObjectType();
  auto *boolType = db.getBoolType();
  auto *strType = db.getStringType();
  strType->insertMethod(op::ComparisonOperator[op::EQ], boolType,
                        {{objType, "other"}});
  strType->insertMethod(op::ComparisonOperator[op::NE], boolType,
                        {{objType, "other"}});
  strType->insertMethod("__str__", strType, {});
  strType->insertMethod(op::ArithmeticOperator[op::ADD], strType,
                        {{strType, "other"}});
}

/// Initialize the data base and register builtin types
QTypeDB::QTypeDB() {
  // registering builtin types
  auto *obj = registerNewType(ObjectStr, nullptr);
  registerNewType(BoolStr, nullptr);
  registerNewType(IntegerStr, nullptr);
  registerNewType(FloatStr, nullptr);
  registerNewType(NothingStr, obj);
  registerNewType(StringStr, obj);

  // building builtin types method tables
  buildObjectType(*this);
  buildBoolType(*this);
  buildNothingType(*this);
  buildIntegerType(*this);
  buildFloatType(*this);
  buildStringType(*this);
}

QType *QTypeDB::getObjectType() const {
  return ASSERT(this->at(ObjectStr).get());
}

QType *QTypeDB::getIntegerType() const {
  return ASSERT(this->at(IntegerStr).get());
}

QType *QTypeDB::getFloatType() const {
  return ASSERT(this->at(FloatStr).get());
}

QType *QTypeDB::getBoolType() const { return ASSERT(this->at(BoolStr).get()); }

QType *QTypeDB::getStringType() const {
  return ASSERT(this->at(StringStr).get());
}

QType *QTypeDB::getNothingType() const {
  return ASSERT(this->at(NothingStr).get());
}

QType *QTypeDB::registerNewType(const std::string &name, QType *parent) {
  assert(this->find(name) == this->end() && "Type is already registered");
  auto newType = std::make_unique<QType>(parent, name);
  this->insert({name, std::move(newType)});
  return this->at(name).get();
}

std::unique_ptr<JSONNode> QTypeDB::toJson() {
  auto l = make<ListNode>();
  for (auto &it : *this) {
    l->push_back(it.second->toJson());
  }
  return l;
}

QType *QTypeDB::getType(llvm::StringRef name) const {
  if (this->count(name) == 0)
    return nullptr;

  return this->at(name).get();
}

} // namespace quick::sema::type
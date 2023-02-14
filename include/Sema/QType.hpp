//===--- QType.hpp ----------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines QType that captures the semantics of quick types
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_TYPE_QTYPE_HPP
#define QUICK_TYPE_QTYPE_HPP

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"

#include "AST/AST.hpp"
#include "Utils/Json.hpp"

#include <map>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace quick {
namespace sema {
namespace type {

template <typename T> using Table = llvm::StringMap<std::pair<int, T>>;

class QType;

/// ===-------------------------------------------------------------------=== //
/// Builtin types
/// ===-------------------------------------------------------------------=== //
constexpr char ObjectStr[] = "Object";
constexpr char IntegerStr[] = "Integer";
constexpr char FloatStr[] = "Float";
constexpr char StringStr[] = "String";
constexpr char BoolStr[] = "Boolean";
constexpr char NothingStr[] = "Nothing";

static inline bool isPrimitive(const std::string &str) {
  return (str == IntegerStr || str == FloatStr || str == BoolStr ||
          str == StringStr || str == NothingStr);
}

static inline bool isBuiltin(const std::string &str) {
  return (str == IntegerStr || str == FloatStr || str == BoolStr ||
          str == StringStr || str == NothingStr || str == ObjectStr);
}

/// ===-------------------------------------------------------------------=== //
/// Unary/Binary operation keywords
/// ===-------------------------------------------------------------------=== //
namespace op {
enum ARITH_OP { ADD = 0, SUB, MUL, DIV, MOD };
constexpr const char *ArithmeticOperator[] = {"__add__", "__sub__", "__mul__",
                                              "__div__", "__mod__"};

enum UN_OP { NEG = 0, NOT };
constexpr const char *UnaryOperator[] = {"__neg__", "__not__"};

enum COMP_OP { EQ = 0, NE, LE, LT, GE, GT };
constexpr const char *ComparisonOperator[] = {"__eq__", "__ne__", "__le__",
                                              "__lt__", "__ge__", "__gt__"};
} // namespace op

struct QVarDecl {
  QType *type;
  std::string name;
};

/// ===-------------------------------------------------------------------=== //
/// A Quick type's method internal representation
/// ===-------------------------------------------------------------------=== //
class QMethod {
public:
  enum class Kind { Override, New, Constructor };
  QMethod(QType *type, llvm::ArrayRef<QVarDecl> actuals, QType *retType,
          llvm::StringRef name, Kind kind = Kind::New)
      : type(type), formals(actuals.begin(), actuals.end()),
        returnType(retType), name(name), kind(kind) {}
  bool operator==(const QMethod &);
  bool operator!=(const QMethod &other) { return !(*this == other); }
  const llvm::SmallVector<QVarDecl, 6> &getFormals() const { return formals; }
  QType *getReturnType() const { return returnType; }
  std::string getName() const { return name; }
  QType *getType() const { return type; }
  Kind getKind() const { return kind; }

  std::unique_ptr<json::JSONNode> toJson();

private:
  llvm::SmallVector<QVarDecl, 6> formals;
  QType *returnType;
  std::string name;
  QType *type;
  Kind kind;
};

/// ===-------------------------------------------------------------------=== //
/// A Quick type internal representation
/// ===-------------------------------------------------------------------=== //
class QType {
  QType *parent;
  std::string name;
  std::unique_ptr<QMethod> constructor = nullptr;
  Table<QType *> members;
  Table<std::unique_ptr<QMethod>> methods;
  int numNewMethods = 0;

public:
  QType(QType *parent, llvm::StringRef name) : parent(parent), name(name) {}
  bool operator==(const QType &);
  bool operator!=(const QType &);

  std::string mangleName(llvm::StringRef mname,
                         llvm::ArrayRef<QType *> argTypes) const;

  void setConstructor(std::unique_ptr<QMethod> c) {
    constructor = std::move(c);
  }

  // Getters
  QType *getParent() { return parent; }
  QMethod *getConstructor() { return constructor.get(); }
  void setParent(QType *p) { parent = p; }
  const std::string &getName() const { return name; }
  auto &getMembers() { return members; }
  auto &getMethods() { return methods; }

  // Builds types members and methods
  bool insertMethod(const std::string &name, QType *retType,
                    llvm::ArrayRef<QVarDecl> = {});
  bool insertMember(const QVarDecl &);

  // Utility
  QType *lowestCommonAncestor(const QType *) const;
  bool isDescendentOf(const QType *) const;
  QMethod *lookUpMethod(llvm::StringRef, llvm::ArrayRef<QType *> argTypes);
  QType *lookUpMember(llvm::StringRef);
  std::unique_ptr<json::JSONNode> toJson();
};

} // namespace type
} // namespace sema
} // namespace quick

#endif // QUICK_TYPE_QTYPE_HPP

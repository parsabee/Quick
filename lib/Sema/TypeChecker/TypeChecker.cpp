//===--- TypeChecker.hpp ----------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Sema/TypeChecker.hpp"
#include "Utils/Utils.hpp"

namespace quick::sema {

using namespace llvm;
using namespace ast;

type::QType *TypeChecker::visitIntegerLiteral(const IntegerLiteral &) {
  return tdb.getIntegerType();
}

type::QType *TypeChecker::visitFloatLiteral(const FloatLiteral &) {
  return tdb.getFloatType();
}

type::QType *TypeChecker::visitBoolLiteral(const BoolLiteral &) {
  return tdb.getBoolType();
}

type::QType *TypeChecker::visitStringLiteral(const StringLiteral &) {
  return tdb.getStringType();
}

type::QType *TypeChecker::visitNothingLiteral(const NothingLiteral &) {
  return tdb.getNothingType();
}

type::QType *TypeChecker::visitBinaryOperator(const BinaryOperator &binOp) {
  auto *lhsType = visitExpression(binOp.getLHS());
  if (!lhsType)
    return nullptr;

  auto *rhsType = visitExpression(binOp.getRHS());
  if (!rhsType)
    return nullptr;

  auto checkCall = [&](const char *m) -> type::QType * {
    if (const auto *method = lhsType->lookUpMethod(m, {rhsType})) {
      auto &args = method->getFormals();
      if (args.size() != 1) {
        logError(file, binOp.getLocation(),
                 "expected 1 argument to the binary operator");
        return nullptr;
      }

      auto *argType = args.back().type;
      if (argType != rhsType && !rhsType->isDescendentOf(argType)) {
        logError(file, binOp.getLocation(),
                 "can't use argument of type <" + rhsType->getName() +
                     "> where type <" + argType->getName() + "> is expected.");
        return nullptr;
      }

      return method->getReturnType();
    } else {
      logError(file, binOp.getLocation(),
               std::string("method <") + m + "> with parameters <" +
                   rhsType->getName() + "> is not defined for type <" +
                   lhsType->getName() + ">");
      return nullptr;
    }
  };

  switch (binOp.getOpCode()) {
  case BinaryOperator::Operator::Plus:
    return checkCall(type::op::ArithmeticOperator[type::op::ADD]);
  case BinaryOperator::Operator::Minus:
    return checkCall(type::op::ArithmeticOperator[type::op::SUB]);
  case BinaryOperator::Operator::Times:
    return checkCall(type::op::ArithmeticOperator[type::op::MUL]);
  case BinaryOperator::Operator::Divide:
    return checkCall(type::op::ArithmeticOperator[type::op::DIV]);
  case BinaryOperator::Operator::Modulo:
    return checkCall(type::op::ArithmeticOperator[type::op::MOD]);
  case BinaryOperator::Operator::Less:
    return checkCall(type::op::ComparisonOperator[type::op::LT]);
  case BinaryOperator::Operator::LessEqual:
    return checkCall(type::op::ComparisonOperator[type::op::LE]);
  case BinaryOperator::Operator::Greater:
    return checkCall(type::op::ComparisonOperator[type::op::GT]);
  case BinaryOperator::Operator::GreaterEqual:
    return checkCall(type::op::ComparisonOperator[type::op::GE]);
  case BinaryOperator::Operator::Equals:
    return checkCall(type::op::ComparisonOperator[type::op::EQ]);
  case BinaryOperator::Operator::NotEquals:
    return checkCall(type::op::ComparisonOperator[type::op::NE]);
  }
  assert(false && "shouldn't get to this point");
  return nullptr;
}

type::QType *TypeChecker::visitUnaryOperator(const UnaryOperator &unOp) {
  type::QType *type = visitExpression(unOp.getOperand());
  if (!type)
    return nullptr;

  auto checkUnOp = [&](const char *op) -> type::QType * {
    if (const auto *method = type->lookUpMethod(op, {})) {
      if (!method->getFormals().empty()) {
        logError(file, unOp.getLocation(),
                 "unary operator doesn't take any arguments");
        return nullptr;
      }

      return method->getReturnType();
    }
    logError(file, unOp.getLocation(),
             "object of type '" + type->getName() + "' has no operator '" + op +
                 "'");
    return nullptr;
  };

  switch (unOp.getOpCode()) {
  case UnaryOperator::Operator::Neg:
    return checkUnOp(type::op::UnaryOperator[type::op::NEG]);
  case UnaryOperator::Operator::Not:
    return checkUnOp(type::op::UnaryOperator[type::op::NOT]);
  default:
    return nullptr;
  }
}

type::QType *TypeChecker::visitCall(const Call &call) {
  // typechecking arguments
  std::vector<type::QType *> argtypes;
  for (auto &arg : call.getArgs()) {
    auto argType = visitExpression(*arg);
    if (!argType)
      return nullptr;
    argtypes.push_back(argType);
  }

  auto &callee = call.getCallee();
  if (auto identExpr = callee.as_a<IdentifierExpression>()) {
    // it has to be a constructor
    auto type = tdb.getType(identExpr->getVarName());
    if (!type) {
      logError(file, call.getLocation(),
               "calling a constructor of a type that doesn't exist <" +
                   identExpr->getVarName() + ">");
      return nullptr;
    }

    if (!type->getConstructor() || type::isPrimitive(type->getName())) {
      logError(file, call.getLocation(),
               "this type has no constructor <" + type->getName() + ">");
      return nullptr;
    }

    return type;
  } else if (auto memAccess = callee.as_a<MemberAccess>()) {
    auto *objType = visitExpression(memAccess->getObject());
    if (!objType)
      return nullptr;

    auto qMethod = objType->lookUpMethod(memAccess->getVarName(), argtypes);
    if (!qMethod) {
      logError(file, call.getLocation(),
               "object of type <" + objType->getName() +
                   "> has no method named <" + memAccess->getVarName() + ">");
      return nullptr;
    }

    return qMethod->getReturnType();
  }
  return nullptr;
}

type::QType *
TypeChecker::visitMemberAccess(const MemberAccess &memberAccess) {
  auto type = visitExpression(memberAccess.getObject());
  if (!type)
    return nullptr;

  if (auto memberT = type->lookUpMember(memberAccess.getVarName()))
    return memberT;

  logError(file, memberAccess.getLocation(),
           "object of type <" + type->getName() + "> has no member <" +
               memberAccess.getVarName() + ">");
  return nullptr;
}

type::QType *
TypeChecker::visitIdentifierExpression(const IdentifierExpression &lvalue) {
  if (auto t = env.lookup(lvalue.getVar().getName()))
    return t;
  else {
    logError(file, lvalue.getLocation(),
             "identifier <" + lvalue.getVar().getName() +
                 "> is not declared in current scope");
    return nullptr;
  }
}

} // namespace quick::sema
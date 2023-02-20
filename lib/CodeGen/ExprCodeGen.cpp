//===--- ExprCodeGen.hpp ----------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "CodeGen/FnCodeGen.hpp"
#include "CodeGen/ExprCodeGen.hpp"
#include "Utils/Utils.hpp"

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"

namespace quick::codegen {

using namespace ast;
using namespace llvm;
using namespace codegen::type;
using namespace sema::type;

llvm::Value *visitIntOrFloat(IRBuilder<> &builder, llvm::Type *type, Value*literal) {
  auto alloca = builder.CreateAlloca(type);
  builder.CreateStore(literal, alloca);
  auto load = builder.CreateLoad(alloca);
  return load;
}

llvm::Value *
ExprCodeGen::visitIntegerLiteral(const ast::IntegerLiteral &integer) {
  return visitIntOrFloat(builder, builder.getInt64Ty(),
                         builder.getInt64(integer.get()));
}

llvm::Value *ExprCodeGen::visitFloatLiteral(const ast::FloatLiteral &floating) {
  return visitIntOrFloat(
      builder, builder.getDoubleTy(),
      ConstantFP::get(llvm::Type::getDoubleTy(llvmCntx), floating.get()));
}

llvm::Value *ExprCodeGen::visitBoolLiteral(const ast::BoolLiteral &boolean) {
  return visitIntOrFloat(builder, builder.getInt8Ty(),
                         builder.getInt8(boolean.get()));
}

std::string interpEscapeSeq(const char *str) {
  std::stringstream ss;
  for (; *str != '\0'; str++) {
    if (*str == '\\') {
      str++;
      if (*str == '\0')
        break;
      char c = *str;
      switch (c) {
      case 'n':
        ss << '\n';
        break;
      case 't':
        ss << '\t';
        break;
      default:
        ss << '\\';
        ss << c;
      }
    } else if (*str != '\"') {
      ss << *str;
    }
  }
  return ss.str();
}

llvm::Value *ExprCodeGen::visitStringLiteral(const ast::StringLiteral &strLit) {
  llvm::Value *str =
      builder.CreateGlobalStringPtr(interpEscapeSeq(strLit.get().c_str()));
  Function *func = module.getFunction("String_create");
  if (!func) {
    logger.log("Link Error: function <String_create> not found");
    return nullptr;
  }
  return builder.CreateCall(func, {str});
}

llvm::Value *ExprCodeGen::visitNothingLiteral(const ast::NothingLiteral &) {
  Function *func = module.getFunction("Nothing_create");
  if (!func) {
    logger.log("Link Error: function <Nothing_create> not found");
    return nullptr;
  }
  return builder.CreateCall(func, {});
}

llvm::Value *ExprCodeGen::visitCall(const ast::Call &call) {
  // generating code for the arguments
  auto argVals =
      llvm::to_vector<4>(llvm::map_range(call.getArgs(), [&](auto &expr) {
        auto v = visitExpression(*expr);
        assert(v);
        return v;
      }));

  if (auto *ident = call.getCallee().as_a<IdentifierExpression>()) {
    // constructor
    auto *irType = typeRegistery.get(ident->getVarName());
    assert(irType);
    auto *constructor = getOrCreateFnSym(ident->getVarName() + "_create",
                                         module, nullptr, false);

    // casting args to the appropriate type
    llvm::SmallVector<llvm::Value *, 4> castVals;
    for (unsigned i = 0, end = argVals.size(); i < end; i++)
      castVals.push_back(
          builder.CreateBitCast(argVals[i], constructor->getArg(i)->getType()));

    return builder.CreateCall(constructor, castVals);
  } else if (auto *memAccess = call.getCallee().as_a<MemberAccess>()) {
    // member access
    auto &methodName = call.getCallee().getVarName();
    auto *obj = visitExpression(memAccess->getObject());
    assert(obj);
    auto irType = typeRegistery.get(obj->getType());
    assert(irType);
    return irType->dispatch(builder, methodName.c_str(), obj, argVals, &module);
  }

  // error
  return nullptr;
}

llvm::Value *ExprCodeGen::visitIdentifierExpression(
    const ast::IdentifierExpression &lvalue) {
  auto val = llvmEnv.lookup(lvalue.getVar().getName());
  assert(val && "value must exist in the environment");
  auto load = builder.CreateLoad(val);
  return load;
}

llvm::Value *
ExprCodeGen::visitMemberAccess(const ast::MemberAccess &memberAccess) {
  auto obj = visitExpression(memberAccess.getObject());
  assert(obj);

  auto &member = memberAccess.getVarName();
  auto *irType = typeRegistery.get(obj->getType());
  auto complexType = static_cast<ComplexType *>(irType);
  assert(complexType);
  assert(complexType->getMembers().count(member));
  auto &memberEntry = complexType->getMembers()[member];
  auto ptr = builder.CreateStructGEP(obj, memberEntry.first);
  return builder.CreateLoad(ptr);
}

llvm::Value *ExprCodeGen::visitUnaryOperator(const ast::UnaryOperator &unOp) {
  Value *val = visitExpression(unOp.getOperand());
  if (!val)
    return nullptr;

  // Getting the corresponding LLVM type for the type
  auto *llvmType = typeRegistery.get(val->getType());
  assert(llvmType);

  auto dispatch = [&](const char *m) {
    return llvmType->dispatch(builder, m, val, {});
  };

  // Creating dispatches (method calls) based on the operator string
  switch (unOp.getOpCode()) {
  case UnaryOperator::Operator::Neg:
    return dispatch(op::UnaryOperator[op::NEG]);
  case UnaryOperator::Operator::Not:
    return dispatch(op::UnaryOperator[op::NOT]);
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
/// Emits IR for left and right hand side of the expression, then creates
/// appropriate dispatches for the binary operation: primitives will
/// use primitive binary operations like (add, div, sub, ...), and complex
/// types will use dynamic method dispatch
//===----------------------------------------------------------------------===//
llvm::Value *
ExprCodeGen::visitBinaryOperator(const ast::BinaryOperator &binOp) {
  // Emit IR for left hand side
  Value *lhs = visitExpression(binOp.getLHS());
  if (!lhs)
    return nullptr;

  // Getting the corresponding LLVM type for the type of left hand side
  auto *llvmType = typeRegistery.get(lhs->getType());
  assert(llvmType);

  // Emitting IR for the right hand side
  Value *rhs = visitExpression(binOp.getRHS());
  if (!rhs)
    return nullptr;

  auto dispatch = [&](const char *m) {
    return llvmType->dispatch(builder, m, lhs, {rhs});
  };

  // Creating dispatches (method calls) based on the operator string
  using namespace sema::type::op;
  switch (binOp.getOpCode()) {
  case BinaryOperator::Operator::Plus:
    return dispatch(ArithmeticOperator[ADD]);
  case BinaryOperator::Operator::Minus:
    return dispatch(ArithmeticOperator[SUB]);
  case BinaryOperator::Operator::Times:
    return dispatch(ArithmeticOperator[MUL]);
  case BinaryOperator::Operator::Divide:
    return dispatch(ArithmeticOperator[DIV]);
  case BinaryOperator::Operator::Modulo:
    return dispatch(ArithmeticOperator[MOD]);
  case BinaryOperator::Operator::Less:
    return dispatch(ComparisonOperator[LT]);
  case BinaryOperator::Operator::LessEqual:
    return dispatch(ComparisonOperator[LE]);
  case BinaryOperator::Operator::Greater:
    return dispatch(ComparisonOperator[GT]);
  case BinaryOperator::Operator::GreaterEqual:
    return dispatch(ComparisonOperator[GE]);
  case BinaryOperator::Operator::Equals:
    return dispatch(ComparisonOperator[EQ]);
  case BinaryOperator::Operator::NotEquals:
    return dispatch(ComparisonOperator[NE]);
  }
  return nullptr;
}

}
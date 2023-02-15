//===--- PrintVisitor.hpp ---------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Utils/PrintVisitor.hpp"
#include "AST/AST.hpp"

namespace quick::ast {

void PrintVisitor::visitTranslationUnit(
    const TranslationUnit &translationUnit) {
  emitln("TranslationUnit ");
  indent(IndentationType::DiagBar);
  visitCompoundStmt(translationUnit.getCompoundStmt());
  dedent();
}

void PrintVisitor::visitIntegerLiteral(const IntegerLiteral &integerLiteral) {
  emitln("IntegerLiteral "
         "(" +
         std::to_string(integerLiteral.get()) + ")");
}

void PrintVisitor::visitFloatLiteral(const FloatLiteral &floatLiteral) {
  emitln("FloatLiteral "
         "(" +
         std::to_string(floatLiteral.get()) + ")");
}

void PrintVisitor::visitBoolLiteral(const BoolLiteral &boolLiteral) {
  emitln("BoolLiteral "
         "(" +
         std::to_string(boolLiteral.get()) + ")");
}

void PrintVisitor::visitStringLiteral(const StringLiteral &stringLiteral) {
  emitln("StringLiteral "
         "(" +
         stringLiteral.get() + ")");
}

void PrintVisitor::visitNothingLiteral(const NothingLiteral &) {
  emitln("None");
}

void PrintVisitor::visitIdentifier(const Identifier &identifier) {
  emitln("Identifier "
         "(" +
         identifier.getName() + ")");
}

void PrintVisitor::visitVarDecl(const VarDecl &varDecl) {
  emitln("VarDecl ");
  indent(IndentationType::VertBar);
  visitIdentifier(varDecl.getVar());
  dedent();
  indent(IndentationType::DiagBar);
  visitIdentifier(varDecl.getType());
  dedent();
}

void PrintVisitor::visitStaticMemberDecl(
    const quick::ast::StaticMemberDecl &memberDecl) {
  emitln("StaticMemberDecl ");
  indent(IndentationType::VertBar);
  visitIdentifier(memberDecl.getObject().getMember());
  dedent();
  indent(IndentationType::DiagBar);
  visitIdentifier(memberDecl.getType());
  dedent();
}

void PrintVisitor::visitIdentifierExpression(
    const IdentifierExpression &lValue) {
  emitln("IdentifierExpression ");
  indent(IndentationType::DiagBar);
  visitIdentifier(lValue.getVar());
  dedent();
}

void PrintVisitor::visitCompoundStmt(const CompoundStmt &compoundStmt) {
  emitln("CompoundStmt ");
  auto retVal = true;
  /// stmt a unique_ptr to a statement
  for (auto stmt = compoundStmt.begin(); stmt != compoundStmt.end(); stmt++) {
    if (stmt == --compoundStmt.end())
      indent(IndentationType::DiagBar);
    else
      indent(IndentationType::VertBar);
    this->visitStatement(*stmt->get());
    dedent();
  }
}

void PrintVisitor::visitAssignment(const Assignment &assignment) {
  emitln("Assignment ");
  indent(IndentationType::VertBar);
  visitLValue(assignment.getLHS());
  dedent();
  indent(IndentationType::DiagBar);
  visitExpression(assignment.getRHS());
  dedent();
}

void PrintVisitor::visitStaticAssignment(const StaticAssignment &assignment) {
  emitln("StaticAssignment ");
  indent(IndentationType::VertBar);
  visitDecl(assignment.getDecl());
  dedent();
  indent(IndentationType::DiagBar);
  visitExpression(assignment.getRHS());
  dedent();
}

void PrintVisitor::visitValueStmt(const ValueStmt &valueStmt) {
  emitln("ValueStmt ");
  indent(IndentationType::DiagBar);
  visitExpression(valueStmt.getExpr());
  dedent();
}

void PrintVisitor::visitBinaryOperator(const BinaryOperator &binaryOperator) {
  static std::map<BinaryOperator::Operator, std::string> OpCodeStringLookUp{
      {BinaryOperator::Operator::Plus, "+"},
      {BinaryOperator::Operator::Minus, "-"},
      {BinaryOperator::Operator::Divide, "/"},
      {BinaryOperator::Operator::Times, "*"},
      {BinaryOperator::Operator::Equals, "=="},
      {BinaryOperator::Operator::Greater, ">"},
      {BinaryOperator::Operator::GreaterEqual, ">="},
      {BinaryOperator::Operator::Less, "<"},
      {BinaryOperator::Operator::LessEqual, "<="}};

  emitln("BinaryOperator (" + OpCodeStringLookUp[binaryOperator.getOpCode()] +
         ")");
  indent(IndentationType::VertBar);
  visitExpression(binaryOperator.getRHS());
  dedent();
  indent(IndentationType::DiagBar);
  visitExpression(binaryOperator.getLHS());
  dedent();
}

void PrintVisitor::visitReturn(const Return &returnStmt) {
  emitln("Return ");
  if (returnStmt.getRetVal()) {
    indent(IndentationType::DiagBar);
    auto status = false;
    visitExpression(*(returnStmt.getRetVal()));
    dedent();
  }
}

void PrintVisitor::visitIf(const If &ifStmt) {
  emitln("If ");
  indent(IndentationType::VertBar);
  emitln("Cond ");
  indent(IndentationType::DiagBar);
  visitExpression(ifStmt.getCond());
  dedent();

  if (ifStmt.getElseBlock() != nullptr) {
    dedent();
    indent(IndentationType::VertBar);
  } else {
    dedent();
    indent(IndentationType::DiagBar);
  }

  emitln("Then ");
  indent(IndentationType::DiagBar);
  visitCompoundStmt(ifStmt.getIfBlock());
  dedent();

  if (ifStmt.getElseBlock() != nullptr) {
    dedent();
    indent(IndentationType::DiagBar);
    emitln("Else ");
    indent(IndentationType::DiagBar);
    visitCompoundStmt(*ifStmt.getElseBlock());
    dedent();
  }

  dedent();
}

void PrintVisitor::visitWhile(const While &whileStmt) {
  emitln("While ");
  indent(IndentationType::VertBar);
  emitln("Cond ");
  indent(IndentationType::DiagBar);

  visitExpression(whileStmt.getCond());
  dedent();
  dedent();

  indent(IndentationType::DiagBar);
  emitln("Block ");
  indent(IndentationType::DiagBar);
  visitCompoundStmt(whileStmt.getBlock());
  dedent();

  dedent();
  dedent();
}

void PrintVisitor::visitCall(const Call &call) {
  /// TODO
  emitln("Call ");
}

void PrintVisitor::visitUnaryOperator(const UnaryOperator &unOp) {
  switch (unOp.getOpCode()) {
  case UnaryOperator::Operator::Neg:
    emitln("UnaryOperator (-)");
    break;
  case UnaryOperator::Operator::Not:
    emitln("UnaryOperator (!)");
    break;
  }
}

void PrintVisitor::visitPrintStatement(
    const quick::ast::PrintStatement &print) {
  emitln("Print ");
  indent(IndentationType::DiagBar);
  for (auto &expr : *print.getArgs()) {
    visitExpression(*expr);
  }
  dedent();
}

void PrintVisitor::visitMemberAccess(const MemberAccess &memberAccess) {}

void PrintVisitor::visitTypeSwitch(const ast::TypeSwitch &typeSwitch) {
  emitln("TypeSwitch");
  indent(IndentationType::DiagBar);
  for (auto &c : typeSwitch.getCases()) {
    visitTypeSwitchCase(*c);
  }
  dedent();
}

void PrintVisitor::visitTypeSwitchCase(
    const ast::TypeSwitchCase &typeSwitchCase) {
  emitln("TypeSwitchCase");
  indent(IndentationType::DiagBar);
  visitVarDecl(typeSwitchCase.getVarDecl());
  indent(IndentationType::DiagBar);
  visitCompoundStmt(typeSwitchCase.getBlock());
  dedent();
  dedent();
}

void PrintVisitor::visitTypeAlternatives(
    const ast::TypeAlternatives &typeAlts) {
  // TODO
}

void PrintVisitor::visitClass(const ast::Class &clss) {
  // TODO
}

void PrintVisitor::visitClasses(const ast::Classes &clsses) {
  // TODO
}

void PrintVisitor::visitMethod(const ast::Method &method) {
  // TODO
}

void PrintVisitor::visitMethods(const ast::Methods &methods) {
  // TODO
}

void PrintVisitor::visitArguments(const ast::Arguments &args) {
  // TODO
}

void PrintVisitor::visitParameters(const ast::Parameters &params) {
  // TODO
}

void print(const ASTNode &node) {
  PrintVisitor printVisitor;
}

} // namespace quick::ast
//===--- ASTNodeDownCastTest.cpp --------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// ASTNode DownCast "as_a<>()" method test
//
//===----------------------------------------------------------------------===//

/* All Nodes Defined as Not Expected Here:

NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
NOT_EXPECT_NODE_AS_A(node, Identifier);
NOT_EXPECT_NODE_AS_A(node, Expression);
NOT_EXPECT_NODE_AS_A(node, Statement);
NOT_EXPECT_NODE_AS_A(node, Decl);
NOT_EXPECT_NODE_AS_A(node, LValue);
NOT_EXPECT_NODE_AS_A(node, Arguments);
NOT_EXPECT_NODE_AS_A(node, Classes);
NOT_EXPECT_NODE_AS_A(node, Class);
NOT_EXPECT_NODE_AS_A(node, Parameters);
NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
NOT_EXPECT_NODE_AS_A(node, VarDecl);
NOT_EXPECT_NODE_AS_A(node, Methods);
NOT_EXPECT_NODE_AS_A(node, Method);
NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
NOT_EXPECT_NODE_AS_A(node, StringLiteral);
NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
NOT_EXPECT_NODE_AS_A(node, Call);
NOT_EXPECT_NODE_AS_A(node, MemberAccess);
NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
NOT_EXPECT_NODE_AS_A(node, Assignment);
NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
NOT_EXPECT_NODE_AS_A(node, ValueStmt);
NOT_EXPECT_NODE_AS_A(node, Return);
NOT_EXPECT_NODE_AS_A(node, If);
NOT_EXPECT_NODE_AS_A(node, While);
NOT_EXPECT_NODE_AS_A(node, PrintStatement);
NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);

 */

#include "AST/AST.hpp"
#include "gtest/gtest.h"

using namespace quick::ast;
using namespace std;

#define EXPECT_NODE_AS_A(NODE, AS_A)                                           \
  auto(AS_A##_##NODE) = (NODE)->as_a<AS_A>();                                  \
  EXPECT_NE((AS_A##_##NODE), nullptr)

#define NOT_EXPECT_NODE_AS_A(NODE, AS_A)                                       \
  auto(AS_A##_##NODE) = (NODE)->as_a<AS_A>();                                  \
  EXPECT_EQ((AS_A##_##NODE), nullptr)

TEST(ASTNodeTest, DownCastIdentifier) {
  ASTNode *node = new Identifier(Location(), "hello world");
  EXPECT_NODE_AS_A(node, Identifier);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Expression);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, LValue);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastIntegerLiteral) {
  ASTNode *node = new IntegerLiteral(Location(), 1);
  EXPECT_NODE_AS_A(node, IntegerLiteral);
  EXPECT_NODE_AS_A(node, Expression);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, LValue);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastFloatLiteral) {
  ASTNode *node = new FloatLiteral(Location(), 1.0);
  EXPECT_NODE_AS_A(node, FloatLiteral);
  EXPECT_NODE_AS_A(node, Expression);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, LValue);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastStringLiteral) {
  ASTNode *node = new StringLiteral(Location(), "hello");
  EXPECT_NODE_AS_A(node, StringLiteral);
  EXPECT_NODE_AS_A(node, Expression);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, LValue);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastBoolLiteral) {
  ASTNode *node = new BoolLiteral(Location(), true);
  EXPECT_NODE_AS_A(node, BoolLiteral);
  EXPECT_NODE_AS_A(node, Expression);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, LValue);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastNothingLiteral) {
  ASTNode *node = new NothingLiteral(Location());
  EXPECT_NODE_AS_A(node, NothingLiteral);
  EXPECT_NODE_AS_A(node, Expression);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, LValue);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastIdentifierExpression) {
  ASTNode *node = new IdentifierExpression(
      Location(), make_unique<Identifier>(Location(), "hello"));
  EXPECT_NODE_AS_A(node, IdentifierExpression);
  EXPECT_NODE_AS_A(node, LValue);
  EXPECT_NODE_AS_A(node, Expression);
  EXPECT_NODE_AS_A(Expression_node, LValue);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastBinaryOperator) {
  ASTNode *node =
      new BinaryOperator(Location(), BinaryOperator::Operator::Plus,
                         make_unique<IntegerLiteral>(Location(), 1),
                         make_unique<IntegerLiteral>(Location(), 1));
  EXPECT_NODE_AS_A(node, BinaryOperator);
  EXPECT_NODE_AS_A(node, Expression);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, LValue);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastUnaryOperator) {
  ASTNode *node = new UnaryOperator(Location(), UnaryOperator::Operator::Neg,
                                    make_unique<IntegerLiteral>(Location(), 1));
  EXPECT_NODE_AS_A(node, UnaryOperator);
  EXPECT_NODE_AS_A(node, Expression);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, LValue);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastCall) {
  ASTNode *node =
      new Call(Location(),
               std::unique_ptr<LValue>(new IdentifierExpression(
                   Location(), make_unique<Identifier>(Location(), "hello"))),
               make_unique<Arguments>(Location()));
  EXPECT_NODE_AS_A(node, Call);
  EXPECT_NODE_AS_A(node, Expression);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, LValue);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, MemberAccess);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
  delete node;
}

TEST(ASTNodeTest, DownCastMemberAccess) {
  ASTNode *node =
      new MemberAccess(Location(), make_unique<Identifier>(Location(), "hello"),
                       make_unique<IntegerLiteral>(Location(), 1));
  EXPECT_NODE_AS_A(node, MemberAccess);
  EXPECT_NODE_AS_A(node, Expression);
  EXPECT_NODE_AS_A(node, LValue);

  NOT_EXPECT_NODE_AS_A(node, TranslationUnit);
  NOT_EXPECT_NODE_AS_A(node, Identifier);
  NOT_EXPECT_NODE_AS_A(node, Statement);
  NOT_EXPECT_NODE_AS_A(node, Decl);
  NOT_EXPECT_NODE_AS_A(node, Arguments);
  NOT_EXPECT_NODE_AS_A(node, Classes);
  NOT_EXPECT_NODE_AS_A(node, Class);
  NOT_EXPECT_NODE_AS_A(node, Parameters);
  NOT_EXPECT_NODE_AS_A(node, StaticMemberDecl);
  NOT_EXPECT_NODE_AS_A(node, VarDecl);
  NOT_EXPECT_NODE_AS_A(node, Methods);
  NOT_EXPECT_NODE_AS_A(node, Method);
  NOT_EXPECT_NODE_AS_A(node, IntegerLiteral);
  NOT_EXPECT_NODE_AS_A(node, FloatLiteral);
  NOT_EXPECT_NODE_AS_A(node, BoolLiteral);
  NOT_EXPECT_NODE_AS_A(node, StringLiteral);
  NOT_EXPECT_NODE_AS_A(node, NothingLiteral);
  NOT_EXPECT_NODE_AS_A(node, IdentifierExpression);
  NOT_EXPECT_NODE_AS_A(node, BinaryOperator);
  NOT_EXPECT_NODE_AS_A(node, UnaryOperator);
  NOT_EXPECT_NODE_AS_A(node, Call);
  NOT_EXPECT_NODE_AS_A(node, CompoundStmt);
  NOT_EXPECT_NODE_AS_A(node, Assignment);
  NOT_EXPECT_NODE_AS_A(node, StaticAssignment);
  NOT_EXPECT_NODE_AS_A(node, ValueStmt);
  NOT_EXPECT_NODE_AS_A(node, Return);
  NOT_EXPECT_NODE_AS_A(node, If);
  NOT_EXPECT_NODE_AS_A(node, While);
  NOT_EXPECT_NODE_AS_A(node, PrintStatement);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitch);
  NOT_EXPECT_NODE_AS_A(node, TypeSwitchCase);
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

#include "AST/AST.hpp"
#include "gtest/gtest.h"

using namespace quick::ast;
using namespace std;

/// Testing Cast
/// helper macro to cast and check if result is not null
#define EXPECT_AS_A(NODE, AS_A)                                                \
  auto(AS_A##_node) = (NODE)->as_a<AS_A>();                                    \
  EXPECT_NE((AS_A##_node), nullptr)

#define NOT_EXPECT_AS_A(NODE, AS_A)                                            \
  auto(AS_A##_node) = (NODE)->as_a<AS_A>();                                    \
  EXPECT_EQ((AS_A##_node), nullptr)

TEST(ASTNodeTest, DownCastIdentifier) {
  ASTNode *node = new Identifier(Location(), "hello world");
  EXPECT_AS_A(node, Identifier);
  NOT_EXPECT_AS_A(node, TranslationUnit);
  NOT_EXPECT_AS_A(node, Expression);
  NOT_EXPECT_AS_A(node, Statement);
  NOT_EXPECT_AS_A(node, Decl);
  NOT_EXPECT_AS_A(node, Arguments);
  NOT_EXPECT_AS_A(node, Classes);
  NOT_EXPECT_AS_A(node, Class);
  NOT_EXPECT_AS_A(node, Parameters);
  NOT_EXPECT_AS_A(node, Methods);
  NOT_EXPECT_AS_A(node, Method);
  delete node;
}

TEST(ASTNodeTest, DownCastIntegerLiteral) {
  ASTNode *node = new IntegerLiteral(Location(), 1);
  EXPECT_AS_A(node, IntegerLiteral);
  EXPECT_AS_A(node, Expression);
  delete node;
}

TEST(ASTNodeTest, DownCastFloatLiteral) {
  ASTNode *node = new FloatLiteral(Location(), 1.0);
  EXPECT_AS_A(node, FloatLiteral);
  EXPECT_AS_A(node, Expression);
  delete node;
}

TEST(ASTNodeTest, DownCastStringLiteral) {
  ASTNode *node = new StringLiteral(Location(), "hello");
  EXPECT_AS_A(node, StringLiteral);
  EXPECT_AS_A(node, Expression);
  delete node;
}

TEST(ASTNodeTest, DownCastBoolLiteral) {
  ASTNode *node = new BoolLiteral(Location(), true);
  EXPECT_AS_A(node, BoolLiteral);
  EXPECT_AS_A(node, Expression);
  delete node;
}

TEST(ASTNodeTest, DownCastNothingLiteral) {
  ASTNode *node = new NothingLiteral(Location());
  EXPECT_AS_A(node, NothingLiteral);
  EXPECT_AS_A(node, Expression);
  delete node;
}

TEST(ASTNodeTest, DownCastIdentifierExpression) {
  ASTNode *node = new IdentifierExpression(
      Location(), make_unique<Identifier>(Location(), "hello"));
  EXPECT_AS_A(node, IdentifierExpression);
  EXPECT_AS_A(node, Expression);
  delete node;
}

TEST(ASTNodeTest, DownCastBinaryOperator) {
  ASTNode *node =
      new BinaryOperator(Location(), BinaryOperator::Operator::Plus,
                         make_unique<IntegerLiteral>(Location(), 1),
                         make_unique<IntegerLiteral>(Location(), 1));
  EXPECT_AS_A(node, BinaryOperator);
  EXPECT_AS_A(node, Expression);
  delete node;
}

TEST(ASTNodeTest, DownCastUnaryOperator) {
  ASTNode *node = new UnaryOperator(Location(), UnaryOperator::Operator::Neg,
                                    make_unique<IntegerLiteral>(Location(), 1));
  EXPECT_AS_A(node, UnaryOperator);
  EXPECT_AS_A(node, Expression);
  delete node;
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

#include "Sema/TypeChecker.hpp"
#include "gtest/gtest.h"
#include "Compiler/Pipeline.hpp"

using namespace quick;
using namespace quick::ast;
using namespace quick::compiler;

TEST(TypeCheckerTest, TestVisitMemberAccessOK2) {
  auto source = "class A { this.a = 1; }"
                "a = A(); a.a;";
  auto status_or_parsed = ParseString(source);
  EXPECT_TRUE(status_or_parsed.ok());
  auto status_or_typechecked = TypeCheck(status_or_parsed.ValueOrDie());
  EXPECT_TRUE(status_or_typechecked.ok());
}

TEST(TypeCheckerTest, TestVisitMemberAccessOK) {
  auto source = "class A { this.a = 1; }";
  auto status_or_parsed = ParseString(source);
  EXPECT_TRUE(status_or_parsed.ok());
  auto status_or_typechecked = TypeCheck(status_or_parsed.ValueOrDie());
  EXPECT_TRUE(status_or_typechecked.ok());
}

TEST(TypeCheckerTest, TestVisitMemberAccessBAD) {
  auto source = "class A { this.a = 1; }"
                "a = A(); a.b;";
  auto status_or_parsed = ParseString(source);
  EXPECT_TRUE(status_or_parsed.ok());
  auto status_or_typechecked = TypeCheck(status_or_parsed.ValueOrDie());
  EXPECT_FALSE(status_or_typechecked.ok());
}

TEST(TypeCheckerTest, TestVisitIdentifierExpressionOK) {
  auto source = "a: Integer = 1; a;";
  auto status_or_parsed = ParseString(source);
  EXPECT_TRUE(status_or_parsed.ok());
  auto status_or_typechecked = TypeCheck(status_or_parsed.ValueOrDie());
  EXPECT_TRUE(status_or_typechecked.ok());
}

TEST(TypeCheckerTest, TestVisitIdentifierExpressionBAD) {
  auto source = "a;";
  auto status_or_parsed = ParseString(source);
  EXPECT_TRUE(status_or_parsed.ok());
  auto status_or_typechecked = TypeCheck(status_or_parsed.ValueOrDie());
  EXPECT_FALSE(status_or_typechecked.ok());
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

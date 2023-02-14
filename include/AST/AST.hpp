//===--- AST.hpp - AST Nodes ------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file defines the nodes in the abstract syntax tree of a Quick program
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_AST_AST_HPP
#define QUICK_AST_AST_HPP

#include "AST/Location.hpp"
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace quick::ast {

/// ===-------------------------------------------------------------------=== //
/// Generates an ID for every ast node: quick::ast::NodeID::<node class name>
/// ===-------------------------------------------------------------------=== //
enum class NodeID {
  ASTNode = 0,
#define ABSTRACT_NODE_HANDLER(NODE) NODE,
#define NODE_HANDLER(NODE) NODE,
#include "AST/ASTNodes.def"
};

#define UPDATE_ID(NODE) this->_id = NodeID::NODE

/// ===-------------------------------------------------------------------=== //
/// ASTNode - Abstract node class, the base for every other AST node
/// ===-------------------------------------------------------------------=== //
class ASTNode {
  const Location _loc;

protected:
  NodeID _id = NodeID::ASTNode;
  explicit ASTNode(Location loc) : _loc(loc) {}

public:
  inline const Location &getLocation() const { return _loc; }
  inline NodeID getID() const { return _id; }
  virtual ~ASTNode() = default;
};

class CompoundStmt; // Forward ref
class Classes;

/// ===-------------------------------------------------------------------=== //
/// TranslationUnit - Root node of the AST
/// ===-------------------------------------------------------------------=== //
class TranslationUnit final : public ASTNode {
  std::unique_ptr<Classes> _classes;
  std::unique_ptr<CompoundStmt> _compoundStmt;

public:
  TranslationUnit(Location loc, std::unique_ptr<Classes> classes,
                  std::unique_ptr<CompoundStmt> compoundStmt)
      : ASTNode(loc), _classes(std::move(classes)),
        _compoundStmt(std::move(compoundStmt)) {
    UPDATE_ID(TranslationUnit);
  }

  inline const CompoundStmt &getCompoundStmt() const { return *_compoundStmt; }
  inline const Classes &getClasses() const { return *_classes; }
};

/// ===-------------------------------------------------------------------=== //
/// Statement - Abstract statement node
/// ===-------------------------------------------------------------------=== //
class Statement : public ASTNode {
public:
  enum class Kind {
    ValueStmt,
    Assignment,
    StaticAssignment,
    Return,
    If,
    While,
    Print,
    TypeSwitch,
    TypeSwitchCase
  };

  inline Kind getKind() const { return _kind; }

protected:
  Statement(Location loc, Kind kind) : ASTNode(loc), _kind(kind) {
    UPDATE_ID(Statement);
  }

private:
  Kind _kind;
};

/// ===-------------------------------------------------------------------=== //
/// Sequence, a list of AST objects
/// ===-------------------------------------------------------------------=== //
template <typename T> using Sequence = std::vector<std::unique_ptr<T>>;

/// ===-------------------------------------------------------------------=== //
/// CompoundStmt - A block of statements
/// ===-------------------------------------------------------------------=== //
class CompoundStmt final : public Sequence<Statement>, public ASTNode {
public:
  explicit CompoundStmt(const Location &loc) : ASTNode(loc) {
    UPDATE_ID(CompoundStmt);
  }
  bool hasReturn() const;
};

/// ===-------------------------------------------------------------------=== //
/// Expression - Abstract expression node class
/// ===-------------------------------------------------------------------=== //
class Expression : public ASTNode {
public:
  enum class Kind {
    Call,
    BinaryOperator,
    UnaryOperator,
    LValue,
    IntegerLiteral,
    FloatLiteral,
    BoolLiteral,
    NothingLiteral,
    StringLiteral
  };

  inline Kind getKind() const { return _kind; }

protected:
  Expression(Location loc, Kind kind) : ASTNode(loc), _kind(kind) {
    UPDATE_ID(Expression);
  }

private:
  Kind _kind;
};

/// ===-------------------------------------------------------------------=== //
/// ValueStmt - A statement with an expression
/// ===-------------------------------------------------------------------=== //
class ValueStmt final : public Statement {
  std::unique_ptr<Expression> _expr;

public:
  explicit ValueStmt(Location loc, std::unique_ptr<Expression> expr)
      : Statement(loc, Statement::Kind::ValueStmt), _expr(std::move(expr)) {
    UPDATE_ID(ValueStmt);
  }

  inline const Expression &getExpr() const { return *_expr; }
};

/// ===-------------------------------------------------------------------=== //
/// UnaryOperator - expression - operations with one operand
/// ===-------------------------------------------------------------------=== //
class UnaryOperator final : public Expression {
public:
  enum class Operator { Neg, Not };

  UnaryOperator(Location loc, Operator opCode,
                std::unique_ptr<Expression> operand)
      : Expression(loc, Expression::Kind::UnaryOperator), _opCode(opCode),
        _operand(std::move(operand)) {
    UPDATE_ID(UnaryOperator);
  }

  inline const Expression &getOperand() const { return *_operand; }
  inline const Operator getOpCode() const { return _opCode; }

private:
  Operator _opCode;
  std::unique_ptr<Expression> _operand;
};

/// ===-------------------------------------------------------------------=== //
/// BinaryOperator - operation with two operands
/// ===-------------------------------------------------------------------=== //
class BinaryOperator final : public Expression {
public:
  enum class Operator {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Equals,
    NotEquals,
    Greater,
    GreaterEqual,
    Less,
    LessEqual
  };

  BinaryOperator(Location loc, Operator opCode,
                 std::unique_ptr<Expression> leftHandSide,
                 std::unique_ptr<Expression> rightHandSide)
      : Expression(loc, Expression::Kind::BinaryOperator), _opCode(opCode),
        _leftHandSide(std::move(leftHandSide)),
        _rightHandSide(std::move(rightHandSide)) {
    UPDATE_ID(BinaryOperator);
  }

  inline Operator getOpCode() const { return _opCode; }
  inline const Expression &getLHS() const { return *_leftHandSide; }
  inline const Expression &getRHS() const { return *_rightHandSide; }

private:
  Operator _opCode;
  std::unique_ptr<Expression> _leftHandSide;
  std::unique_ptr<Expression> _rightHandSide;
};

/// ===-------------------------------------------------------------------=== //
/// Identifier - any named concept e.g. variable, type, method
/// ===-------------------------------------------------------------------=== //
class Identifier final : public ASTNode {
  const std::string _name;

public:
  Identifier(Location loc, const std::string &name)
      : ASTNode(loc), _name(name) {
    UPDATE_ID(Identifier);
  };

  inline const std::string &getName() const { return _name; }
};

/// ===-------------------------------------------------------------------=== //
/// LValue - An expression with storage
/// ===-------------------------------------------------------------------=== //
class LValue : public Expression {
public:
  enum class Kind { Ident, MemberAccess };

  inline Kind getKind() const { return _kind; }
  virtual const std::string &getVarName() const = 0;

protected:
  Kind _kind;

  LValue(Location loc, Kind kind)
      : Expression(loc, Expression::Kind::LValue), _kind(kind) {
    UPDATE_ID(LValue);
  }
};

class IdentifierExpression;

/// ===-------------------------------------------------------------------=== //
/// MemberAccess
/// ===-------------------------------------------------------------------=== //
class MemberAccess final : public LValue {
  friend class StaticMemberDecl;
  std::unique_ptr<Identifier> _member;
  std::unique_ptr<Expression> _obj;

public:
  MemberAccess(const Location loc, std::unique_ptr<Identifier> member,
               std::unique_ptr<Expression> object)
      : LValue(loc, LValue::Kind::MemberAccess), _member(std::move(member)),
        _obj(std::move(object)) {
    UPDATE_ID(MemberAccess);
  }

  inline const Identifier &getMember() const { return *_member; }
  inline const Expression &getObject() const { return *_obj; }
  inline const std::string &getVarName() const override {
    return _member->getName();
  }
};

/// ===-------------------------------------------------------------------=== //
/// IdentifierExpression - An Identifier that is an L Value
/// ===-------------------------------------------------------------------=== //
class IdentifierExpression final : public LValue {
  std::unique_ptr<Identifier> _var;

public:
  IdentifierExpression(Location loc, std::unique_ptr<Identifier> var)
      : LValue(loc, LValue::Kind::Ident), _var(std::move(var)) {
    UPDATE_ID(IdentifierExpression);
  }

  inline const Identifier &getVar() const { return *_var; }
  inline const std::string &getVarName() const override {
    return _var->getName();
  }
};

/// ===-------------------------------------------------------------------=== //
/// Decl -- abstract declaration type
/// ===-------------------------------------------------------------------=== //
class Decl : public ASTNode {
public:
  enum class Kind { Var, Member };
  inline const Identifier &getType() const { return *_type; }
  inline bool isMemberDecl() const { return _kind == Kind::Member; }
  inline Kind getKind() const { return _kind; }

protected:
  std::unique_ptr<Identifier> _type;
  Kind _kind;
  Decl(const Location &loc, std::unique_ptr<Identifier> type, Kind kind)
      : ASTNode(loc), _type(std::move(type)), _kind(kind) {
    UPDATE_ID(Decl);
  }
};

/// ===-------------------------------------------------------------------=== //
/// VarDecl - a variable declaration, <var>: <type>
/// ===-------------------------------------------------------------------=== //
class VarDecl final : public Decl {
  std::unique_ptr<IdentifierExpression> _var;

public:
  VarDecl(const Location &loc, std::unique_ptr<IdentifierExpression> var,
          std::unique_ptr<Identifier> type)
      : Decl(loc, std::move(type), Kind::Var), _var(std::move(var)) {
    UPDATE_ID(VarDecl);
  }

  inline const Identifier &getVar() const { return _var->getVar(); }
};

/// ===-------------------------------------------------------------------===
/// StaticMemberDecl - Declaration of a field member <expr>.<var>: <type>
/// ===-------------------------------------------------------------------===
class StaticMemberDecl final : public Decl {
  std::unique_ptr<MemberAccess> _object;

public:
  StaticMemberDecl(Location loc, std::unique_ptr<MemberAccess> var,
                   std::unique_ptr<Identifier> type)
      : Decl(loc, std::move(type), Kind::Member), _object(std::move(var)) {
    UPDATE_ID(StaticMemberDecl);
  }

  inline const MemberAccess &getObject() const { return *_object; }
};

/// ===-------------------------------------------------------------------=== //
/// Assignment - A variable declaration and assignment or update
/// ===-------------------------------------------------------------------=== //
class Assignment final : public Statement {
protected:
  std::unique_ptr<LValue> _lvalue;
  std::unique_ptr<Expression> _rvalue;

public:
  Assignment(Location loc, std::unique_ptr<LValue> lvalue,
             std::unique_ptr<Expression> rvalue)
      : Statement(loc, Statement::Kind::Assignment), _lvalue(std::move(lvalue)),
        _rvalue(std::move(rvalue)) {
    UPDATE_ID(Assignment);
  }

  inline const LValue &getLHS() const { return *_lvalue; }
  inline const Expression &getRHS() const { return *_rvalue; }
};

/// ===-------------------------------------------------------------------=== //
/// StaticAssignment - A variable declaration and assignment
/// ===-------------------------------------------------------------------=== //
class StaticAssignment final : public Statement {
protected:
  std::unique_ptr<Decl> _decl;
  std::unique_ptr<Expression> _rvalue;

public:
  StaticAssignment(Location loc, std::unique_ptr<Decl> decl,
                   std::unique_ptr<Expression> rvalue)
      : Statement(loc, Statement::Kind::StaticAssignment),
        _decl(std::move(decl)), _rvalue(std::move(rvalue)) {
    UPDATE_ID(StaticAssignment);
  }

  inline const Decl &getDecl() const { return *_decl; }
  inline const Expression &getRHS() const { return *_rvalue; }
};

/// ===-------------------------------------------------------------------=== //
/// Literal template
/// ===-------------------------------------------------------------------=== //
template <typename T> class Literal {
  T _literal;

protected:
  explicit Literal(T literal) : _literal(std::move(literal)) {}
  
public:
  inline T get() const { return _literal; }
};

/// ===-------------------------------------------------------------------=== //
/// IntegerLiteral
/// ===-------------------------------------------------------------------=== //
class IntegerLiteral final : public Expression, public Literal<long int> {
public:
  IntegerLiteral(Location loc, long int integer)
      : Expression(loc, Expression::Kind::IntegerLiteral), Literal<long>(
                                                               integer) {
    UPDATE_ID(IntegerLiteral);
  }
};

/// ===-------------------------------------------------------------------=== //
/// FloatLiteral
/// ===-------------------------------------------------------------------=== //
class FloatLiteral final : public Expression, public Literal<double> {
public:
  FloatLiteral(Location loc, double theFloat)
      : Expression(loc, Expression::Kind::FloatLiteral), Literal<double>(
                                                             theFloat) {
    UPDATE_ID(FloatLiteral);
  }
};

/// ===-------------------------------------------------------------------=== //
/// BoolLiteral
/// ===-------------------------------------------------------------------=== //
class BoolLiteral final : public Expression, public Literal<bool> {
public:
  BoolLiteral(Location loc, bool boolean)
      : Expression(loc, Expression::Kind::BoolLiteral), Literal<bool>(boolean) {
    UPDATE_ID(BoolLiteral);
  }
};

/// ===-------------------------------------------------------------------=== //
/// NothingLiteral -- None
/// ===-------------------------------------------------------------------=== //
class NothingLiteral final : public Expression {
public:
  explicit NothingLiteral(Location loc)
      : Expression(loc, Expression::Kind::NothingLiteral) {
    UPDATE_ID(NothingLiteral);
  }
};

/// ===-------------------------------------------------------------------=== //
/// StringLiteral
/// ===-------------------------------------------------------------------=== //
class StringLiteral final : public Expression, public Literal<std::string> {
public:
  StringLiteral(Location loc, std::string text)
      : Expression(loc, Expression::Kind::StringLiteral), Literal<std::string>(
                                                              std::move(text)) {
    UPDATE_ID(StringLiteral);
  }
};

/// ===-------------------------------------------------------------------=== //
/// Return
/// ===-------------------------------------------------------------------=== //
class Return final : public Statement {
  std::unique_ptr<Expression> _retVal;

public:
  explicit Return(Location loc, std::unique_ptr<Expression> retVal)
      : Statement(loc, Statement::Kind::Return), _retVal(std::move(retVal)) {
    UPDATE_ID(Return);
  }

  inline const Expression *getRetVal() const {
    return _retVal != nullptr ? _retVal.get() : nullptr;
  }
};

/// ===-------------------------------------------------------------------=== //
/// Arguments - A list of expressions
/// ===-------------------------------------------------------------------=== //
class Arguments final : public Sequence<Expression>, public ASTNode {
public:
  explicit Arguments(const Location &loc) : ASTNode(loc) {
    UPDATE_ID(Arguments);
  }
};

/// ===-------------------------------------------------------------------=== //
/// PrintStatement
/// ===-------------------------------------------------------------------=== //
class PrintStatement final : public Statement {
  std::unique_ptr<Arguments> exprList;

public:
  PrintStatement(Location loc, std::unique_ptr<Arguments> exprList)
      : Statement(loc, Statement::Kind::Print), exprList(std::move(exprList)) {
    UPDATE_ID(PrintStatement);
  }

  inline auto &getArgs() const { return exprList; }
};

/// ===-------------------------------------------------------------------=== //
/// Parameters - a.k.a formals, a list of variable declarations
/// ===-------------------------------------------------------------------=== //
class Parameters final : public Sequence<VarDecl>, public ASTNode {
public:
  explicit Parameters(const Location &loc) : ASTNode(loc) {
    UPDATE_ID(Parameters);
  }
};

/// ===-------------------------------------------------------------------=== //
/// Method
/// ===-------------------------------------------------------------------=== //
class Method final : public ASTNode {
  std::unique_ptr<Parameters> _params;
  std::unique_ptr<Identifier> _name;
  std::unique_ptr<Identifier> _returnType;
  std::unique_ptr<CompoundStmt> _body;

public:
  Method(const Location &loc, std::unique_ptr<Parameters> params,
         std::unique_ptr<Identifier> name,
         std::unique_ptr<Identifier> returnType,
         std::unique_ptr<CompoundStmt> body)
      : ASTNode(loc), _params(std::move(params)), _name(std::move(name)),
        _returnType(std::move(returnType)), _body(std::move(body)) {
    UPDATE_ID(Method);
  }

  inline const Parameters &getParams() const { return *_params; }
  inline const Identifier &getMethodIdent() const { return *_name; }
  inline const Identifier &getReturnType() const { return *_returnType; }
  inline const CompoundStmt &getBody() const { return *_body; }
};

/// ===-------------------------------------------------------------------=== //
/// Methods - A list of Methods
/// ===-------------------------------------------------------------------=== //
class Methods final : public Sequence<Method>, public ASTNode {
public:
  explicit Methods(const Location &loc) : ASTNode(loc) { UPDATE_ID(Methods); }
};

/// ===-------------------------------------------------------------------=== //
/// Class
/// ===-------------------------------------------------------------------=== //
class Class final : public ASTNode {
  std::unique_ptr<Methods> _methods;
  std::unique_ptr<Identifier> _name;
  std::unique_ptr<Method> _constructor;
  std::unique_ptr<Identifier> _super;

public:
  Class(const Location &loc, std::unique_ptr<Methods> methods,
        std::unique_ptr<Identifier> name, std::unique_ptr<Method> constructor,
        std::unique_ptr<Identifier> super = nullptr)
      : ASTNode(loc), _methods(std::move(methods)), _name(std::move(name)),
        _constructor(std::move(constructor)), _super(std::move(super)) {
    UPDATE_ID(Class);
  }

  inline const Method &getConstructor() const { return *_constructor; }
  inline const Identifier &getClassIdent() const { return *_name; }
  inline const Identifier *getSuper() const {
    return _super.get(); /* could be null */
  }
  inline const Methods &getMethods() const { return *_methods; }
};

/// ===-------------------------------------------------------------------=== //
/// Classes - A list of Classes
/// ===-------------------------------------------------------------------=== //
class Classes final : public Sequence<Class>, public ASTNode {
public:
  explicit Classes(const Location &loc) : ASTNode(loc) { UPDATE_ID(Classes); }
};

/// ===-------------------------------------------------------------------=== //
/// Call - A method call
/// ===-------------------------------------------------------------------=== //
class Call final : public Expression {
  std::unique_ptr<LValue> _callee;
  std::unique_ptr<Arguments> _args;

public:
  Call(const Location &loc, std::unique_ptr<LValue> callee,
       std::unique_ptr<Arguments> args)
      : Expression(loc, Expression::Kind::Call), _callee(std::move(callee)),
        _args(std::move(args)) {
    UPDATE_ID(Call);
  }

  inline const LValue &getCallee() const { return *_callee; }
  inline const Arguments &getArgs() const { return *_args; }
};

/// ===-------------------------------------------------------------------=== //
/// If - conditional branch
/// ===-------------------------------------------------------------------=== //
class If final : public Statement {
  std::unique_ptr<Expression> _cond;
  std::unique_ptr<CompoundStmt> _ifStmts;
  std::unique_ptr<CompoundStmt> _elseStmts;

public:
  If(Location loc, std::unique_ptr<Expression> cond,
     std::unique_ptr<CompoundStmt> ifStmts,
     std::unique_ptr<CompoundStmt> elseStmts)
      : Statement(loc, Statement::Kind::If), _cond(std::move(cond)),
        _ifStmts(std::move(ifStmts)), _elseStmts(std::move(elseStmts)) {
    UPDATE_ID(If);
  }

  inline const Expression &getCond() const { return *_cond; }
  inline const CompoundStmt &getIfBlock() const { return *_ifStmts; }
  inline const CompoundStmt *getElseBlock() const { return _elseStmts.get(); }
  inline bool hasElse() const { return _elseStmts != nullptr; }
};

/// ===-------------------------------------------------------------------=== //
/// While - conditional loop
/// ===-------------------------------------------------------------------=== //
class While final : public Statement {
  std::unique_ptr<Expression> _cond;
  std::unique_ptr<CompoundStmt> _block;

public:
  While(Location loc, std::unique_ptr<Expression> cond,
        std::unique_ptr<CompoundStmt> stmts)
      : Statement(loc, Statement::Kind::While), _cond(std::move(cond)),
        _block(std::move(stmts)) {
    UPDATE_ID(While);
  }

  inline const Expression &getCond() const { return *_cond; }
  inline const CompoundStmt &getBlock() const { return *_block; }
};

/// ===-------------------------------------------------------------------=== //
/// TypeSwitchCase - a case in type_switch statement
/// ===-------------------------------------------------------------------=== //
class TypeSwitchCase final : public Statement {
  std::unique_ptr<VarDecl> _varDecl;
  std::unique_ptr<CompoundStmt> _block;

public:
  TypeSwitchCase(Location loc, std::unique_ptr<VarDecl> varDecl,
                 std::unique_ptr<CompoundStmt> stmts)
      : Statement(loc, Statement::Kind::TypeSwitchCase),
        _varDecl(std::move(varDecl)), _block(std::move(stmts)) {
    UPDATE_ID(TypeSwitchCase);
  }

  inline const VarDecl &getVarDecl() const { return *_varDecl; }
  inline const CompoundStmt &getBlock() const { return *_block; }
};

/// ===-------------------------------------------------------------------=== //
/// TypeAlternatives - sequence of type cases
/// ===-------------------------------------------------------------------=== //
class TypeAlternatives final : public Sequence<TypeSwitchCase>, public ASTNode {
public:
  explicit TypeAlternatives(const Location &loc) : ASTNode(loc) {
    UPDATE_ID(TypeAlternatives);
  }
};

/// ===-------------------------------------------------------------------=== //
/// TypeSwitch - a type_switch statement
/// ===-------------------------------------------------------------------=== //
class TypeSwitch final : public Statement {
  std::unique_ptr<LValue> _value;
  std::unique_ptr<TypeAlternatives> _cases;

public:
  TypeSwitch(Location loc, std::unique_ptr<LValue> value,
             std::unique_ptr<TypeAlternatives> alts)
      : Statement(loc, Statement::Kind::TypeSwitch), _value(std::move(value)),
        _cases(std::move(alts)) {
    UPDATE_ID(TypeSwitch);
  }

  inline const LValue &getValue() const { return *_value; }
  inline const TypeAlternatives &getCases() const { return *_cases; }
};

/// ===-------------------------------------------------------------------=== //
/// This functions casts ast node 'obj' to ast type 'CastT', if the node id
/// the node exactly matches the id of the type(so not exactly dynamic_cast).
/// ===-------------------------------------------------------------------=== //
template <typename CastT, typename NodeT>
const CastT *getAs(const NodeT &obj, NodeID id) {
  if (obj.getID() == id) {
    return static_cast<const CastT *>(&obj);
  }
  return nullptr;
}

/// ===-------------------------------------------------------------------=== //
/// Helper macro casts `NODE` to `TYPE`. `TYPE` must be passed without namespace
/// ===-------------------------------------------------------------------=== //
#define GET_NODE_AS(NODE, TYPE)                                                \
  getAs<quick::ast::TYPE>(NODE, quick::ast::NodeID::TYPE)

} // namespace quick::ast

#endif // QUICK_AST_AST_HPP

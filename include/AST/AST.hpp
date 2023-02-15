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
/// ASTNode - Abstract node class, the base for every other AST node
/// ===-------------------------------------------------------------------=== //
class ASTNode {
protected:
  enum class Kind : uint64_t {
    TranslationUnit = 1,
    Statement,
    Expression,
    LValue,
    Decl,
    Identifier,
    VarDecl,
    StaticMemberDecl,
    Arguments,
    Parameters,
    Class,
    Classes,
    Method,
    Methods,
    TypeAlternatives,
    CompoundStmt
  };

  ASTNode(Location loc, Kind kind) : _loc(loc), _kind(kind) {}

public:
  /// ===-----------------------------------------------------------------=== //
  /// RTTI is off, these are some dynamic and static type information to
  /// implement dynamic cast. When adding a new ASTNode, to conform with this
  /// infrastructure, add the node to the appropriate enum, then add
  /// `__INITIALIZE_NODE_TYPE_INFO` in the node class's definition
  /// ===-----------------------------------------------------------------=== //
  static constexpr uint64_t __id = 0;
  static constexpr uint64_t __sub_type_id_padding = 8;
  static constexpr uint64_t __remaining_bits = 24;
  static constexpr uint64_t __mask = __id << __remaining_bits;
  virtual uint64_t __get_type_dyn_mask() const = 0;

  /// this function acts as a dynamic cast for ASTNodes
  template <typename CastT> const CastT *as_a() const {
    if ((this->__get_type_dyn_mask() & CastT::__mask) == CastT::__mask) {
      return static_cast<const CastT *>(this);
    }
    return nullptr;
  }
  /// ===-----------------------------------------------------------------=== //

  inline const Location &getLocation() const { return _loc; }
  inline Kind getKind() const { return _kind; }
  virtual ~ASTNode() = default;

private:
  const Location _loc;
  Kind _kind;
};

/// Initializes dynamic type info inside an ASTNode class definition
#define __INITIALIZE_NODE_TYPE_INFO(NODE, PARENT)                              \
  static constexpr uint64_t __id = (uint64_t)PARENT::Kind::NODE;               \
  static constexpr uint64_t __remaining_bits =                                 \
      PARENT::__remaining_bits - __sub_type_id_padding;                        \
  static constexpr uint64_t __mask = __id << __remaining_bits;                 \
  uint64_t __get_type_dyn_mask() const override { return __mask; }

#define __INIT_KIND ((__id << __sub_type_id_padding) + 1)

class CompoundStmt; // Forward ref
class Classes;

/// ===-------------------------------------------------------------------=== //
/// TranslationUnit - Root node of the AST
/// ===-------------------------------------------------------------------=== //
class TranslationUnit final : public ASTNode {
  std::unique_ptr<Classes> _classes;
  std::unique_ptr<CompoundStmt> _compoundStmt;

public:
  __INITIALIZE_NODE_TYPE_INFO(TranslationUnit, ASTNode);
  TranslationUnit(Location loc, std::unique_ptr<Classes> classes,
                  std::unique_ptr<CompoundStmt> compoundStmt)
      : ASTNode(loc, (ASTNode::Kind)__id), _classes(std::move(classes)),
        _compoundStmt(std::move(compoundStmt)) {}

  inline const CompoundStmt &getCompoundStmt() const { return *_compoundStmt; }
  inline const Classes &getClasses() const { return *_classes; }
};

/// ===-------------------------------------------------------------------=== //
/// Statement - Abstract statement node
/// ===-------------------------------------------------------------------=== //
class Statement : public ASTNode {
public:
  __INITIALIZE_NODE_TYPE_INFO(Statement, ASTNode);
  enum class Kind : uint64_t {
    ValueStmt = __INIT_KIND,
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
  Statement(Location loc, Kind kind)
      : ASTNode(loc, (ASTNode::Kind)__id), _kind(kind) {}

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
  __INITIALIZE_NODE_TYPE_INFO(CompoundStmt, ASTNode);
  explicit CompoundStmt(const Location &loc)
      : ASTNode(loc, (ASTNode::Kind)__id) {}
  bool hasReturn() const;
};

/// ===-------------------------------------------------------------------=== //
/// Expression - Abstract expression node class
/// ===-------------------------------------------------------------------=== //
class Expression : public ASTNode {
public:
  __INITIALIZE_NODE_TYPE_INFO(Expression, ASTNode);
  enum class Kind {
    Call = __INIT_KIND,
    UnaryOperator,
    BinaryOperator,
    LValue,
    IntegerLiteral,
    FloatLiteral,
    BoolLiteral,
    NothingLiteral,
    StringLiteral
  };

  inline Kind getKind() const { return _kind; }

protected:
  Expression(Location loc, Kind kind)
      : ASTNode(loc, (ASTNode::Kind)__id), _kind(kind) {}

private:
  Kind _kind;
};

/// ===-------------------------------------------------------------------=== //
/// ValueStmt - A statement with an expression
/// ===-------------------------------------------------------------------=== //
class ValueStmt final : public Statement {
  std::unique_ptr<Expression> _expr;

public:
  __INITIALIZE_NODE_TYPE_INFO(ValueStmt, Statement);
  explicit ValueStmt(Location loc, std::unique_ptr<Expression> expr)
      : Statement(loc, Statement::Kind::ValueStmt), _expr(std::move(expr)) {}
  inline const Expression &getExpr() const { return *_expr; }
};

/// ===-------------------------------------------------------------------=== //
/// UnaryOperator - expression - operations with one operand
/// ===-------------------------------------------------------------------=== //
class UnaryOperator final : public Expression {
public:
  __INITIALIZE_NODE_TYPE_INFO(UnaryOperator, Expression);
  enum class Operator { Neg = __INIT_KIND, Not };

  UnaryOperator(Location loc, Operator opCode,
                std::unique_ptr<Expression> operand)
      : Expression(loc, Expression::Kind::UnaryOperator), _opCode(opCode),
        _operand(std::move(operand)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(BinaryOperator, Expression);
  enum class Operator : uint64_t {
    Plus = __INIT_KIND,
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
        _rightHandSide(std::move(rightHandSide)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(Identifier, ASTNode);
  Identifier(Location loc, const std::string &name)
      : ASTNode(loc, (ASTNode::Kind)__id), _name(name){};
  inline const std::string &getName() const { return _name; }
};

/// ===-------------------------------------------------------------------=== //
/// LValue - An expression with storage
/// ===-------------------------------------------------------------------=== //
class LValue : public Expression {
public:
  __INITIALIZE_NODE_TYPE_INFO(LValue, Expression);
  enum class Kind : uint64_t { Ident = __INIT_KIND, MemberAccess };

  inline Kind getKind() const { return _kind; }
  virtual const std::string &getVarName() const = 0;

protected:
  Kind _kind;

  LValue(Location loc, Kind kind)
      : Expression(loc, Expression::Kind::LValue), _kind(kind) {}
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
  __INITIALIZE_NODE_TYPE_INFO(MemberAccess, LValue);
  MemberAccess(const Location loc, std::unique_ptr<Identifier> member,
               std::unique_ptr<Expression> object)
      : LValue(loc, LValue::Kind::MemberAccess), _member(std::move(member)),
        _obj(std::move(object)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(Ident, LValue);
  IdentifierExpression(Location loc, std::unique_ptr<Identifier> var)
      : LValue(loc, LValue::Kind::Ident), _var(std::move(var)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(Decl, ASTNode);
  enum class Kind { Var = __INIT_KIND, Member };
  inline const Identifier &getType() const { return *_type; }
  inline bool isMemberDecl() const { return _kind == Kind::Member; }
  inline Kind getKind() const { return _kind; }

protected:
  std::unique_ptr<Identifier> _type;
  Kind _kind;
  Decl(const Location &loc, std::unique_ptr<Identifier> type, Kind kind)
      : ASTNode(loc, (ASTNode::Kind)__id), _type(std::move(type)), _kind(kind) {
  }
};

/// ===-------------------------------------------------------------------=== //
/// VarDecl - a variable declaration, <var>: <type>
/// ===-------------------------------------------------------------------=== //
class VarDecl final : public Decl {
  std::unique_ptr<IdentifierExpression> _var;

public:
  __INITIALIZE_NODE_TYPE_INFO(Var, Decl);
  VarDecl(const Location &loc, std::unique_ptr<IdentifierExpression> var,
          std::unique_ptr<Identifier> type)
      : Decl(loc, std::move(type), Kind::Var), _var(std::move(var)) {}

  inline const Identifier &getVar() const { return _var->getVar(); }
};

/// ===-------------------------------------------------------------------=== //
/// StaticMemberDecl - Declaration of a field member <expr>.<var>: <type>
/// ===-------------------------------------------------------------------=== //
class StaticMemberDecl final : public Decl {
  std::unique_ptr<MemberAccess> _object;

public:
  __INITIALIZE_NODE_TYPE_INFO(Member, Decl);
  StaticMemberDecl(Location loc, std::unique_ptr<MemberAccess> var,
                   std::unique_ptr<Identifier> type)
      : Decl(loc, std::move(type), Kind::Member), _object(std::move(var)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(Assignment, Statement);
  Assignment(Location loc, std::unique_ptr<LValue> lvalue,
             std::unique_ptr<Expression> rvalue)
      : Statement(loc, Statement::Kind::Assignment), _lvalue(std::move(lvalue)),
        _rvalue(std::move(rvalue)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(StaticAssignment, Statement);
  StaticAssignment(Location loc, std::unique_ptr<Decl> decl,
                   std::unique_ptr<Expression> rvalue)
      : Statement(loc, Statement::Kind::StaticAssignment),
        _decl(std::move(decl)), _rvalue(std::move(rvalue)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(IntegerLiteral, Expression);
  IntegerLiteral(Location loc, long int integer)
      : Expression(loc, Expression::Kind::IntegerLiteral), Literal<long>(
                                                               integer) {}
};

/// ===-------------------------------------------------------------------=== //
/// FloatLiteral
/// ===-------------------------------------------------------------------=== //
class FloatLiteral final : public Expression, public Literal<double> {
public:
  __INITIALIZE_NODE_TYPE_INFO(FloatLiteral, Expression);
  FloatLiteral(Location loc, double theFloat)
      : Expression(loc, Expression::Kind::FloatLiteral), Literal<double>(
                                                             theFloat) {}
};

/// ===-------------------------------------------------------------------=== //
/// BoolLiteral
/// ===-------------------------------------------------------------------=== //
class BoolLiteral final : public Expression, public Literal<bool> {
public:
  __INITIALIZE_NODE_TYPE_INFO(BoolLiteral, Expression);
  BoolLiteral(Location loc, bool boolean)
      : Expression(loc, Expression::Kind::BoolLiteral), Literal<bool>(boolean) {
  }
};

/// ===-------------------------------------------------------------------=== //
/// NothingLiteral -- None
/// ===-------------------------------------------------------------------=== //
class NothingLiteral final : public Expression {
public:
  __INITIALIZE_NODE_TYPE_INFO(NothingLiteral, Expression);
  explicit NothingLiteral(Location loc)
      : Expression(loc, Expression::Kind::NothingLiteral) {}
};

/// ===-------------------------------------------------------------------=== //
/// StringLiteral
/// ===-------------------------------------------------------------------=== //
class StringLiteral final : public Expression, public Literal<std::string> {
public:
  __INITIALIZE_NODE_TYPE_INFO(StringLiteral, Expression);
  StringLiteral(Location loc, std::string text)
      : Expression(loc, Expression::Kind::StringLiteral), Literal<std::string>(
                                                              std::move(text)) {
  }
};

/// ===-------------------------------------------------------------------=== //
/// Return
/// ===-------------------------------------------------------------------=== //
class Return final : public Statement {
  std::unique_ptr<Expression> _retVal;

public:
  __INITIALIZE_NODE_TYPE_INFO(Return, Statement);
  explicit Return(Location loc, std::unique_ptr<Expression> retVal)
      : Statement(loc, Statement::Kind::Return), _retVal(std::move(retVal)) {}

  inline const Expression *getRetVal() const {
    return _retVal != nullptr ? _retVal.get() : nullptr;
  }
};

/// ===-------------------------------------------------------------------=== //
/// Arguments - A list of expressions
/// ===-------------------------------------------------------------------=== //
class Arguments final : public Sequence<Expression>, public ASTNode {
public:
  __INITIALIZE_NODE_TYPE_INFO(Arguments, ASTNode);
  explicit Arguments(const Location &loc) : ASTNode(loc, (ASTNode::Kind)__id) {}
};

/// ===-------------------------------------------------------------------=== //
/// PrintStatement
/// ===-------------------------------------------------------------------=== //
class PrintStatement final : public Statement {
  std::unique_ptr<Arguments> exprList;

public:
  __INITIALIZE_NODE_TYPE_INFO(Print, Statement);
  PrintStatement(Location loc, std::unique_ptr<Arguments> exprList)
      : Statement(loc, Statement::Kind::Print), exprList(std::move(exprList)) {}

  inline auto &getArgs() const { return exprList; }
};

/// ===-------------------------------------------------------------------=== //
/// Parameters - a.k.a formals, a list of variable declarations
/// ===-------------------------------------------------------------------=== //
class Parameters final : public Sequence<VarDecl>, public ASTNode {
public:
  __INITIALIZE_NODE_TYPE_INFO(Parameters, ASTNode);
  explicit Parameters(const Location &loc)
      : ASTNode(loc, (ASTNode::Kind)__id) {}
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
  __INITIALIZE_NODE_TYPE_INFO(Method, ASTNode);
  Method(const Location &loc, std::unique_ptr<Parameters> params,
         std::unique_ptr<Identifier> name,
         std::unique_ptr<Identifier> returnType,
         std::unique_ptr<CompoundStmt> body)
      : ASTNode(loc, (ASTNode::Kind)__id), _params(std::move(params)),
        _name(std::move(name)), _returnType(std::move(returnType)),
        _body(std::move(body)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(Methods, ASTNode);
  explicit Methods(const Location &loc) : ASTNode(loc, (ASTNode::Kind)__id) {}
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
  __INITIALIZE_NODE_TYPE_INFO(Class, ASTNode);
  Class(const Location &loc, std::unique_ptr<Methods> methods,
        std::unique_ptr<Identifier> name, std::unique_ptr<Method> constructor,
        std::unique_ptr<Identifier> super = nullptr)
      : ASTNode(loc, (ASTNode::Kind)__id), _methods(std::move(methods)),
        _name(std::move(name)), _constructor(std::move(constructor)),
        _super(std::move(super)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(Classes, ASTNode);
  explicit Classes(const Location &loc) : ASTNode(loc, (ASTNode::Kind)__id) {}
};

/// ===-------------------------------------------------------------------=== //
/// Call - A method call
/// ===-------------------------------------------------------------------=== //
class Call final : public Expression {
  std::unique_ptr<LValue> _callee;
  std::unique_ptr<Arguments> _args;

public:
  __INITIALIZE_NODE_TYPE_INFO(Call, Expression);
  Call(const Location &loc, std::unique_ptr<LValue> callee,
       std::unique_ptr<Arguments> args)
      : Expression(loc, Expression::Kind::Call), _callee(std::move(callee)),
        _args(std::move(args)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(If, Statement);
  If(Location loc, std::unique_ptr<Expression> cond,
     std::unique_ptr<CompoundStmt> ifStmts,
     std::unique_ptr<CompoundStmt> elseStmts)
      : Statement(loc, Statement::Kind::If), _cond(std::move(cond)),
        _ifStmts(std::move(ifStmts)), _elseStmts(std::move(elseStmts)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(While, Statement);
  While(Location loc, std::unique_ptr<Expression> cond,
        std::unique_ptr<CompoundStmt> stmts)
      : Statement(loc, Statement::Kind::While), _cond(std::move(cond)),
        _block(std::move(stmts)) {}

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
  __INITIALIZE_NODE_TYPE_INFO(TypeSwitchCase, Statement);
  TypeSwitchCase(Location loc, std::unique_ptr<VarDecl> varDecl,
                 std::unique_ptr<CompoundStmt> stmts)
      : Statement(loc, Statement::Kind::TypeSwitchCase),
        _varDecl(std::move(varDecl)), _block(std::move(stmts)) {}

  inline const VarDecl &getVarDecl() const { return *_varDecl; }
  inline const CompoundStmt &getBlock() const { return *_block; }
};

/// ===-------------------------------------------------------------------=== //
/// TypeAlternatives - sequence of type cases
/// ===-------------------------------------------------------------------=== //
class TypeAlternatives final : public Sequence<TypeSwitchCase>, public ASTNode {
public:
  __INITIALIZE_NODE_TYPE_INFO(TypeAlternatives, ASTNode);
  explicit TypeAlternatives(const Location &loc)
      : ASTNode(loc, (ASTNode::Kind)__id) {}
};

/// ===-------------------------------------------------------------------=== //
/// TypeSwitch - a type_switch statement
/// ===-------------------------------------------------------------------=== //
class TypeSwitch final : public Statement {
  std::unique_ptr<LValue> _value;
  std::unique_ptr<TypeAlternatives> _cases;

public:
  __INITIALIZE_NODE_TYPE_INFO(TypeSwitch, Statement);
  TypeSwitch(Location loc, std::unique_ptr<LValue> value,
             std::unique_ptr<TypeAlternatives> alts)
      : Statement(loc, Statement::Kind::TypeSwitch), _value(std::move(value)),
        _cases(std::move(alts)) {}

  inline const LValue &getValue() const { return *_value; }
  inline const TypeAlternatives &getCases() const { return *_cases; }
};

} // namespace quick::ast

#endif // QUICK_AST_AST_HPP

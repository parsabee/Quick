//===--- IRType.cpp ---------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "CodeGen/IRType.hpp"
#include "llvm/IR/IRBuilder.h"

namespace quick {
namespace codegen {
namespace type {

using namespace llvm;
using namespace sema::type;

static inline llvm::Type *ptr(llvm::Type *t) {
  return llvm::PointerType::get(t, 0);
}

void updateMethodTable(
    IRType *type, MethodTable &methodTable,
    const std::vector<std::pair<std::string, FunctionType *>> &newMethods,
    llvm::StructType *vtable, llvm::StructType *superVtable) {

  auto numCurMethods = methodTable.size() + 1;
  for (auto &p : newMethods) {
    if (methodTable.count(p.first)) {
      // update fn ptr
      methodTable[p.first].second = {p.second, type};
    } else {
      methodTable.insert({p.first, {numCurMethods++, {p.second, type}}});
    }
  }

  std::vector<Type *> ordered_vtable(numCurMethods);
  ordered_vtable[0] = ptr(superVtable);
  for (auto &p : methodTable) {
    ordered_vtable[std::get<0>(p.second)] = ptr(std::get<1>(p.second).first);
  }
  vtable->setBody(ordered_vtable);
}

static std::pair<std::unique_ptr<ObjectType>, StructType *>
buildObjectType(LLVMTypeRegistry &tr, llvm::Module &module,
                llvm::StructType *type, llvm::Type *strType) {
  auto &cntx = module.getContext();
  auto *vtable = llvm::StructType::create(cntx, "Object_vtable");

  std::vector<llvm::Type *> members;
  members.push_back(ptr(vtable));
  type->setBody(members);

  auto eqFn = llvm::FunctionType::get(llvm::Type::getInt8Ty(cntx),
                                      {ptr(type), ptr(type)}, false);
  auto neFn = llvm::FunctionType::get(llvm::Type::getInt8Ty(cntx),
                                      {ptr(type), ptr(type)}, false);
  auto strFn = llvm::FunctionType::get(ptr(strType), {ptr(type)}, false);
  auto delFn =
      llvm::FunctionType::get(llvm::Type::getVoidTy(cntx), {ptr(type)}, false);
  //  vtable->setBody({ptr(vtable), ptr(eqFn), ptr(strFn), ptr(delFn)});

  FunctionType *constructorTy = FunctionType::get(ptr(type), {}, false);
  auto *constructor = Function::Create(
      constructorTy, GlobalValue::ExternalLinkage, "Object_create", module);
  constructor->setCallingConv(CallingConv::C);

  auto objT = std::make_unique<ObjectType>(tr, module, type, nullptr,
                                           MethodTable{}, vtable);
  auto &methodTable = objT->getMethodTable();
  IRType *irType = objT.get();
  int cntr = 1;
  methodTable.insert({"__eq__", {cntr++, {eqFn, objT.get()}}});
  methodTable.insert({"__ne__", {cntr++, {neFn, objT.get()}}});
  methodTable.insert({"__str__", {cntr++, {strFn, objT.get()}}});
  methodTable.insert({"__del__", {cntr++, {delFn, objT.get()}}});
  updateMethodTable(irType, methodTable, {}, vtable, vtable);
  return {std::move(objT), vtable};
}

static std::unique_ptr<NothingType>
buildNothingType(LLVMTypeRegistry &tr, llvm::Module &module,
                 llvm::StructType *type, llvm::StructType *objType,
                 llvm::Type *strType, ObjectType *super,
                 llvm::StructType *objectVtable) {
  auto &cntx = module.getContext();
  auto *vtable = llvm::StructType::create(cntx, "Nothing_vtable");
  std::vector<llvm::Type *> bodyTypes;
  bodyTypes.push_back(ptr(vtable));
  for (unsigned i = 1, end = objType->getNumElements(); i < end; i++) {
    bodyTypes.push_back(objType->getElementType(i));
  }
  type->setBody(bodyTypes);

  auto strFn = llvm::FunctionType::get(ptr(strType), {ptr(type)}, false);
  auto delFn =
      llvm::FunctionType::get(llvm::Type::getVoidTy(cntx), {ptr(type)}, false);
  auto eqFn = llvm::FunctionType::get(llvm::Type::getInt8Ty(cntx),
                                      {ptr(type), ptr(objType)}, false);
  auto neFn = eqFn;

  auto nothingT = std::make_unique<NothingType>(
      tr, module, type, super, super->getMethodTable(), vtable);
  std::vector<std::pair<std::string, FunctionType *>> newMethods = {
      {"__str__", strFn},
      {"__del__", delFn},
      {"__eq__", eqFn},
      {"__ne__", neFn}};

  auto &methodTable = nothingT->getMethodTable();
  updateMethodTable(nothingT.get(), methodTable, newMethods, vtable,
                    objectVtable);
  FunctionType *constructorTy = FunctionType::get(ptr(type), {}, false);

  auto *constructor = Function::Create(
      constructorTy, GlobalValue::ExternalLinkage, "Nothing_create", module);
  constructor->setCallingConv(CallingConv::C);
  return nothingT;
}

static std::unique_ptr<StringType>
buildStringType(LLVMTypeRegistry &tr, llvm::Module &module,
                llvm::StructType *type, llvm::StructType *objType,
                ObjectType *super, llvm::StructType *objectVtable) {
  auto &cntx = module.getContext();
  auto *vtable = llvm::StructType::create(cntx, "String_vtable");
  std::vector<llvm::Type *> bodyTypes;
  bodyTypes.push_back(ptr(vtable));
  for (unsigned i = 1, end = objType->getNumElements(); i < end; i++) {
    bodyTypes.push_back(objType->getElementType(i));
  }
  bodyTypes.push_back(llvm::Type::getInt8PtrTy(cntx));
  type->setBody(bodyTypes);

  auto eqFn = llvm::FunctionType::get(llvm::Type::getInt8Ty(cntx),
                                      {ptr(type), ptr(objType)}, false);
  auto neFn = llvm::FunctionType::get(llvm::Type::getInt8Ty(cntx),
                                      {ptr(type), ptr(objType)}, false);
  auto strFn = llvm::FunctionType::get(ptr(type), {ptr(type)}, false);
  auto delFn =
      llvm::FunctionType::get(llvm::Type::getVoidTy(cntx), {ptr(type)}, false);
  auto addFn =
      llvm::FunctionType::get(ptr(type), {ptr(type), ptr(type)}, false);

  //  vtable->setBody({ptr(eqFn), ptr(strFn), ptr(delFn), ptr(addFn)});

  auto stringT = std::make_unique<StringType>(tr, module, type, super,
                                              super->getMethodTable(), vtable);
  auto &methodTable = stringT->getMethodTable();
  std::vector<std::pair<std::string, FunctionType *>> newMethods = {
      {"__eq__", eqFn},
      {"__ne__", neFn},
      {"__str__", strFn},
      {"__del__", delFn},
      {"__add__", addFn}};

  updateMethodTable(stringT.get(), methodTable, newMethods, vtable,
                    objectVtable);

  FunctionType *constructorTy =
      FunctionType::get(ptr(type), {llvm::Type::getInt8PtrTy(cntx)}, false);
  auto *constructor = Function::Create(
      constructorTy, GlobalValue::ExternalLinkage, "String_create", module);
  constructor->setCallingConv(CallingConv::C);
  return stringT;
}

LLVMTypeRegistry::LLVMTypeRegistry(llvm::Module &module) : module(module) {
  auto *objStruct = llvm::StructType::create(module.getContext(), "Object");
  auto *nothingStruct =
      llvm::StructType::create(module.getContext(), "Nothing");
  auto *strStruct = llvm::StructType::create(module.getContext(), "String");

  auto &&[objectType, objectVtable] =
      buildObjectType(*this, module, objStruct, strStruct);
  auto intType = std::make_unique<IntType>(*this, module.getContext());
  auto floatType = std::make_unique<FloatType>(*this, module.getContext());
  auto boolType = std::make_unique<BoolType>(*this, module.getContext());
  auto stringType = buildStringType(*this, module, strStruct, objStruct,
                                    objectType.get(), objectVtable);
  auto nothingType =
      buildNothingType(*this, module, nothingStruct, objStruct, strStruct,
                       objectType.get(), objectVtable);
  registerType(std::move(intType));
  registerType(std::move(floatType));
  registerType(std::move(boolType));
  registerType(std::move(objectType));
  registerType(std::move(nothingType));
  registerType(std::move(stringType));

  this->objectVTable = objectVtable;
}

IRType *LLVMTypeRegistry::get(QType *qtype) {
  return get(qtype->getName());
}

IRType *LLVMTypeRegistry::get(llvm::Type *type) {
  auto it = this->typemap.find(type);
  if (it == this->typemap.end())
    return nullptr;
  return it->second;
}

IRType *LLVMTypeRegistry::get(llvm::StringRef typeName) {
  auto it = stringmap.find(typeName);
  if (it == stringmap.end())
    return nullptr;
  return it->second.get();
}

void LLVMTypeRegistry::dump(llvm::raw_ostream &out) {
  out << "StringMap: \n";
  for (auto &p : stringmap) {
    out << "\t{" << p.first() << ", " << p.second->getName() << "}\n";
  }

  out << "TypeMap: \n";
  for (auto &p : typemap) {
    out << "\t{" << p.getFirst() << ", " << p.getSecond()->getName() << "}\n";
  }
}

/// Handles method calls of an integer object
llvm::Value *IntType::dispatch(llvm::IRBuilder<> &b, const char *method,
                               llvm::Value *self, llvm::ArrayRef<Value *> args,
                               llvm::Module *module) {
  assert(args.size() <= 1 && "binary/unary operator");
  auto method_is = [&](const char *other) {
    return std::strcmp(method, other) == 0;
  };
  if (args.empty()) {
    if (method_is(op::UnaryOperator[op::NEG])) {
      return b.CreateNeg(self);
    } else if (method_is(op::UnaryOperator[op::NOT])) {
      return b.CreateICmpEQ(self, b.getInt64(0));
    } else if (method_is("__str__")) {
      auto *strtype = tr.get("String");
      auto fn = module->getOrInsertFunction(
          "String_create_int",
          llvm::FunctionType::get(
              strtype->getType(),
              {llvm::Type::getInt64Ty(module->getContext())}, false));
      assert(fn);
      return b.CreateCall(fn, self);
    }
  }

  auto arg = args.back();
  if (method_is(op::ArithmeticOperator[op::ADD])) {
    return b.CreateAdd(self, arg);
  } else if (method_is(op::ArithmeticOperator[op::SUB])) {
    return b.CreateSub(self, arg);
  } else if (method_is(op::ArithmeticOperator[op::MUL])) {
    return b.CreateMul(self, arg);
  } else if (method_is(op::ArithmeticOperator[op::DIV])) {
    return b.CreateSDiv(self, arg);
  } else if (method_is(op::ComparisonOperator[op::LE])) {
    return b.CreateICmpSLE(self, arg);
  } else if (method_is(op::ComparisonOperator[op::LT])) {
    return b.CreateICmpSLT(self, arg);
  } else if (method_is(op::ComparisonOperator[op::GE])) {
    return b.CreateICmpSGE(self, arg);
  } else if (method_is(op::ComparisonOperator[op::GT])) {
    return b.CreateICmpSGT(self, arg);
  } else if (method_is(op::ComparisonOperator[op::NE])) {
    return b.CreateICmpNE(self, arg);
  } else if (method_is(op::ComparisonOperator[op::EQ])) {
    return b.CreateICmpEQ(self, arg);
  } else if (method_is(op::ArithmeticOperator[op::MOD])) {
    return b.CreateSRem(self, args.back());
  }
  assert(false && "we shouldn't get here");
  return nullptr;
}

/// Handles method calls of an float object
llvm::Value *FloatType::dispatch(llvm::IRBuilder<> &b, const char *method,
                                 llvm::Value *self,
                                 llvm::ArrayRef<Value *> args,
                                 llvm::Module *module) {
  assert(args.size() <= 1 && "binary/unary operator");
  auto method_is = [&](const char *other) {
    return std::strcmp(method, other) == 0;
  };
  if (args.empty()) {
    if (method_is(op::UnaryOperator[op::NEG])) {
      return b.CreateFNeg(self);
    } else if (method_is("__str__")) {
      auto *strtype = tr.get("String");
      auto fn = module->getOrInsertFunction(
          "String_create_float",
          llvm::FunctionType::get(
              strtype->getType(),
              {llvm::Type::getDoubleTy(module->getContext())}, false));
      assert(fn);
      return b.CreateCall(fn, self);
    }
  }

  auto arg = args.back();
  if (method_is(op::ArithmeticOperator[op::ADD])) {
    return b.CreateFAdd(self, arg);
  } else if (method_is(op::ArithmeticOperator[op::SUB])) {
    return b.CreateFSub(self, arg);
  } else if (method_is(op::ArithmeticOperator[op::MUL])) {
    return b.CreateFMul(self, arg);
  } else if (method_is(op::ArithmeticOperator[op::DIV])) {
    return b.CreateFDiv(self, arg);
  } else if (method_is(op::ComparisonOperator[op::LE])) {
    return b.CreateFCmpOLE(self, arg);
  } else if (method_is(op::ComparisonOperator[op::LT])) {
    return b.CreateFCmpOLT(self, arg);
  } else if (method_is(op::ComparisonOperator[op::GE])) {
    return b.CreateFCmpOGE(self, arg);
  } else if (method_is(op::ComparisonOperator[op::GT])) {
    return b.CreateFCmpOGT(self, arg);
  } else if (method_is(op::ComparisonOperator[op::NE])) {
    return b.CreateFCmpONE(self, arg);
  } else if (method_is(op::ComparisonOperator[op::EQ])) {
    return b.CreateFCmpOEQ(self, arg);
  }
  assert(false && "we shouldn't get here");
  return nullptr;
}

/// Handles method calls of an boolean object
llvm::Value *BoolType::dispatch(llvm::IRBuilder<> &b, const char *method,
                                llvm::Value *self, llvm::ArrayRef<Value *> args,
                                llvm::Module *module) {
  assert(args.size() <= 1 && "binary/unary operator");
  auto method_is = [&](const char *other) {
    return std::strcmp(method, other) == 0;
  };

  if (args.empty()) {
    if (method_is(op::UnaryOperator[op::NEG]))
      return b.CreateNeg(self);
    else if (method_is(op::UnaryOperator[op::NOT])) {
      return b.CreateICmpEQ(
          self, llvm::ConstantExpr::getIntegerValue(
                    llvm::IntegerType::get(cntx, 8), llvm::APInt(8, 0)));
    } else if (method_is("__str__")) {
      auto *strtype = tr.get("String");
      auto fn = module->getOrInsertFunction(
          "String_create_bool",
          llvm::FunctionType::get(strtype->getType(),
                                  {llvm::Type::getInt8Ty(module->getContext())},
                                  false));
      return b.CreateCall(fn, self);
    }
  }

  if (method_is(op::ComparisonOperator[op::NE])) {
    return b.CreateICmpNE(self, args.back());
  } else if (method_is(op::ComparisonOperator[op::EQ])) {
    return b.CreateICmpEQ(self, args.back());
  }
  return nullptr;
}

/// Handles method calls of a complex object
llvm::Value *ComplexType::dispatch(IRBuilder<> &b, const char *method,
                                   llvm::Value *self,
                                   llvm::ArrayRef<llvm::Value *> args,
                                   llvm::Module *) {
  // loading the vtable
  auto vtableAddr = b.CreateStructGEP(self, 0);
  auto loadedVtable = b.CreateLoad(vtableAddr);

  // loading the method
  assert(methodTable.count(method) &&
         "method must exist in method table -- bug");
  int idx = methodTable[method].first;
  auto fnAddr = b.CreateStructGEP(loadedVtable, idx);
  auto fn = b.CreateLoad(fnAddr);

  // calling the method
  SmallVector<Value *, 4> newArgs;
  auto fnType = cast<FunctionType>(fn->getType()->getPointerElementType());
  for (unsigned i = 0, end = args.size(); i < end; i++) {
    auto pType = fnType->getParamType(i + 1);
    Value *arg = args[i];
    if (arg->getType() != pType)
      arg = b.CreateBitCast(arg, pType);
    newArgs.push_back(arg);
  }

  auto selfCastTo = fnType->getParamType(0);
  auto selfCast = self;
  if (self->getType() != selfCastTo)
    selfCast = b.CreateBitCast(self, selfCastTo);
  newArgs.insert(newArgs.begin(), selfCast);
  return b.CreateCall(fn, newArgs);
}

llvm::Value *ComplexType::instantiate(IRBuilder<> &b, Value *lvalue,
                                      llvm::ArrayRef<llvm::Value *> args,
                                      llvm::Module *) {
  auto valCast = args[0];
  auto lvalType = lvalue->getType()->getPointerElementType();
  if (lvalType != valCast->getType())
    valCast = b.CreateBitCast(valCast, lvalType);
  auto *val = b.CreateStore(valCast, lvalue);
  val->setAlignment(MaybeAlign(8));
  return val;
}

llvm::Value *ComplexType::alloc(IRBuilder<> &b) {
  auto val = b.CreateAlloca(getType());
  val->setAlignment(MaybeAlign(8));
  return val;
}

std::unique_ptr<ComplexType>
ComplexType::create(LLVMTypeRegistry &tr, Module &module, llvm::StringRef name,
                    ComplexType *super, Table<llvm::Type *> members,
                    MethodTable methodTable, StructType *vtable) {

  // adding vtable as the first element
  std::vector<llvm::Type *> allMembers(members.size() + 1);
  allMembers[0] = llvm::PointerType::get(vtable, 0);

  for (auto &entry : members) {
    allMembers[entry.second.first] = entry.second.second;
  }

  auto type = llvm::StructType::create(module.getContext(), name);
  type->setBody(allMembers);

  return std::unique_ptr<ComplexType>(
      new ComplexType(tr, module, type, super, std::move(methodTable), name,
                      vtable, std::move(members)));
}

/// Creates a 'store' instruction
llvm::Value *Primitive::instantiate(IRBuilder<> &b, Value *lvalue,
                                    llvm::ArrayRef<llvm::Value *> args,
                                    llvm::Module *) {
  assert(args.size() == 1);
  auto val = b.CreateStore(args[0], lvalue);
  val->setAlignment(MaybeAlign(8));
  return val;
}

/// Creates an 'alloca' instruction for the primitive type 't'
llvm::Value *Primitive::alloc(IRBuilder<> &b) {
  auto val = b.CreateAlloca(t);
  // every value is 8 bytes
  val->setAlignment(MaybeAlign(8));
  return val;
}
} // namespace type
} // namespace codegen
} // namespace quick

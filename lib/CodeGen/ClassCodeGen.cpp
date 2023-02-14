//===--- ClassCodeGen.hpp ---------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "CodeGen/ClassCodeGen.hpp"
#include "Sema/QType.hpp"
#include "Utils/Utils.hpp"

namespace quick::codegen {

using namespace ast;
using namespace llvm;
using namespace sema::type;
using namespace codegen::type;

bool ClassCodeGen::visitClass(const Class &clss) {
  return visitMethods(clss.getMethods());
}

bool ClassCodeGen::visitMethods(const Methods &methods) {
  for (auto &m : methods) {
    if (!visitMethod(*m))
      return false;
  }
  return true;
}

bool ClassCodeGen::visitMethod(const Method &method) {
  LLVMEnv env;
  auto &scope = env.addNewScope();
  auto &retTypeName = method.getReturnType().getName();
  auto *retType = tr.get(retTypeName);
  std::vector<Type *> argTypes;
  auto *irType = tr.get(qType);
  argTypes.push_back(irType->getType());

  auto &name = method.getMethodIdent().getName();
  QMethod *qMethod;
  assert(qType->getMethods().count(name));
  qMethod = qType->getMethods()[name].second.get();

  auto methodName = qMethod->getType()->getName() + qMethod->getName();
  for (auto &p : method.getParams()) {
    auto &pName = p->getVar().getName();
    auto t = tr.get(p->getType().getName());
    assert(t);
    argTypes.push_back(t->getType());
  }
  auto fn = module.getFunction(methodName);
  assert(fn);
  auto *bb = llvm::BasicBlock::Create(module.getContext(), methodName, fn);
  builder.SetInsertPoint(bb);
  FnCodeGen::Args args;
  auto &params = method.getParams();
  for (size_t i = 0, end = params.size(); i < end; i++) {
    auto val = fn->getArg(i + 1);
    args.push_back({params[i]->getVar().getName(), val});
  }

  auto thisArg = fn->getArg(0);
  auto *storage = builder.CreateAlloca(thisArg->getType());
  builder.CreateStore(thisArg, storage);
  scope.insert({"this", storage});
  for (auto &arg : args) {
    auto *s = builder.CreateAlloca(arg.second->getType());
    builder.CreateStore(arg.second, s);
    scope.insert({arg.first, s});
  }

  FnCodeGen methodCodeGen(builder, module, method.getBody(), tr, env, qType,
                          method.getMethodIdent().getName(), retType, args);
  if (!methodCodeGen.visitCompoundStmt(method.getBody()))
    return false;

  if (!fn->getBasicBlockList().back().getTerminator()) {
    InsertPointGuard g(builder);
    builder.SetInsertPoint(&fn->getBasicBlockList().back(),
                           fn->getBasicBlockList().back().end());
    auto val = builder.CreateCall(module.getFunction("Nothing_create"));
    builder.CreateRet(val);
  }

  return true;
}

void ClassCodeGen::generateGetVtableFunction(Constant *globalVtable) {
  InsertPointGuard guard(builder);
  auto &cntx = module.getContext();
  auto getVtableFn = FunctionType::get(globalVtable->getType(), false);
  auto fnName = std::string(typeName) + "_get_vtable";
  auto fn = getOrCreateFnSym(fnName, module, getVtableFn, false);
  auto *newBB = BasicBlock::Create(cntx, fnName, fn);
  builder.SetInsertPoint(newBB, newBB->end());
  builder.CreateRet(globalVtable);
}

bool ClassCodeGen::generateCreateFunction(llvm::Function *createFn,
                                          llvm::Function *initFn,
                                          StructType *type, StructType *vtable,
                                          Constant *globalVtable,
                                          MethodTable &methodTable) {
  auto &cntx = module.getContext();
  auto *bb = llvm::BasicBlock::Create(cntx, typeName + "_create", createFn);
  builder.SetInsertPoint(bb);

  // Calculate size of type
  auto &dl = module.getDataLayout();
  auto size = dl.getTypeAllocSize(type);

  // Create malloc call
  auto mallocFn =
      getOrCreateFnSym("malloc", module, llvm::Type::getInt8PtrTy(cntx),
                       {llvm::Type::getInt64Ty(cntx)});
  auto memPtr = builder.CreateCall(
      mallocFn, {builder.getInt(APInt(64, size.getFixedSize()))});
  auto thisPtr = builder.CreateBitCast(memPtr, llvm::PointerType::get(type, 0));

  auto vtablePtr = builder.CreateStructGEP(thisPtr, 0);
  builder.CreateStore(globalVtable, vtablePtr);

  // Create init call
  std::vector<Value *> args;
  args.push_back(thisPtr);
  for (auto &arg : createFn->args()) {
    args.push_back(&arg);
  }
  builder.CreateCall(initFn, args);
  builder.CreateRet(thisPtr);
  return true;
}

bool ClassCodeGen::generateInitFunction(llvm::Function *initFn,
                                        llvm::Function *superInitFn) {
  auto &cntx = module.getContext();
  auto &constructor = theClass.getConstructor();

  auto *bb = llvm::BasicBlock::Create(cntx, typeName + "_init", initFn);
  builder.SetInsertPoint(bb);

  FnCodeGen::Args args;
  auto &params = constructor.getParams();
  for (size_t i = 0, end = params.size(); i < end; i++) {
    auto val = initFn->getArg(i + 1);
    args.push_back({params[i]->getVar().getName(), val});
  }

  auto irType = tr.get(qType);
  LLVMEnv env;
  auto &scope = env.addNewScope();
  auto thisArg = initFn->getArg(0);
  auto *storage = builder.CreateAlloca(thisArg->getType());
  builder.CreateStore(thisArg, storage);
  scope.insert({"this", storage});
  for (auto &arg : args) {
    auto *s = builder.CreateAlloca(arg.second->getType());
    builder.CreateStore(arg.second, s);
    scope.insert({arg.first, s});
  }
  FnCodeGen constructorCodeGen(builder, module, constructor.getBody(), tr, env,
                               qType, typeName + "_init", irType, args);

  for (auto &stmt : constructor.getBody()) {
    // checking if it's a super type initializer
    if (auto *valStmt = GET_NODE_AS(*stmt, ValueStmt)) {
      if (auto *call = GET_NODE_AS(valStmt->getExpr(), Call)) {
        auto s = theClass.getSuper();
        if (!s || s->getName() == "Object")
          continue;

        // initializing super members
        std::vector<llvm::Value *> arguments;
        arguments.push_back(
            builder.CreateBitCast(thisArg, superInitFn->getArg(0)->getType()));
        for (auto &arg : call->getArgs()) {
          auto val = constructorCodeGen.getExprCodeGen().visitExpression(*arg);
          assert(val);
          arguments.push_back(val);
        }
        builder.CreateCall(superInitFn, arguments);
        continue;
      }
    }

    // otherwise use FnCodeGenerator to generate code
    if (!constructorCodeGen.visitStatement(*stmt))
      return false;
  }
  env.popCurrentScope();
  builder.CreateRetVoid();
  return true;
}

MethodTable ClassCodeGen::generateVTable(IRType *irType, StructType *vtable) {
  auto numNewMethods = 0;
  for (auto &p : qType->getMethods()) {
    if (p.second.second->getKind() == QMethod::Kind::New)
      numNewMethods++;
  }
  std::vector<std::pair<std::string, FunctionType *>> newMethods(numNewMethods);

  MethodTable methodTable = super->getMethodTable();
  for (auto &p : qType->getMethods()) {
    std::string methodName = p.first();
    std::vector<llvm::Type *> formalTypes;
    formalTypes.push_back(irType->getType());
    for (auto &[type, var] : p.second.second->getFormals()) {
      auto fIRType = tr.get(type);
      assert(fIRType);
      formalTypes.push_back(fIRType->getType());
    }
    auto *mRetType = tr.get(p.second.second->getReturnType());
    assert(mRetType);
    auto fnType =
        llvm::FunctionType::get(mRetType->getType(), formalTypes, false);

    if (methodTable.count(methodName)) {
      // override
      auto &m = methodTable[methodName];
      m.second = {fnType, irType};
    } else {
      newMethods[p.second.first] = {std::move(methodName), fnType};
    }
  }
  updateMethodTable(irType, methodTable, newMethods, vtable, superVtable);
  return std::move(methodTable);
}

Table<Type *> ClassCodeGen::generateMemberTable() {
  Table<Type *> memberTable = super->getMembers();
  int cntr = memberTable.size();
  for (auto &member : qType->getMembers()) {
    auto memberType = tr.get(member.second.second);
    assert(memberType);
    memberTable[member.first()] = {member.second.first + cntr,
                                   memberType->getType()};
  }

  return memberTable;
}

ClassCodeGen::ClassCodeGen(const Class &theClass, llvm::Module &module,
                           llvm::IRBuilder<> &builder, LLVMTypeRegistry &tr)
    : theClass(theClass), module(module), builder(builder), tr(tr),
      tdb(QTypeDB::get()), typeName(theClass.getClassIdent().getName()) {
  qType = tdb.getType(typeName);
  assert(qType);
  if (theClass.getSuper()) {
    auto type = tr.get(theClass.getSuper()->getName());
    assert(type);
    super = static_cast<ComplexType *>(type);
    assert(super);
    superVtable = super->getVtable();
  } else {
    super = static_cast<ComplexType *>(tr.get("Object"));
    assert(super);
    superVtable = tr.getObjectVtable();
  }
}

Status ClassCodeGen::generate() {
  auto &constructor = theClass.getConstructor();

  std::string constructorName = typeName + "_create";
  auto vtable =
      llvm::StructType::create(module.getContext(), typeName + "_vtable");

  // Creating the type
  auto memberTable = generateMemberTable();

  auto ctype = ComplexType::create(tr, module, typeName, super,
                                   std::move(memberTable), {}, vtable);

  auto irType = ctype.get();
  tr.registerType(std::move(ctype));

  // Creating the vtable
  auto methodTable = generateVTable(irType, vtable);

  // Generate global vtable instance and set it as objects vtable
  auto vtableInstName = typeName + "Vtable";
  auto globalVtable = module.getOrInsertGlobal(vtableInstName, vtable, [&]() {
    std::vector<Constant *> funcs(methodTable.size() + 1);
    llvm::Constant *superVtableInst = module.getOrInsertGlobal(
        std::string(super->getName()) + "Vtable", super->getVtable());
    assert(superVtableInst);
    funcs[0] = superVtableInst;
    for (auto &entry : methodTable) {
      std::string fnName = entry.first();
      fnName.insert(0, entry.second.second.second->getName());
      auto fn = module.getOrInsertFunction(fnName, entry.second.second.first);
      if (auto fnGv = dyn_cast<GlobalValue>(fn.getCallee())) {
        funcs[entry.second.first] = fnGv;
      }
    }
    auto constantStruct = ConstantStruct::get(vtable, funcs);
    auto gv = new GlobalVariable(
        module, vtable, true, llvm::GlobalValue::LinkageTypes::InternalLinkage,
        constantStruct, vtableInstName);
    return gv;
  });
  irType->setMethodTable(std::move(methodTable));
  irType->setVTable(vtable);

  // Creating the create/init functions
  std::vector<llvm::Type *> params;
  for (auto &p : constructor.getParams()) {
    auto irT = tr.get(p->getType().getName());
    assert(irT);
    params.push_back(irT->getType());
  }
  auto createFnType = llvm::FunctionType::get(irType->getType(), params, false);
  auto createFn =
      llvm::Function::Create(createFnType, llvm::GlobalValue::ExternalLinkage,
                             typeName + "_create", module);
  params.insert(params.begin(), irType->getType());

  std::string superName;
  if (theClass.getSuper())
    superName = theClass.getSuper()->getName();
  else
    superName = "Object";
  auto superInitFn =
      getOrCreateFnSym(superName + "_init", module, irType->getType(), params);

  auto &cntx = builder.getContext();
  auto initFnType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(cntx), params, false);
  auto initFn =
      llvm::Function::Create(initFnType, llvm::GlobalValue::ExternalLinkage,
                             typeName + "_init", module);

  initFn->setDSOLocal(true);
  createFn->setDSOLocal(true);

  // Creating the init function
  if (!generateInitFunction(initFn, superInitFn)) {
    return Status::ERROR;
  }

  // Creating the create function
  generateCreateFunction(createFn, initFn, irType->getStructType(), vtable,
                         globalVtable, irType->getMethodTable());
  generateGetVtableFunction(globalVtable);

  return visitClass(theClass) ? Status::OK : Status::ERROR;
}

} // namespace quick::codegen
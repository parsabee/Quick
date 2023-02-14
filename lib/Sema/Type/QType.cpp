//===--- QType.cpp ----------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//

#include "Sema/QType.hpp"
#include "AST/ASTVisitor.hpp"

using namespace llvm;

namespace quick {
namespace sema {
namespace type {

using namespace json;

bool QMethod::operator==(const QMethod &other) {
  return *type == *other.type && name == other.name;
}

std::unique_ptr<JSONNode> QMethod::toJson() {
  auto m = make<DictionaryNode>();
  m->insert({"name", make<StringNode>(name)});

  std::string k;
  switch (kind) {
  case Kind::Constructor:
    k = "Constructor";
    break;
  case Kind::New:
    k = "New";
    break;
  case Kind::Override:
    k = "Override";
    break;
  }

  m->insert({"kind", make<StringNode>(k)});
  m->insert({"return_type", make<StringNode>(returnType->getName())});

  if (!formals.empty()) {
    auto l = make<ListNode>();
    for (auto &it : formals)
      l->push_back(make<StringNode>(it.type->getName()));

    m->insert({"args", std::move(l)});
  }

  return m;
}

std::unique_ptr<JSONNode> QType::toJson() {
  auto t = make<DictionaryNode>();
  t->insert({"name", make<StringNode>(name)});
  if (parent)
    t->insert({"parent", make<StringNode>(parent->name)});

  if (!members.empty()) {
    auto mmL = make<ListNode>();
    for (auto &m : members) {
      mmL->push_back(make<StringNode>(m.first()));
    }
    t->insert({"members", std::move(mmL)});
  }

  if (!methods.empty()) {
    auto mtdsL = make<ListNode>();
    for (auto &m : methods) {
      mtdsL->push_back(m.second.second->toJson());
    }
    t->insert({"methods", std::move(mtdsL)});
  }

  return t;
}

bool QType::insertMember(const QVarDecl &namedVar) {
  if (members.count(namedVar.name) != 0)
    return false;

  auto id = members.size() + 1;
  members.insert({namedVar.name, {id, namedVar.type}});
  return true;
}

bool QType::insertMethod(const std::string &methodName, QType *retType,
                         llvm::ArrayRef<QVarDecl> args) {
  llvm::SmallVector<QType *, 4> argTypes;
  for (auto &[type, n] : args) {
    argTypes.push_back(type);
  }
  //  auto mName = mangleName(methodName, argTypes);

  if (methods.count(methodName) != 0)
    return false;

  auto isConstructor = [&] { return name == methodName; };

  auto isOverride = [&]() -> Optional<std::pair<int, const QMethod *>> {
    QType *p = parent;
    while (p != nullptr) {
      auto it = p->methods.find(methodName);
      if (it != p->methods.end()) {
        int idx = it->second.first;
        return {std::pair(idx, it->second.second.get())};
      }
      p = p->parent;
    }
    return None;
  };

  QMethod::Kind kind = QMethod::Kind::New;
  int idx = numNewMethods;
  if (isConstructor()) {
    kind = QMethod::Kind::Constructor;
    constructor =
        std::make_unique<QMethod>(this, args, retType, methodName, kind);
  } else {
    if (auto opt = isOverride()) {
      auto method = opt.getValue().second;
      idx = opt.getValue().first;
      kind = QMethod::Kind::Override;
    } else {
      numNewMethods++;
    }
    //  methods.insert({mName, QMethod(this, args, retType, mName, kind)});
    methods[methodName] = {
        idx, std::make_unique<QMethod>(this, args, retType, methodName, kind)};
  }

  return true;
}

QType *QType::lowestCommonAncestor(const QType *other) const {
  auto *t = const_cast<QType *>(this);
  while (other != t && !other->isDescendentOf(t)) {
    if (!t)
      return nullptr;
    t = t->parent;
  }
  return t;
}

bool QType::isDescendentOf(const QType *other) const {
  auto *p = parent;
  while (p) {
    if (p == other)
      return true;

    p = p->parent;
  }
  return false;
}

bool QType::operator==(const QType &other) { return name == other.name; }

bool QType::operator!=(const QType &other) { return !(*this == other); }

std::string QType::mangleName(llvm::StringRef mname,
                              llvm::ArrayRef<QType *> argTypes) const {
  // todo
  std::string mangledName = mname;
  return mangledName;
}

QMethod *QType::lookUpMethod(llvm::StringRef mname,
                             llvm::ArrayRef<QType *> argTypes) {
  auto mangledName = mangleName(mname, argTypes);
  auto curType = this;
  QMethod *method = nullptr;
  while (curType) {
    if (curType->methods.count(mangledName) == 0)
      curType = curType->parent;
    else {
      auto it = curType->methods.find(mangledName);
      method = it->getValue().second.get();
      break;
    }
  }
  return method;
}

QType *QType::lookUpMember(llvm::StringRef mname) {
  QType *p = this;
  while (p) {
    if (p->members.count(mname)) {
      auto &t = *p->members.find(mname);
      return t.second.second;
    }

    p = p->parent;
  }
  return nullptr;
}

} // namespace type
} // namespace sema
} // namespace quick
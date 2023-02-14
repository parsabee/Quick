//===--- Runtime.cpp - Runtime functions ------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
#include "Runtime/Runtime.hpp"

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>

extern "C" {

/// ===-------------------------------------------------------------------=== //
/// Object Type
/// ===-------------------------------------------------------------------=== //
Object_vtable_t ObjectVtable = {nullptr, Object__eq__, Object__ne__,
                                Object__str__, Object__del__};

String_t *Object__str__(Object_t *self) {
  std::stringstream ss;
  ss << self;
  auto s = ss.str();
  return String_create(s.c_str());
}

bool Object__eq__(Object_t *self, Object_t *other) { return self == other; }

bool Object__ne__(Object_t *self, Object_t *other) { return self != other; }

void Object__del__(Object_t *self) { std::free(self); }

Object_t *Object_create() {
  Object_t *obj = (Object_t *)std::malloc(sizeof(Object_t));
  obj->vtable = &ObjectVtable;
  return obj;
}

Object_vtable_t *Object_get_vtable() { return &ObjectVtable; }

void Object_init(Object_t *) {}

/// ===-------------------------------------------------------------------=== //
/// Nothing Type
/// ===-------------------------------------------------------------------=== //

static Nothing_vtable_t NothingVtable = {&ObjectVtable, Nothing__eq__,
                                         Nothing__ne__, Nothing__str__,
                                         Nothing__del__};

Nothing_t None = {&NothingVtable};

String_t *Nothing__str__(Nothing_t *) { return String_create("None"); }

void Nothing__del__(Nothing_t *) {}

bool Nothing__ne__(Nothing_t *, Object_t *b) { return (void *)b != &None; }

bool Nothing__eq__(Nothing_t *, Object_t *b) { return (void *)b == &None; }

Nothing_t *Nothing_create() { return &None; }

Nothing_vtable_t *Nothing_get_vtable() { return &NothingVtable; }

/// ===-------------------------------------------------------------------=== //
/// String Type
/// ===-------------------------------------------------------------------=== //

String_vtable_t StringVtable = {&ObjectVtable, String__eq__,  String__ne__,
                                String__str__, String__del__, String__add__};

String_t *String__str__(String_t *self) { return self; }

bool String__eq__(String_t *self, Object_t *o) {
  if (o->vtable != (Object_vtable_t *)&StringVtable)
    return false;

  auto *other = (String_t *)o;
  auto *s1 = (std::string *)self->__data;
  auto *s2 = (std::string *)other->__data;
  return *s1 == *s2;
}

bool String__ne__(String_t *self, Object_t *o) {
  if (o->vtable != (Object_vtable_t *)&StringVtable)
    return false;

  auto *other = (String_t *)o;
  auto *s1 = (std::string *)self->__data;
  auto *s2 = (std::string *)other->__data;
  return *s1 != *s2;
}

String_t *String__add__(String_t *self, String_t *other) {
  auto *s1 = (std::string *)self->__data;
  auto *s2 = (std::string *)other->__data;
  std::string tmp;
  tmp.reserve(s1->size() + s2->size());
  tmp += *s1;
  tmp += *s2;
  return String_create(tmp.c_str());
}

void String__del__(String_t *self) {
  auto *str = (std::string *)self->__data;
  delete str;
  std::free(self);
}

const char *String_getData(String_t *str) {
  auto *s = (std::string *)str->__data;
  return s->c_str();
}

String_t *String_create(const char *str) {
  String_t *obj = (String_t *)std::malloc(sizeof(String_t));
  obj->vtable = &StringVtable;
  obj->__data = (void *)new std::string(str);
  return obj;
}

String_t *String_create_int(long the_int) {
  auto str = std::to_string(the_int);
  return String_create(str.c_str());
}

String_t *String_create_float(double the_float) {
  auto str = std::to_string(the_float);
  return String_create(str.c_str());
}

String_t *String_create_bool(bool the_bool) {
  return the_bool ? String_create("True") : String_create("False");
}

String_vtable_t *String_get_vtable() { return &StringVtable; }

/// ===-------------------------------------------------------------------=== //
/// Traversing the v table of a's type hierarchy until we find a match
/// if we can't find one, it's not a subtype
/// ===-------------------------------------------------------------------=== //
bool is_subtype(Object_vtable_t *a, Object_vtable_t *b) {
  if (a == b)
    return true;
  if (a->superVtable == nullptr)
    return false;
  return is_subtype(a->superVtable, b);
}
}

#define STR_SYM(FN) #FN, (void *)&FN

void registerRuntimeFunctions(
    const std::function<void(const std::string &sym, void *addr)> &registerFn) {
  // object
  registerFn(STR_SYM(ObjectVtable));
  registerFn(STR_SYM(Object_get_vtable));
  registerFn(STR_SYM(Object_create));
  registerFn(STR_SYM(Object_init));
  registerFn(STR_SYM(Object__eq__));
  registerFn(STR_SYM(Object__ne__));
  registerFn(STR_SYM(Object__str__));
  registerFn(STR_SYM(Object__del__));

  // nothing
  registerFn(STR_SYM(NothingVtable));
  registerFn(STR_SYM(Nothing_get_vtable));
  registerFn(STR_SYM(Nothing_create));
  registerFn(STR_SYM(Nothing__eq__));
  registerFn(STR_SYM(Nothing__ne__));
  registerFn(STR_SYM(Nothing__str__));
  registerFn(STR_SYM(Nothing__del__));

  // string
  registerFn(STR_SYM(StringVtable));
  registerFn(STR_SYM(String_get_vtable));
  registerFn(STR_SYM(String_getData));
  registerFn(STR_SYM(String_create));
  registerFn(STR_SYM(String_create_int));
  registerFn(STR_SYM(String_create_float));
  registerFn(STR_SYM(String_create_bool));
  registerFn(STR_SYM(String__str__));
  registerFn(STR_SYM(String__eq__));
  registerFn(STR_SYM(String__ne__));
  registerFn(STR_SYM(String__del__));
  registerFn(STR_SYM(String__add__));

  // runtime helpers
  registerFn(STR_SYM(is_subtype));
}
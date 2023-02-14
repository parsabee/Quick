//===--- Runtime.hpp - Runtime functions ------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// This file declares the type, vtable and functions for Quick's builtin types
//
//===----------------------------------------------------------------------===//

#ifndef QUICK_RUNTIME_RUNTIME_HPP
#define QUICK_RUNTIME_RUNTIME_HPP

#include <cstdint>
#include <functional>

extern "C" {

// forward ref
typedef struct Q_String String_t;
typedef struct Q_Object Object_t;
typedef struct Q_Nothing Nothing_t;

/// ===-------------------------------------------------------------------=== //
/// Object Type -- every other class inherits its vtable and members
/// ===-------------------------------------------------------------------=== //
typedef struct Q_Object_vtable Object_vtable_t;
struct Q_Object_vtable {
  Object_vtable_t *superVtable;
  bool (*__eq__Object)(Object_t *, Object_t *);
  bool (*__ne__Object)(Object_t *, Object_t *);
  String_t *(*__str__)(Object_t *);
  void (*__del__)(Object_t *);
};

struct Q_Object {
  Object_vtable_t *vtable;
};

bool Object__eq__(Object_t *self, Object_t *other);
bool Object__ne__(Object_t *self, Object_t *other);
String_t *Object__str__(Object_t *);
void Object__del__(Object_t *);

Object_t *Object_create();
Object_vtable_t *Object_get_vtable();

/// ===-------------------------------------------------------------------=== //
/// Nothing Type
/// ===-------------------------------------------------------------------=== //
typedef struct Q_Nothing_vtable {
  Object_vtable_t *superVtable;
  bool (*__eq__Object)(Nothing_t *, Object_t *); // override
  bool (*__ne__Object)(Nothing_t *, Object_t *); // override
  String_t *(*__str__)(Nothing_t *);             // override
  void (*__del__)(Nothing_t *);                  // override
} Nothing_vtable_t;

struct Q_Nothing {
  Nothing_vtable_t *vtable;
};

String_t *Nothing__str__(Nothing_t *);
void Nothing__del__(Nothing_t *);
bool Nothing__eq__(Nothing_t *, Object_t *b);
bool Nothing__ne__(Nothing_t *, Object_t *b);

Nothing_t *Nothing_create();
Nothing_vtable_t *Nothing_get_vtable();

/// ===-------------------------------------------------------------------=== //
/// String Type
/// ===-------------------------------------------------------------------=== //
typedef struct Q_String_vtable {
  Object_vtable_t *superVtable;
  bool (*__eq__Object)(String_t *, Object_t *);       // override
  bool (*__ne__Object)(String_t *, Object_t *);       // override
  String_t *(*__str__)(String_t *);                   // override
  void (*__del__)(String_t *);                        // override
  String_t *(*__add__String)(String_t *, String_t *); // new method
} String_vtable_t;

struct Q_String {
  String_vtable_t *vtable;
  void *__data;
};

String_t *String__str__(String_t *);
bool String__eq__(String_t *self, Object_t *o);
bool String__ne__(String_t *self, Object_t *o);
void String__del__(String_t *);
String_t *String__add__(String_t *self, String_t *other);

String_t *String_create(const char *str);
String_t *String_create_int(long the_int);
String_t *String_create_float(double the_float);
String_t *String_create_bool(bool the_bool);

String_vtable_t *String_get_vtable();

/// ===-------------------------------------------------------------------=== //
/// runtime function for checking if type a is a subtype of type b
/// ===-------------------------------------------------------------------=== //
bool is_subtype(Object_vtable_t *a, Object_vtable_t *b);
}

/// ===-------------------------------------------------------------------=== //
/// a function for registering the addresses of runtime functions
/// to be linked in, calls registerFn for every runtime function
/// ===-------------------------------------------------------------------=== //
void registerRuntimeFunctions(
    const std::function<void(const std::string &sym, void *addr)> &registerFn);

#endif // QUICK_RUNTIME_RUNTIME_HPP

//===--- Status.hpp ---------------------------------------------*- C++ -*-===//
//
// Under MIT License, see: <project root>/LICENSE.txt
// Copyright (c) 2023 Parsa Bagheri
//
//===----------------------------------------------------------------------===//
//
// defines the status infrastructure
//
//===----------------------------------------------------------------------===//
#ifndef QUICK_UTILS_STATUS_HPP
#define QUICK_UTILS_STATUS_HPP

#include <assert.h>
#include <cstdlib>
#include <utility>

namespace quick {

//===----------------------------------------------------------------------===//
// Status - different systems may return these stati
//===----------------------------------------------------------------------===//
enum class Status {
  OK = 0,
  ERROR,
  PARSE_ERROR,
  TYPECHECK_ERROR,
  CODEGEN_ERROR,
  INIT_ERROR,
  INVALID_SOURCE,
  VALUE_RELEASED,
};

inline bool ok(Status status) { return status == Status::OK; }

//===----------------------------------------------------------------------===//
// StatusOr - different systems may return StatusOr<Type>, constructed from
// an object or status. Type `T` must have a default constructor
//===----------------------------------------------------------------------===//
template <typename T> class StatusOr {
  Status _status = Status::OK;
  T _value;

public:
  StatusOr(Status status) : _status(status) {
    assert(_status != Status::OK && "must not initialize with OK");
  }
  StatusOr(const StatusOr &) = delete;
  StatusOr(StatusOr &&other)
      : _status(other._status), _value(std::move(other._value)) {}
  StatusOr(T &&val) : _value(std::move(val)) {}
  Status status() { return _status; }
  bool ok() { return _status == Status::OK; }
  // releases the value
  T ValueOrDie() {
    if (!ok()) {
      assert(false && "bad status");
      std::abort();
    }

    _status = Status::VALUE_RELEASED;
    return std::move(_value);
  }
};

/// Exists if status is not ok, otherwise returns value
#define EXIT_ON_ERROR(STATUS_OR)                                               \
  [&]() {                                                                      \
    auto tmp = (std::move(STATUS_OR));                                         \
    if (!tmp.ok())                                                             \
      std::exit(static_cast<int>(tmp.status()));                               \
    return tmp.ValueOrDie();                                                   \
  }()

} // namespace quick

#endif // QUICK_UTILS_STATUS_HPP
#ifndef clox_native_h
#define clox_native_h

#include "value.h"

Value printNative(int argCount, Value *args);
Value clockNative(int argCount, Value *args);
Value lengthNative(int argCount, Value *args);

#endif
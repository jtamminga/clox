#include <stdio.h>
#include <time.h>
#include <string.h>

#include "native.h"
#include "object.h"

Value printNative(int argCount, Value* args) {
    printValue(args[0]);
    printf("\n");
    return NIL_VAL;
}

Value lengthNative(int argCount, Value* args) {
    if (IS_STRING(args[0])) {
        return NUMBER_VAL(strlen(AS_CSTRING(args[0])));
    }
}

Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}
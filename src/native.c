#include <stdio.h>
#include <time.h>

#include "native.h"

Value printNative(int argCount, Value* args) {
    printValue(args[0]);
    printf("\n");
    return NIL_VAL;
}

Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}
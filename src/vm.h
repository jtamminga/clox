#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    // points into the VM's value stack at the first
    // slot that this function can use
    Value* slots;
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX];

    // pointer to next element in array
    Value* stackTop;
    // global variables
    Table globals;
    // intern strings
    Table strings;
    ObjUpvalue* openUpvalues;

    // store the 
    Obj* objects;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif
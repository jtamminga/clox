#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

// instructions
// note: this has to fit in uint8_t, so max 255 op codes
typedef enum {
    OP_CONSTANT,
    OP_NIL,
    OP_DUP,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_DEFINE_GLOBAL,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
    OP_OUT_PROPERTY,
    OP_GET_SUPER,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_NOT_EQUAL,
    OP_GREATER_EQUAL,
    OP_LESS_EQUAL,
    OP_ADD,     
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_INC,
    OP_DEC,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_INVOKE,
    OP_SUPER_INVOKE,
    OP_CLOSURE,
    OP_CLOSE_UPVALUE,
    OP_RETURN,
    OP_CLASS,
    OP_INHERIT,
    OP_METHOD,
    OP_ARRAY,
    OP_GET_ARRAY,
    OP_SET_ARRAY
} OpCode;

// bytecode
typedef struct {
    int count; // entries in use
    int capacity; // max count
    uint8_t* code; // array of codes
    int *lines; // map bytecode to corresponding line in src
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif
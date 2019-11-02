#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

// instructions
typedef enum {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_DEFINE_GLOBAL,
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
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_RETURN,
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
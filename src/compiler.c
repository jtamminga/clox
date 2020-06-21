#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"
#include "memory.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

// precedence levels for expressions
// first item is the lowest, last is the highest
typedef enum {                  
  PREC_NONE,               
  PREC_POSTFIX,     // ++ --     
  PREC_ASSIGNMENT,  // =        
  PREC_OR,          // or       
  PREC_AND,         // and      
  PREC_EQUALITY,    // == !=    
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -      
  PREC_FACTOR,      // * /      
  PREC_PREFIX,      // ! - ++ --
  PREC_CALL,        // . () []  
  PREC_PRIMARY                  
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth; // 0 is global scope
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    Token name;
    bool hasSuperClass;
} ClassCompiler;

Parser parser;

ClassCompiler* currentClass = NULL;

Compiler* current = NULL;

static Chunk* currentChunk() {
    return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

// returns true if the current token has the given type
static bool check(TokenType type) {
    return parser.current.type == type;
}

// if current token has the given type we consume and return true
// otherwise we leave the token alone and return false
static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    // +2 to take into account OP_LOOP's 2 operands
    // which we also need to jump over
    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    // use two bytes, which allows us to have a jump offset
    // up to 65,536 bytes of code
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }

    emitByte(OP_RETURN);
}

// returns the index where it was appended
static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINTMAX_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }
    
    return (uint8_t)constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself.
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        // create a copy because the parser lexeme is a pointer to 
        // the source code, and that string might get freed once the
        // code finishes compiling. The function object outlives the
        // compiler and persists until runtime
        current->function->name = copyString(
            parser.previous.start, parser.previous.length);
    }

    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;

    #ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(),
            function->name != NULL ? function->name->chars : "<script>");
    }
    #endif

    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
            current->locals[current->localCount - 1].depth >
                current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        current->localCount--;
    }
}

static bool matchEquality() {
    return match(TOKEN_EQUAL)
        || match(TOKEN_PLUS_EQUAL)
        || match(TOKEN_MINUS_EQUAL)
        || match(TOKEN_STAR_EQUAL)
        || match(TOKEN_SLASH_EQUAL);
}

static void emitEquality(TokenType type) {
    switch (type) {
        case TOKEN_PLUS_EQUAL: emitByte(OP_ADD); break;
        case TOKEN_MINUS_EQUAL: emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR_EQUAL: emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH_EQUAL: emitByte(OP_DIVIDE); break;
        default:
            break; // unreachable
    }
}

// forward declarations
static void function();
static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

// returns the index where it was appended
static uint8_t identifierConstant(Token *name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Cannot read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    // reuse the upvalue index, if the upvalue already exists
    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function");
        return 0;
    }

    // if not found, we create a new one
    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) return -1;

    // base case:
    // look for matching variable in the enclosing function
    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    // recursion!
    // look beyond the enclosing function
    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    // other base case:
    // no enclosing function, variable can't be resolved lexically
    // and is treated as global
    return -1;
}

static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    // this marks variable as declared but not initialized
    local->depth = -1;
    local->isCaptured = false;
}

static void declareVariable() {
    // global variables are implicitly declared.
    // globals are late bound (at runtime), compiler doesn't keep
    // track of them.
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Variable with this name already declared in this scope.");
        }
    }
    

    addLocal(*name);
}

// returns index where variable was appended in globals table
static uint8_t parseVariable(const char *errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    // return dummy index, because local variables don't
    // have index in globals table
    if (current->scopeDepth > 0)
        return 0;

    return identifierConstant(&parser.previous);
}

static void markInitialized() {
    // when a top level function there is no local variable to mark
    // initialized - the function is bound to a global variable
    if (current->scopeDepth == 0)
        return;
    current->locals[current->localCount - 1].depth =
        current->scopeDepth;
}

// defining a variable is marking it ready for use.
// this is marked by setting the scope depth
// (remember that before init it is -1)
static void defineVariable(uint8_t global) {
    // we don't need to do anything if there is a local variable.
    // we just leave the value (vars initializer) on top of the stack
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();

            if (argCount == 255) {
                error("Cannot have more than 255 arguments");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void parameterList() {
    // compile the parameter list
    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255)
            {
                errorAtCurrent("Cannot have more than 255 parameters.");
            }

            uint8_t paramConst = parseVariable("Expect parameter name.");
            defineVariable(paramConst);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}

static void binary(bool canAssign) {
    // remember the operator
    TokenType operatorType = parser.previous.type;

    // compile the right operand
    ParseRule* rule = getRule(operatorType);
    // precedence + 1 allows for left-association
    // turns: 1 + 2 + 3 + 4 into: ((1 + 2) + 3) + 4
    parsePrecedence((Precedence)(rule->precedence + 1));

    // emit the operator instruction
    switch (operatorType) {
        case TOKEN_BANG_EQUAL:    emitByte(OP_NOT_EQUAL); break;  
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;           
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;         
        case TOKEN_GREATER_EQUAL: emitByte(OP_GREATER_EQUAL); break;   
        case TOKEN_LESS:          emitByte(OP_LESS); break;            
        case TOKEN_LESS_EQUAL:    emitByte(OP_LESS_EQUAL); break; 
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
        default:
            return; // unreachable
    }
}

static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expected property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && matchEquality()) {
        if (parser.previous.type == TOKEN_EQUAL) {
            expression();
        } else {
            TokenType type = parser.previous.type;
            emitBytes(OP_OUT_PROPERTY, name);
            expression();
            emitEquality(type);
        }

        emitBytes(OP_SET_PROPERTY, name);
    } else if (match(TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList();
        emitBytes(OP_INVOKE, name);
        emitByte(argCount);
    } else {
        if (match(TOKEN_INC)) {
            emitBytes(OP_INC_PROP, name);
            emitByte(OP_DEC);
        } else if (match(TOKEN_DEC)) {
            emitBytes(OP_DEC_PROP, name);
            emitByte(OP_INC);
        } else {
            emitBytes(OP_GET_PROPERTY, name);
        }
    }
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        default:
            return; // unreachable
    }
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

static void string(bool canAssign) {
    // +1 and -2 to trim the quotes
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
        parser.previous.length - 2)));
}

static void emitVariableOps(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign) {
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && matchEquality()) {    
        if (parser.previous.type == TOKEN_EQUAL) {
            expression();
        } else {
            TokenType type = parser.previous.type;
            emitBytes(getOp, (uint8_t)arg);
            expression();
            emitEquality(type);
        }
        
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);

    if (check(TOKEN_INC) || check(TOKEN_DEC)) {
        emitByte(OP_DUP);
        emitByte(parser.current.type == TOKEN_INC ? OP_INC : OP_DEC);
        emitVariableOps(parser.previous, true);
        emitByte(OP_POP);

        advance();
    }
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static void super_(bool canAssign) {
    if (currentClass == NULL) {
        error("Cannot use 'super' outside of a class.");
    } else if (!currentClass->hasSuperClass) {
        error("Cannot use 'super' in a class with no superclass.");
    }

    consume(TOKEN_DOT, "Expect '.' after 'super'.");
    consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
    uint8_t name = identifierConstant(&parser.previous);

    namedVariable(syntheticToken("this"), false);
    if (match(TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList();
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_SUPER_INVOKE, name);
        emitByte(argCount);
    } else {
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_GET_SUPER, name);
    }
}

static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Cannot use 'this' outside of a class.");
        return;
    }
    variable(false);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // compile the operand
    parsePrecedence(PREC_PREFIX);

    // emit the operator instruction
    switch (operatorType) {
        case TOKEN_BANG:
            emitByte(OP_NOT);
            break;
        case TOKEN_MINUS:
            emitByte(OP_NEGATE);
            break;
        default:
            return; // unreachable
    }
}

// left hand side already been compiled.
// at runtime the left value will be on top of the stack.
static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

// handle pre increment and decrement operator & variable
static void incdec(bool canAssign) {
    Token op = parser.previous;
    bool prop = false;
    uint8_t name;

    advance();
    emitVariableOps(parser.previous, false);
    while(match(TOKEN_DOT)) {
        prop = true;

        consume(TOKEN_IDENTIFIER, "Expected property name after '.'.");
        name = identifierConstant(&parser.previous);
    }

    if (prop) {
        emitBytes(op.type == TOKEN_INC ? OP_INC_PROP : OP_DEC_PROP, name);
    } else {
        emitByte(op.type == TOKEN_INC ? OP_INC : OP_DEC);
        emitVariableOps(parser.previous, true);
    }
}

static void lamda(bool assign) {
    Compiler compiler;
    initCompiler(&compiler, TYPE_FUNCTION);
    beginScope();

    // compile the parameter list
    parameterList();

    consume(TOKEN_ARROW, "Expect '=>' before lambda body.");
    expression();
    // consume(TOKEN_SEMICOLON, "Expect ';' after lambda body.");
    emitByte(OP_RETURN);

    // create the function object
    ObjFunction *function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++)
    {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

// columns:
//  prefix,   infix,   precedence
//
// these are in the same order as the tokens found in scanner.h
// Remember that enums are just numbers :)
ParseRule rules[] = {                                              
  { grouping, call,    PREC_CALL },       // TOKEN_LEFT_PAREN      
  { NULL,     NULL,    PREC_NONE },       // TOKEN_RIGHT_PAREN     
  { NULL,     NULL,    PREC_NONE },       // TOKEN_LEFT_BRACE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_RIGHT_BRACE     
  { NULL,     NULL,    PREC_NONE },       // TOKEN_COMMA           
  { NULL,     dot,     PREC_CALL },       // TOKEN_DOT             
  { unary,    binary,  PREC_TERM },       // TOKEN_MINUS           
  { NULL,     binary,  PREC_TERM },       // TOKEN_PLUS            
  { NULL,     NULL,    PREC_NONE },       // TOKEN_SEMICOLON       
  { NULL,     binary,  PREC_FACTOR },     // TOKEN_SLASH           
  { NULL,     binary,  PREC_FACTOR },     // TOKEN_STAR            
  { unary,    NULL,    PREC_NONE },       // TOKEN_BANG            
  { NULL,     binary,  PREC_EQUALITY },   // TOKEN_BANG_EQUAL      
  { NULL,     binary,  PREC_EQUALITY },   // TOKEN_EQUAL           
  { NULL,     binary,  PREC_COMPARISON }, // TOKEN_EQUAL_EQUAL     
  { NULL,     binary,  PREC_COMPARISON }, // TOKEN_GREATER         
  { NULL,     binary,  PREC_COMPARISON }, // TOKEN_GREATER_EQUAL   
  { NULL,     binary,  PREC_COMPARISON }, // TOKEN_LESS            
  { NULL,     binary,  PREC_COMPARISON }, // TOKEN_LESS_EQUAL
  { incdec,   NULL,    PREC_NONE },       // TOKEN_INC,
  { incdec,   NULL,    PREC_NONE },       // TOKEN_DEC,
  { NULL,     NULL,    PREC_NONE },       // TOKEN_PLUS_EQUAL
  { NULL,     NULL,    PREC_NONE },       // TOKEN_MINUS_EQUAL
  { NULL,     NULL,    PREC_NONE },       // TOKEN_STAR_EQUAL
  { NULL,     NULL,    PREC_NONE },       // TOKEN_SLASH_EQUAL
  { NULL,     NULL,    PREC_NONE },       // TOKEN_ARROW
  { variable, NULL,    PREC_NONE },       // TOKEN_IDENTIFIER      
  { string,   NULL,    PREC_NONE },       // TOKEN_STRING          
  { number,   NULL,    PREC_NONE },       // TOKEN_NUMBER          
  { NULL,     and_,    PREC_AND },        // TOKEN_AND             
  { NULL,     NULL,    PREC_NONE },       // TOKEN_CLASS           
  { NULL,     NULL,    PREC_NONE },       // TOKEN_ELSE            
  { literal,  NULL,    PREC_NONE },       // TOKEN_FALSE           
  { NULL,     NULL,    PREC_NONE },       // TOKEN_FOR             
  { lamda,    NULL,    PREC_NONE },       // TOKEN_FUN             
  { NULL,     NULL,    PREC_NONE },       // TOKEN_IF              
  { literal,  NULL,    PREC_NONE },       // TOKEN_NIL             
  { NULL,     or_,     PREC_OR },         // TOKEN_OR              
  { NULL,     NULL,    PREC_NONE },       // TOKEN_PRINT           
  { NULL,     NULL,    PREC_NONE },       // TOKEN_RETURN          
  { super_,   NULL,    PREC_NONE },       // TOKEN_SUPER           
  { this_,    NULL,    PREC_NONE },       // TOKEN_THIS            
  { literal,  NULL,    PREC_NONE },       // TOKEN_TRUE            
  { NULL,     NULL,    PREC_NONE },       // TOKEN_VAR             
  { NULL,     NULL,    PREC_NONE },       // TOKEN_WHILE           
  { NULL,     NULL,    PREC_NONE },       // TOKEN_ERROR           
  { NULL,     NULL,    PREC_NONE },       // TOKEN_EOF             
}; 

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
        expression();
    }
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

void expression() {
    // start with the lowest precedence level, assignment
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    // compile the parameter list
    parameterList();

    // the body
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    // create the function object
    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }

    // because we end compiler complete we we reach the function body,
    // there's no need to close the lingering outermost scope
}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 &&
            memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }

    function(type);
    emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.name = parser.previous;
    classCompiler.hasSuperClass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    // if there is a superclass
    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");
        namedVariable(parser.previous, false);

        if (identifiersEqual(&className, &parser.previous)) {
            error("A class cannot inherit from itself.");
        }

        beginScope();
        addLocal(syntheticToken("super"));
        defineVariable(0);

        namedVariable(className, false);
        emitByte(OP_INHERIT);
        classCompiler.hasSuperClass = true;
    }

    namedVariable(className, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP);

    if (classCompiler.hasSuperClass) {
        endScope();
    }

    currentClass = currentClass->enclosing;
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();        
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void forStatement() {
    // remember that the increment executes at the end of each
    // loop iteration

    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TOKEN_SEMICOLON)) {
        // no initializer
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;

    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // jump out of the loop if the condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(OP_JUMP);

        int incrementStart = currentChunk()->count;
        // usually an assignment, so we emit a pop to discard its value
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement();

    emitLoop(loopStart);

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP); // condition
    }

    endScope();
}

static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    // some explicit pops to make sure statements have a net
    // state effect of zero

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Cannot return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Cannot return a value from an initializer");
        }

        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

static void whileStatement() {
    int loopStart = currentChunk()->count;

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    statement();

    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;

        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_RETURN:
                return;

            default:
                // Do nothing.
                ;
        }

        advance();
    }
}

static void declaration() {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

static void statement() {
    if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();
    
    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
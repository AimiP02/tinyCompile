#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>

int token;            // current token
char *src, *old_src;  // pointer to source code string;
int poolsize;         // default size of text/data/stack
int line;             // line number

// Execution Segments
int *text,         // text segment
    *old_text,     // for dump text segment
    *stack;        // stack segment

char *data;        // data segment

// Registers
int *PC, *BP, *SP, AX, cycle;

// Instruction
enum {
    LEA, IMM, JMP, CALL, JZ, JNZ, ENT, ADJ, LEV, LI, LC, SI, SC,
    PUSH,
    OR, XOR, AND, EQ, NE,
    LT, GT, LE, GE,
    SHL, SHR,
    ADD, SUB, MUL, DIV, MOD,
    OPEN, READ, CLOS, PRTF, MALC, MSET, MCMP, EXIT
};


// Read file into one token
void next() {
    token = *src++;
    return;
}

void expression(int level) {
    // do nothing
}

void program() {
    next();                  // get next token
    while (token > 0) {
        printf("token is: %c\n", token);
        next();
    }
}

int eval() {
    int option;
    int *tmp;
    while(1) {
        option = *PC++;

        if (option == IMM)           { AX = *PC++; }                                    // mov ax, <num>
        else if (option == LC)       { AX = *(char *)AX; }                              // mov ax, (char *)reg
        else if (option == LI)       { AX = *(int *)AX; }                               // mov ax, (int *)reg
        else if (option == SC)       { *(char *)*SP++ = AX; }                           // mov sp, (char *)ax
        else if (option == SI)       { *(int *)*SP++ = AX; }                            // mov sp, (int *)ax
        else if (option == PUSH)     { *--SP = AX; }                                    // mov sp, ax; dec sp;
        else if (option == JMP)      { PC = (int *)*PC; }                               // jmp <addr>
        else if (option == JZ)       { PC = AX ? PC + 1 : (int *)*PC; }                 // jz <addr>
        else if (option == JNZ)      { PC = AX ? (int *)*PC : PC + 1; }                 // jnz <addr>
        else if (option == CALL)     { *--SP = (int)(PC + 1); PC = (int *)*PC; }        // call <addr>. it will storage the next address into the stack and jump to the target address.
        else if (option == ENT)      { *--SP = (int)BP; BP = SP; SP = SP - *PC++; }     // push bp; mov bp, sp; sub sp, <num>; it will make a new stackframe
        else if (option == ADJ)      { SP = SP + *PC++; }                               // add sp, <num>; it will erase stackframe
        else if (option == LEV)      { SP = BP; BP = (int *)*SP++; PC = (int *)*SP++;}  // mov sp, bp; pop bp; ret;
        else if (option == LEA)      { AX = (int)(BP + *PC++); }                        // it will fetch the parameter in the stack
        
        else if (option == OR)       { AX = *SP++ | AX; }
        else if (option == XOR)      { AX = *SP++ ^ AX; }
        else if (option == AND)      { AX = *SP++ & AX; }
        else if (option == EQ)       { AX = *SP++ == AX; }
        else if (option == NE)       { AX = *SP++ != AX; }
        else if (option == LT)       { AX = *SP++ < AX; }
        else if (option == GT)       { AX = *SP++ > AX; }
        else if (option == LE)       { AX = *SP++ <= AX; }
        else if (option == GE)       { AX = *SP++ >= AX; }
        else if (option == SHL)      { AX = *SP++ << AX; }
        else if (option == SHR)      { AX = *SP++ >> AX; }
        else if (option == ADD)      { AX = *SP++ + AX; }
        else if (option == SUB)      { AX = *SP++ - AX; }
        else if (option == MUL)      { AX = *SP++ * AX; }
        else if (option == DIV)      { AX = *SP++ / AX; }
        else if (option == MOD)      { AX = *SP++ % AX; }

        else if (option == OPEN)     { AX = open((char *)SP[1], SP[0]); }
        else if (option == READ)     { AX = read(SP[2], (char *)SP[1], *SP); }
        else if (option == CLOS)     { AX = close(*SP); }
        else if (option == PRTF)     { tmp = SP + PC[1]; AX = printf((char *)tmp[-1], tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6]); }
        else if (option == MALC)     { AX = (int)malloc(*SP); }
        else if (option == MSET)     { AX = (int)memset((char *)SP[2], SP[1], *SP); }
        else if (option == MCMP)     { AX = (int)memcmp((char *)SP[2], (char *)SP[1], *SP); }
        else if (option == EXIT)     { printf("exit(%d)", *SP); return *SP; }
        else {
            printf("unknown instruction: %d\n", option);
            return -1;
        }
    }
    return 0;
}

int main(int argc, char **argv)
{
    int i, fd;

    argc--;
    argv++;

    poolsize = 256 * 1024; // arbitrary size
    line = 1;

    if ((fd = open(*argv, 0)) < 0) {
        printf("could not open(%s)\n", *argv);
        return -1;
    }

    if (!(src = old_src = malloc(poolsize))) {
        printf("could not malloc(%d) for source area\n", poolsize);
        return -1;
    }

    // read the source file
    if ((i = read(fd, src, poolsize-1)) <= 0) {
        printf("read() returned %d\n", i);
        return -1;
    }

    src[i] = 0; // add EOF character
    close(fd);

    // allocate memory for virtual machine
    if (!(text = old_text = malloc(poolsize))) {
        printf("could not malloc(%d) for text area\n", poolsize);
        return -1;
    }
    if (!(data = malloc(poolsize))) {
        printf("could not malloc(%d) for data area\n", poolsize);
        return -1;
    }
    if (!(stack = malloc(poolsize))) {
        printf("could not malloc(%d) for stack area\n", poolsize);
        return -1;
    }

    // initialize segments
    memset(text, 0, poolsize);
    memset(data, 0, poolsize);
    memset(stack, 0, poolsize);

    // initialize stack and register
    BP = SP = (int *)((int)stack + poolsize);
    AX = 0;

    i = 0;

    text[i++] = IMM;
    text[i++] = 10;
    text[i++] = PUSH;
    text[i++] = IMM;
    text[i++] = 20;
    text[i++] = ADD;
    text[i++] = PUSH;
    text[i++] = EXIT;

    PC = text;

    program();
    return eval();
}
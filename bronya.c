#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int token;           // current token
char *src, *old_src; // pointer to source code string;
int poolsize;        // default size of text/data/stack
int line;            // line number

// Execution Segments
int *text,     // text segment
    *old_text, // for dump text segment
    *stack;    // stack segment

char *data; // data segment

// Registers
int *PC, *BP, *SP, AX, cycle;

int token_val;   // value of current token
int *current_id, // current parse ID
    *symbols;    // symbol table

int *idmain;

int base_type;
int expr_type;

int index_of_BP;

// Instruction
enum {
    LEA,
    IMM,
    JMP,
    CALL,
    JZ,
    JNZ,
    ENT,
    ADJ,
    LEV,
    LI,
    LC,
    SI,
    SC,
    PUSH,
    OR,
    XOR,
    AND,
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
    SHL,
    SHR,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    OPEN,
    READ,
    CLOS,
    PRTF,
    MALC,
    MSET,
    MCMP,
    EXIT
};

// Tokens and Classed
enum {
    Num = 128,
    Fun,
    Sys,
    Glo,
    Loc,
    Id,
    Char,
    Else,
    Enum,
    If,
    Int,
    Return,
    Sizeof,
    While,
    Assign,
    Cond,
    Lor,
    Lan,
    Or,
    Xor,
    And,
    Equ,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Inc,
    Dec,
    Brak
};

// Identifier
enum { Token, Hash, Name, Type, Class, Value, Btype, BClass, BValue, IdSize };

enum { CHAR, INT, PTR };

// Read file into one token
void next() {
    char *last_pos;
    int hash;

    while ( token = *src ) {
        src++;
        // New line
        if ( token == '\n' ) { line++; }
        // define include pragma ... it does'n have ability to parse these words :(
        else if ( token == '#' ) {
            while ( *src != 0 && *src != '\n' ) {
                src++;
            }
        }
        // Parse identifier
        else if ( ( 'a' <= token && token <= 'z' ) || ( 'A' <= token && token <= 'Z' ) || ( token == '_' ) ) {
            last_pos = src - 1;
            hash     = token;
            while ( ( 'a' <= *src && *src <= 'z' ) || ( 'A' <= *src && *src <= 'Z' ) || ( *src == '_' ) ) {
                hash = hash * 147 + *src;
                src++;
            }
            // Find existing identifier: Linear search
            current_id = symbols;
            while ( current_id[Token] ) {
                if ( current_id[Hash] == hash
                     && !memcmp( (char *)current_id[Name], (char *)last_pos, src - last_pos ) ) {
                    // Found one, return
                    token = current_id[Token];
                    return;
                }
                current_id = current_id + IdSize;
            }
            // Store new ID
            current_id[Name] = (int)last_pos;
            current_id[Hash] = hash;
            token = current_id[Token] = Id;
            return;
        }
        // Parse number
        else if ( '0' <= token && token <= '9' ) {
            // Three kinds of number: dec(123), oct(173), hex(0x7B)
            // dec
            token_val = token - '0';
            if ( token_val > 0 ) {
                while ( '0' <= *src && *src <= '9' ) {
                    token_val = token_val * 10 + *src++ - '0';
                }
            }
            else {
                // oct and hex
                if ( *src == 'x' || *src == 'X' ) {
                    // hex
                    token = *++src;
                    while ( ( '0' <= token && token <= '9' ) || ( 'a' <= token && token <= 'f' )
                            || ( 'A' <= token && token <= 'Z' ) ) {
                        token_val = token_val * 16 + ( token & 15 ) + ( token >= 'A' ? 9 : 0 );
                        token     = *++src;
                    }
                }
                else {
                    // oct
                    while ( '0' <= *src && *src <= '7' ) {
                        token_val = token_val * 8 + *src++ - '0';
                    }
                }
            }

            token = Num;
            return;
        }
        // Parse string
        else if ( token == '"' || token == '\'' ) {
            last_pos = data;
            while ( *src != 0 && *src != token ) {
                token_val = *src++;
                if ( token_val == '\\' ) {
                    token_val = *src++;
                    if ( token_val == 'n' ) { token_val = '\n'; }
                }
                if ( token == '"' ) { *data++ = token_val; }
            }
            src++;

            if ( token == '"' ) { token_val = (int)last_pos; }
            else {
                token = Num;
            }

            return;
        }
        // Parse annotation
        else if ( token == '/' ) {
            if ( *src == '/' ) {
                while ( *src != 0 && *src != '\n' ) {
                    src++;
                }
            }
            else {
                // it isn't annotation, but an operation
                token = Div;
                return;
            }
        }
        // Parse operation
        else if ( token == '=' ) {
            // parse '==' and '='
            if ( *src == '=' ) {
                token = Equ;
                src++;
            }
            else {
                token = Assign;
            }
            return;
        }
        else if ( token == '+' ) {
            // parse '++' and '+'
            if ( *src == '+' ) {
                token = Inc;
                src++;
            }
            else {
                token = Add;
            }
            return;
        }
        else if ( token == '-' ) {
            // parse '--' and '-'
            if ( *src == '-' ) {
                token = Dec;
                src++;
            }
            else {
                token = Sub;
            }
            return;
        }
        else if ( token == '*' ) {
            token = Mul;
            return;
        }

        // Div has been handled

        else if ( token == '%' ) {
            token = Mod;
            return;
        }
        else if ( token == '&' ) {
            // parse '&&' and '&'
            if ( *src == '&' ) {
                token = Lan;
                src++;
            }
            else {
                token = And;
            }
            return;
        }
        else if ( token == '|' ) {
            // parse '||' and '|'
            if ( *src == '|' ) {
                token = Lor;
                src++;
            }
            else {
                token = Or;
            }
            return;
        }
        else if ( token == '!' ) {
            // parse '!=' and '!'
            if ( *src == '=' ) {
                token = Neq;
                src++;
            }
            return;
        }
        else if ( token == '^' ) {
            token = Xor;
            return;
        }
        else if ( token == '<' ) {
            // parse '<=' '<<' '<'
            if ( *src == '=' ) {
                token = Le;
                src++;
            }
            else if ( *src == '<' ) {
                token = Shl;
                src++;
            }
            else {
                token = Lt;
            }
            return;
        }
        else if ( token == '>' ) {
            // parse '>=' '>>' '>'
            if ( *src == '=' ) {
                token = Ge;
                src++;
            }
            else if ( *src == '>' ) {
                token = Shr;
                src++;
            }
            else {
                token = Gt;
            }
            return;
        }
        else if ( token == '?' ) {
            token = Cond;
            return;
        }
        else if ( token == '[' ) {
            token = Brak;
            return;
        }
        else if ( token == '~' || token == ';' || token == '{' || token == '}' || token == '(' || token == ')'
                  || token == ']' || token == ',' || token == ':' ) {
            return;
        }
    }
    return;
}

void match( int new_token ) {
    if ( token == new_token ) { next(); }
    else {
        printf( "%d: expected token: %d\n", line, new_token );
        exit( -1 );
    }
}

void expression( int level ) {
    int *id;    // Identifier
    int params; // Function parameters
    int type;   // Type
    int *addr;  // Address

    {
        if ( !token ) {
            printf( "%d: unexpected token EOF of expression\n", line );
            exit( -1 );
        }
        // If token is num
        // Like  int a = 1 ...
        if ( token == Num ) {
            match( Num );

            *++text   = IMM;
            *++text   = token_val;
            expr_type = INT;
        }
        // If token is string
        // Like  char *flag;
        // flag = "1234"; or flag = "1234"
        //                          "5678";
        // The second one is equal to flag = "12345678";
        else if ( token == '"' ) {
            *++text = IMM;
            *++text = token_val;

            match( '"' );

            while ( token == '"' ) {
                match( '"' );
            }

            data      = (char *)( ( (int)data + sizeof( int ) ) & ( -sizeof( int ) ) );
            expr_type = PTR;
        }
        // If token is sizeof
        // Bronya Compiler supports for:
        // sizeof(int) sizeof(char) sizeof(*...)
        else if ( token == Sizeof ) {
            match( Sizeof );
            match( '(' );
            expr_type = INT;

            if ( token == Int ) { match( Int ); }
            else if ( token == Char ) {
                match( Char );
                expr_type = CHAR;
            }

            while ( token == Mul ) {
                match( Mul );
                expr_type = expr_type + PTR;
            }

            match( ')' );

            *++text = IMM;
            *++text = ( expr_type == CHAR ) ? sizeof( char ) : sizeof( int );

            // sizeof(...) will return INT type
            expr_type = INT;
        }
        // If token is variable or function
        // Bronya Compiler supports three type Id:
        // 1. Global/Local variable
        // 2. Function
        // 3. Enum
        else if ( token == Id ) {
            match( Id );

            id = current_id;

            // Function
            if ( token == '(' ) {
                match( '(' );

                params = 0;
                while ( token != ')' ) {
                    expression( Assign );
                    *++text = PUSH;
                    params++;

                    if ( token == ',' ) { match( ',' ); }
                }

                match( ')' );

                // System functions
                if ( id[Class] == Sys ) { *++text = id[Value]; }
                else if ( id[Class] == Fun ) {
                    *++text = CALL;
                    *++text = id[Value];
                }
                else {
                    printf( "%d: bad function call\n", line );
                    exit( -1 );
                }

                // Erase the stackframe
                if ( params > 0 ) {
                    *++text = ADJ;
                    *++text = params;
                }
            }
            // Enum
            else if ( id[Class] == Num ) {
                *++text   = IMM;
                *++text   = id[Value];
                expr_type = INT;
            }
            // Global/Local variable
            else {
                if ( id[Class] == Loc ) {
                    *++text = LEA;
                    *++text = index_of_BP - id[Value];
                }
                else if ( id[Class] == Glo ) {
                    *++text = IMM;
                    *++text = id[Value];
                }
                else {
                    printf( "%d: undefined variable\n", line );
                    exit( -1 );
                }

                expr_type = id[Type];
                *++text   = ( expr_type == Char ) ? LC : LI;
            }
        }
        // Force type conversion
        else if ( token == '(' ) {
            match( '(' );

            if ( token == Int || token == Char ) {
                type = ( token == Char ) ? CHAR : INT;
                match( token );

                while ( token == Mul ) {
                    match( Mul );
                    type = type + PTR;
                }

                match( ')' );

                expression( Inc );

                expr_type = type;
            }
            else {
                expression( Assign );
                match( ')' );
            }
        }
        // Pointer Value
        else if ( token == Mul ) {
            match( Mul );
            expression( Inc );

            if ( expr_type >= PTR ) { expr_type = expr_type - PTR; }
            else {
                printf( "%d: bad dereference\n", line );
                exit( -1 );
            }

            *++text = ( expr_type == CHAR ) ? LC : LI;
        }
        // Get Address
        else if ( token == And ) {
            match( And );
            expression( Inc );

            if ( *text == LC || *text == LI ) { text--; }
            else {
                printf( "%d: bad address of\n", line );
                exit( -1 );
            }

            expr_type = expr_type + PTR;
        }
        // Logic inversion
        else if ( token == '!' ) {
            match( '!' );
            expression( Inc );

            *++text = PUSH;
            *++text = IMM;
            *++text = 0;
            *++text = EQ;

            expr_type = INT;
        }
        // Bitwise inversion
        else if ( token == '~' ) {
            match( '-' );
            expression( Inc );

            *++text = PUSH;
            *++text = IMM;
            *++text = -1;
            *++text = XOR;

            expr_type = INT;
        }
        // Here we judge whether the number is positive or negative, not the
        // addition or subtraction in the calculation.
        else if ( token == Add ) {
            match( Add );
            expression( Inc );

            expr_type = INT;
        }
        else if ( token == Sub ) {
            match( Sub );

            if ( token == Num ) {
                *++text = IMM;
                *++text = -token_val;
                match( Num );
            }
            else {
                *++text = IMM;
                *++text = -1;
                *++text = PUSH;
                expression( Inc );
                *++text = MUL;
            }

            expr_type = INT;
        }
        // Self-increasing and self-decreasing
        else if ( token == Inc || token == Dec ) {
            type = token;
            match( token );
            expression( Inc );

            if ( *text == LC ) {
                *text   = PUSH;
                *++text = LC;
            }
            else if ( *text == LI ) {
                *text   = PUSH;
                *++text = LI;
            }
            else {
                printf( "%d: bad lvalue of pre-increment\n", line );
                exit( -1 );
            }

            *++text = PUSH;
            *++text = IMM;

            // If object is pointer
            *++text = ( expr_type > PTR ) ? sizeof( int ) : sizeof( char );
            *++text = ( type == Inc ) ? ADD : SUB;
            *++text = ( expr_type == CHAR ) ? SC : SI;
        }
    }

    while ( token >= level ) {
        
    }
}

void enumDeclaration() {
    // Parse enum [id] { first = 1, second = 2, third = 3 , ...}
    int i;
    i = 0;
    while ( token != '}' ) {
        if ( token != Id ) {
            printf( "%d: bad enum identifier %d\n", line, token );
            exit( -1 );
        }
        next();
        if ( token == Assign ) {
            next();
            if ( token != Num ) {
                printf( "%d: bad enum initializer\n", line );
                exit( -1 );
            }
            i = token_val;
            next();
        }

        current_id[Class] = Num;
        current_id[Type]  = INT;
        current_id[Value] = i++;

        if ( token == ',' ) { next(); }
    }
}

void parameterDeclaration() {
    int type;
    int params;

    params = 0;

    while ( token != ')' ) {
        type = INT;
        // (int param_1, ...)
        if ( token == Int ) { match( Int ); }
        // (char param_1, ...)
        else if ( token == Char ) {
            type = CHAR;
            match( Char );
        }
        // (int or char **...*param_1, ...)
        while ( token == Mul ) {
            match( Mul );
            type = type + PTR;
        }

        // parameter name
        if ( token != Id ) {
            printf( "%d: bad parameter declaration\n", line );
            exit( -1 );
        }
        if ( current_id[Class] == Loc ) {
            printf( "%d: duplicate parameter declaration\n", line );
            exit( -1 );
        }
        match( Id );

        current_id[BClass] = current_id[Class];
        current_id[Class]  = Loc;

        current_id[Btype] = current_id[Type];
        current_id[Type]  = type;

        current_id[BValue] = current_id[Value];
        current_id[Value]  = params++;

        if ( token == ',' ) { match( ',' ); }
    }

    // Before call function, this compiler will push parameters forwardly.
    // Then accroding to defination of CALL, it will push the next instruction's address to
    // the stack.
    // Finally, callee's function will push the EBP to the stack.
    // So the distance is 'params + 1'
    index_of_BP = params + 1;
}

// statement() is responsible for parsing the content
void statement() {
    int *area_a, *area_b;

    if ( token == If ) {
        match( If );
        match( '(' );
        expression( Assign );
        match( ')' );

        *++text = JZ;
        area_b  = ++text;

        statement();

        if ( token == Else ) {
            match( Else );

            *area_b = (int)( text + 3 );
            *++text = JMP;
            area_b  = ++text;

            statement();
        }

        *area_b = (int)( text + 1 );
    }
    else if ( token == While ) {
        match( While );

        area_a = text + 1;

        match( '(' );
        expression( Assign );
        match( ')' );

        *++text = JZ;
        area_b  = ++text;

        statement();

        *++text = JMP;
        *++text = (int)area_a;
        *area_b = (int)( text + 1 );
    }
    else if ( token == Return ) {
        match( Return );

        if ( token != ';' ) { expression( Assign ); }

        match( ';' );

        *++text = LEV;
    }
    else if ( token == '{' ) {
        match( '{' );

        while ( token != '}' ) {
            statement();
        }

        match( '}' );
    }
    else if ( token == ';' ) {
        match( ';' );
    }
    else {
        expression( Assign );
        match( ';' );
    }
}

void bodyDeclaration() {
    // Bronya Compiler stipulates all variables must declare on the top of function.

    int pos_local;
    int type;

    pos_local = index_of_BP;

    while ( token == Int || token == Char ) {
        base_type = ( token == Int ) ? INT : CHAR;
        match( token );

        while ( token != ';' ) {
            type = base_type;
            while ( token == Mul ) {
                match( Mul );
                type = type + PTR;
            }

            if ( token != Id ) {
                // invalid declaration
                printf( "%d: bad global declaration\n", line );
                exit( -1 );
            }
            if ( current_id[Class] ) {
                // identifier exists
                printf( "%d: duplicate global declaration\n", line );
                exit( -1 );
            }

            match( Id );

            current_id[BClass] = current_id[Class];
            current_id[Class]  = Loc;

            current_id[Btype] = current_id[Type];
            current_id[Type]  = type;

            current_id[BValue] = current_id[Value];
            current_id[Value]  = ++pos_local;

            if ( token == ',' ) { match( ',' ); }
        }

        match( ';' );
    }

    *++text = ENT;
    *++text = pos_local - index_of_BP;

    while ( token != '}' ) {
        statement();
    }

    *++text = LEV;
}

void functionDeclaration() {
    // type func_name (param_1, param_2, ...) { ... }
    match( '(' );
    parameterDeclaration();
    match( ')' );
    match( '{' );
    bodyDeclaration();

    current_id = symbols;
    while ( current_id[Token] ) {
        if ( current_id[Class] == Loc ) {
            current_id[Class] = current_id[BClass];
            current_id[Type]  = current_id[Btype];
            current_id[Value] = current_id[BValue];
        }
        current_id = current_id + IdSize;
    }
}

void globalDeclaration() {
    // globalDeclaration is used to declare (EBNF):
    // enum     -> enumDeclaration ::= 'enum' [id] '{' id ['=' 'num'] {',' id ['=' 'num']} '}'
    // variable -> variableDeclaration ::= type {'*'} id {',' {'*'} id} ';'
    // func     -> funcDeclaration ::= type {'*'} id '(' parameterDeclaration ')' '{' bodyDeclaration '}'
    int type;
    int i;

    base_type = INT;

    // Parse enum
    if ( token == Enum ) {
        match( Enum );
        if ( token != '{' ) {
            // enum id {
            match( Id );
        }
        else if ( token == '{' ) {
            // enum {
            match( '{' );
            enumDeclaration();
            match( '}' );
        }
        match( ';' );
        return;
    }

    // Parse type information
    if ( token == Int ) { match( Int ); }
    else if ( token == Char ) {
        match( Char );
        base_type = CHAR;
    }

    // Parse the comma seperated variable declaration
    while ( token != ';' && token != '}' ) {
        type = base_type;
        while ( token == Mul ) {
            match( Mul );
            type = type + PTR;
        }

        if ( token != Id ) {
            // invalid declaration
            printf( "%d: bad global declaration\n", line );
            exit( -1 );
        }
        if ( current_id[Class] ) {
            // identifier exists
            printf( "%d: duplicate global declaration\n", line );
            exit( -1 );
        }
        match( Id );
        current_id[Type] = type;

        if ( token == '(' ) {
            current_id[Class] = Fun;
            current_id[Value] = (int)( text + 1 );
            functionDeclaration();
        }
        else {
            current_id[Class] = Glo;
            current_id[Value] = (int)data;
            data              = data + sizeof( int );
        }

        if ( token == ',' ) { match( ',' ); }
    }
    next();
}

void program() {
    next(); // get next token
    while ( token > 0 ) {
        globalDeclaration();
    }
}

int eval() {
    int option;
    int *tmp;

    while ( 1 ) {
        option = *PC++;

        if ( option == IMM ) { AX = *PC++; } // mov ax, <num>
        else if ( option == LC ) {
            AX = *(char *)AX;
        } // mov ax, (char *)reg
        else if ( option == LI ) {
            AX = *(int *)AX;
        } // mov ax, (int *)reg
        else if ( option == SC ) {
            *(char *)*SP++ = AX;
        } // mov sp, (char *)ax
        else if ( option == SI ) {
            *(int *)*SP++ = AX;
        } // mov sp, (int *)ax
        else if ( option == PUSH ) {
            *--SP = AX;
        } // mov sp, ax; dec sp;
        else if ( option == JMP ) {
            PC = (int *)*PC;
        } // jmp <addr>
        else if ( option == JZ ) {
            PC = AX ? PC + 1 : (int *)*PC;
        } // jz <addr>
        else if ( option == JNZ ) {
            PC = AX ? (int *)*PC : PC + 1;
        } // jnz <addr>
        else if ( option == CALL ) {
            *--SP = (int)( PC + 1 );
            PC    = (int *)*PC;
        } // call <addr>. it will storage the next address into the stack and jump to the target address.
        else if ( option == ENT ) {
            *--SP = (int)BP;
            BP    = SP;
            SP    = SP - *PC++;
        } // push bp; mov bp, sp; sub sp, <num>; it will make a new stackframe
        else if ( option == ADJ ) {
            SP = SP + *PC++;
        } // add sp, <num>; it will erase stackframe
        else if ( option == LEV ) {
            SP = BP;
            BP = (int *)*SP++;
            PC = (int *)*SP++;
        } // mov sp, bp; pop bp; ret;
        else if ( option == LEA ) {
            AX = (int)( BP + *PC++ );
        } // it will fetch the parameter in the stack

        // Operation
        else if ( option == OR ) {
            AX = *SP++ | AX;
        }
        else if ( option == XOR ) {
            AX = *SP++ ^ AX;
        }
        else if ( option == AND ) {
            AX = *SP++ & AX;
        }
        else if ( option == EQ ) {
            AX = *SP++ == AX;
        }
        else if ( option == NE ) {
            AX = *SP++ != AX;
        }
        else if ( option == LT ) {
            AX = *SP++ < AX;
        }
        else if ( option == GT ) {
            AX = *SP++ > AX;
        }
        else if ( option == LE ) {
            AX = *SP++ <= AX;
        }
        else if ( option == GE ) {
            AX = *SP++ >= AX;
        }
        else if ( option == SHL ) {
            AX = *SP++ << AX;
        }
        else if ( option == SHR ) {
            AX = *SP++ >> AX;
        }
        else if ( option == ADD ) {
            AX = *SP++ + AX;
        }
        else if ( option == SUB ) {
            AX = *SP++ - AX;
        }
        else if ( option == MUL ) {
            AX = *SP++ * AX;
        }
        else if ( option == DIV ) {
            AX = *SP++ / AX;
        }
        else if ( option == MOD ) {
            AX = *SP++ % AX;
        }

        // Syscall
        else if ( option == OPEN ) {
            AX = open( (char *)SP[1], SP[0] );
        }
        else if ( option == READ ) {
            AX = read( SP[2], (char *)SP[1], *SP );
        }
        else if ( option == CLOS ) {
            AX = close( *SP );
        }
        else if ( option == PRTF ) {
            tmp = SP + PC[1];
            AX  = printf( (char *)tmp[-1], tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6] );
        }
        else if ( option == MALC ) {
            AX = (int)malloc( *SP );
        }
        else if ( option == MSET ) {
            AX = (int)memset( (char *)SP[2], SP[1], *SP );
        }
        else if ( option == MCMP ) {
            AX = (int)memcmp( (char *)SP[2], (char *)SP[1], *SP );
        }
        else if ( option == EXIT ) {
            printf( "exit(%d)", *SP );
            return *SP;
        }
        else {
            printf( "unknown instruction: %d\n", option );
            return -1;
        }
    }
    return 0;
}

int main( int argc, char **argv ) {
    int i, fd;

    argc--;
    argv++;

    poolsize = 256 * 1024; // arbitrary size
    line     = 1;

    if ( ( fd = open( *argv, 0 ) ) < 0 ) {
        printf( "could not open(%s)\n", *argv );
        return -1;
    }

    // allocate memory for virtual machine
    if ( !( text = old_text = malloc( poolsize ) ) ) {
        printf( "could not malloc(%d) for text area\n", poolsize );
        return -1;
    }
    if ( !( data = malloc( poolsize ) ) ) {
        printf( "could not malloc(%d) for data area\n", poolsize );
        return -1;
    }
    if ( !( stack = malloc( poolsize ) ) ) {
        printf( "could not malloc(%d) for stack area\n", poolsize );
        return -1;
    }
    if ( !( symbols = malloc( poolsize ) ) ) {
        printf( "could not malloc(%d) for symbol area\n", poolsize );
        return -1;
    }

    // initialize segments
    memset( text, 0, poolsize );
    memset( data, 0, poolsize );
    memset( stack, 0, poolsize );
    memset( symbols, 0, poolsize );

    // initialize stack and register
    BP = SP = (int *)( (int)stack + poolsize );
    AX      = 0;

    src = "char else enum if int return sizeof while "
          "open read close printf malloc memset memcmp exit void main";

    i = Char;
    while ( i <= While ) {
        next();
        current_id[Token] = i++;
    }

    i = OPEN;
    while ( i <= EXIT ) {
        next();
        current_id[Class] = Sys;
        current_id[Type]  = INT;
        current_id[Value] = i++;
    }

    next();
    current_id[Token] = Char;
    next();
    idmain = current_id;

    if ( !( src = old_src = malloc( poolsize ) ) ) {
        printf( "could not malloc(%d) for source area\n", poolsize );
        return -1;
    }

    // read the source file
    if ( ( i = read( fd, src, poolsize - 1 ) ) <= 0 ) {
        printf( "read() returned %d\n", i );
        return -1;
    }

    src[i] = 0; // add EOF character
    close( fd );

    program();
    return eval();
}
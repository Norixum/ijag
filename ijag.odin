package main

import "core:os"
import "core:fmt"
import "core:strings"
import "core:unicode"
import "core:strconv"
import "core:slice"

Token_Id :: string
Token_Num :: int
Token_Op :: enum {
    PLUS,
    MINUS,
    MULTIPLY,
    DIVISON,
    PRIME,
}

List :: struct {
    start, end: int
}

Func_Call :: struct{
    name: string,
    args: [dynamic][dynamic]Instruction,
}

Token :: struct {
    row: int,
    column: int,
    kind: Token_Kind,
    as: struct #raw_union {
        id: Token_Id,
        num: Token_Num,
        op: Token_Op,
        type: Token_Builtin_Type,
    }
}

Token_Kind :: enum {
    ID,
    NUM,
    TYPE,
    OP,
    DDOT,
    LPAR,
    RPAR,
    LSQPAR,
    RSQPAR,
    COL,
    EQ,
    EOL,
    EOF,
    COMMA,
}

Token_Builtin_Type :: enum {
    INTEGER
}

token_builtin_type :: proc(row, column: int, type: Token_Builtin_Type) -> Token {
    return Token {
        row,
        column,
        .TYPE,
        { type = type },
    }
}

token_id :: proc(row, column: int, id: Token_Id) -> Token {
    return Token {
        row,
        column,
        .ID,
        { id = id },
    }
}

token_num :: proc(row, column: int, num: Token_Num) -> Token {
    return Token {
        row,
        column,
        .NUM,
        { num = num },
    }
}

token_op :: proc(row, column: int, op: Token_Op) -> Token {
    return Token {
        row,
        column,
        .OP,
        { op = op },
    }
}

token_new :: proc(row, column: int, type: Token_Kind) -> Token {
    return Token {
        row,
        column,
        type,
        {},
    }
}

AST :: struct {
    functions: map[string]Func_Defenition,
    func_calls: [dynamic]Func_Call,
}

Func_Defenition :: struct {
    body: [dynamic]Instruction,
    parameters: [dynamic]Func_Parameter, 
}

Func_Parameter :: struct {
    name: Token_Id,
    type: Token_Builtin_Type,
}

Push_Num :: int
Push_Op :: Token_Op
Push_Arg :: Token_Id

Instruction :: union {
    Push_Num,
    Push_Op,
    Push_Arg,
    Func_Call
}

Parser :: struct {
    tokens: []Token,
    current: int
}

parser_init :: proc(tokens: []Token) -> Parser {
    return Parser{tokens, -1}
}

parser_next :: proc(parser: ^Parser) -> Token {
    if parser.current < len(parser.tokens)-1 {
        parser.current += 1
    }
    return parser.tokens[parser.current]
}

parser_peek :: proc(parser: ^Parser) -> Token {
    if parser.current < len(parser.tokens)-1 {
        return parser.tokens[parser.current+1]
    }
    return parser.tokens[parser.current]
}

is_type :: proc(token: Token, type: Token_Kind, log := true) -> (result: Token, ok: bool) {
    if token.kind == .EOF {
        if log {
            fmt.printf("(%v:%v): Expected <%v> but found nothing", token.row, token.column, type)
        }
        return
    }
    if token.kind != type { 
        if log {
            fmt.printf("(%v:%v): Expected <%v> but found: %v", token.row, token.column, type, token.kind)
        }
        return
    }
    return token, true
}

is_id :: proc(token: Token) -> (result: Token_Id, ok: bool) {
    token := is_type(token, .ID) or_return
    return token.as.id, true
}

is_op :: proc(token: Token) -> (result: Token_Op, ok: bool) {
    token := is_type(token, .OP) or_return
    return token.as.op, true
}

is_num :: proc(token: Token) -> (result: Token_Num, ok: bool) {
    token := is_type(token, .NUM) or_return
    return token.as.num, true
}

is_exact_id :: proc(token: Token, expected: Token_Id) -> (ok: bool) {
    token := is_type(token, .ID) or_return
    return token.as.id == expected
}

is_exact_op :: proc(token: Token, expected: Token_Op) -> (ok: bool) {
    token := is_type(token, .OP) or_return
    return token.as.op == expected
}

is_exact_num :: proc(token: Token, expected: Token_Num) -> (ok: bool) {
    token := is_type(token, .NUM) or_return
    return token.as.num == expected
}

parser_save :: proc(parser: Parser) -> int {
    return parser.current
}

parser_recover :: proc(parser: ^Parser, pos: int) {
    parser.current = pos
}

parse_expr :: proc(parser: ^Parser, expr: ^[dynamic]Instruction, ast: ^AST, params: []Func_Parameter) -> (ok: bool) {
    next := parser_next(parser)
    #partial switch next.kind {
    case .ID:
        for param in params {
            if strings.compare(param.name, next.as.id) == 0 {
                append(expr, next.as.id)
                return true
            }
        }
        if next.as.id not_in ast.functions {
            fmt.printf("(%v:%v): Function does not exists: %#v", next.row, next.column, next.as.id)
            return
        }

        func_call: Func_Call
        func_call.name = next.as.id
        for i in 0..<len(ast.functions[next.as.id].parameters) {
            expr: [dynamic]Instruction
            parse_expr(parser, &expr, ast, params)
            append(&func_call.args, expr)
        }
        append(expr, func_call)
    case .NUM:
        append(expr, next.as.num)
    case .OP:
        switch next.as.op {
        case .PLUS: fallthrough
        case .MINUS: fallthrough
        case .MULTIPLY: fallthrough
        case .DIVISON:
            parse_expr(parser, expr, ast, params) or_return
            parse_expr(parser, expr, ast, params) or_return
            append(expr, next.as.op)
        case .PRIME:
            parse_expr(parser, expr, ast, params) or_return
            append(expr, next.as.op)
        }
    case .EOL, .EOF:
        return
    case:          
        unimplemented()
    }
    
    return true
}

Lexer :: struct {
    content: string,
    current: int,
    line:    int,
    column:  int,
    error:   bool,
}

lexer :: proc(content: string) -> Lexer {
    return Lexer {
        content = content,
        current = -1,
        line    = 1,
        column  = 1,
        error   = false,
    }
}

lexer_next_char :: proc(lexer: ^Lexer) -> (char: rune, ok: bool) {
    if lexer.current + 1 >= len(lexer.content) do return
    lexer.current += 1
    return cast(rune)lexer.content[lexer.current], true
}

lexer_peek_char :: proc(lexer: ^Lexer) -> (char: rune, ok: bool) {
    if lexer.current + 1 >= len(lexer.content) do return
    return cast(rune)lexer.content[lexer.current + 1], true
}

lexer_next :: proc(lexer: ^Lexer) -> (token: Token, ok: bool) {
    for char in lexer_peek_char(lexer) {
        if !unicode.is_white_space(char) do break
        _ = lexer_next_char(lexer) or_else unreachable()
        lexer.column += 1
        if char == '\n' { 
            token = token_new(lexer.line, lexer.column, .EOL)
            lexer.line += 1
            lexer.column = 1
            return token, true
        }
    }

    char := lexer_next_char(lexer) or_return
    if unicode.is_letter(char) {
        id_begin := lexer.current
        for char in lexer_peek_char(lexer) {
            if !unicode.is_letter(char) do break
            _ = lexer_next_char(lexer) or_else unreachable()
        }
        id := lexer.content[id_begin:lexer.current + 1]
        if strings.compare(id, "Integer") == 0 {
            return token_builtin_type(lexer.line, lexer.column, .INTEGER), true
        } 
        return token_id(lexer.line, lexer.column, id), true
    }
    if unicode.is_digit(char) {
        num_begin := lexer.current
        for char in lexer_peek_char(lexer) {
            if !unicode.is_digit(char) do break
            _ = lexer_next_char(lexer) or_else unreachable()
        }
        num_str := lexer.content[num_begin:lexer.current + 1]
        if num, ok := strconv.parse_int(num_str); !ok {
            fmt.printf("(%v:%v): Invalid number", lexer.line, lexer.column)
            lexer.error = true
            return
        } else {
            return token_num(lexer.line, lexer.column, num), true
        }
    }

    switch char {
    case '=':
        return token_new(lexer.line, lexer.column, .EQ), true
    case '(':
        return token_new(lexer.line, lexer.column, .LPAR), true
    case ')':
        return token_new(lexer.line, lexer.column, .RPAR), true
    case '[':
        return token_new(lexer.line, lexer.column, .LSQPAR), true
    case ']':
        return token_new(lexer.line, lexer.column, .RSQPAR), true
    case '.':
        char := lexer_next_char(lexer) or_return
        if char == '.' {
            return token_new(lexer.line, lexer.column, .DDOT), true
        }
    case ':':
        return token_new(lexer.line, lexer.column, .COL), true
    case '+':
        return token_op(lexer.line, lexer.column, .PLUS), true
    case '-':
        return token_op(lexer.line, lexer.column, .MINUS), true
    case '*':
        return token_op(lexer.line, lexer.column, .MULTIPLY), true
    case '/':
        return token_op(lexer.line, lexer.column, .DIVISON), true
    case '\'':
        return token_op(lexer.line, lexer.column, .PRIME), true
    case ',':
        return token_new(lexer.line, lexer.column, .COMMA), true
    case '#':
        for char in lexer_peek_char(lexer) {
            _ = lexer_next_char(lexer) or_else unreachable()
            lexer.column += 1
            if char == '\n' { 
                token = token_new(lexer.line, lexer.column, .EOL)
                lexer.line += 1
                lexer.column = 1
                return token, true
            }
        }
        return token_new(lexer.line, lexer.column, .EOF), true
    }

    fmt.printf("(%v:%v): Unknown symbol: %v", lexer.line, lexer.column, char)
    lexer.error = true
    return
}

lexer_collect :: proc(lexer: ^Lexer) -> (tokens: [dynamic]Token, ok: bool) {
    for token in lexer_next(lexer) {
        append(&tokens, token)
    }
    append(&tokens, token_new(lexer.line, lexer.column, .EOF))

    return tokens, !lexer.error
}

parser_expect :: proc(parser: ^Parser, expected: Token_Kind) -> (token: Token, ok: bool) {
    token = parser_next(parser)
    if token.kind != expected {
        return
    }
    return token, true    
}

try_parse_definition_parameterless :: proc(parser: ^Parser, ast: ^AST) -> (ok: bool) {
    save := parser_save(parser^)
    defer if !ok do parser_recover(parser, save)
    
    def : Func_Defenition
    id := parser_expect(parser, .ID) or_return
    
    if id.as.id in ast.functions do return false
    parser_expect(parser, .COL) or_return
    type := parser_expect(parser, .TYPE) or_return
    assert(type.as.type == .INTEGER)
    parser_expect(parser, .EQ) or_return
    parse_expr(parser, &def.body, ast, {}) or_return
    ast.functions[id.as.id] = def
    
    return true
}

try_parse_definition :: proc(parser: ^Parser, ast: ^AST) -> (ok: bool) {
    save := parser_save(parser^)
    defer if !ok do parser_recover(parser, save)
    
    def : Func_Defenition
    id := parser_expect(parser, .ID) or_return
    
    if id.as.id in ast.functions do return false
    parser_expect(parser, .LPAR) or_return
    params: for {
        id := parser_expect(parser, .ID) or_return
        for param in def.parameters {
            if strings.compare(id.as.id, param.name) == 0 {
                return
            }
        }
        parser_expect(parser, .COL) or_return
        type := parser_expect(parser, .TYPE) or_return
        append(&def.parameters, Func_Parameter{id.as.id, type.as.type})
        next := parser_next(parser) 
        #partial switch next.kind {
        case .RPAR:
            break params
        case .COMMA:
            continue params
        case:
            return
        }
    }
    
    parser_expect(parser, .COL) or_return
    type := parser_expect(parser, .TYPE) or_return
    assert(type.as.type == .INTEGER)
    parser_expect(parser, .EQ) or_return
    parse_expr(parser, &def.body, ast, def.parameters[:]) or_return
    ast.functions[id.as.id] = def
    return true
}

try_parse_func_call :: proc(parser: ^Parser, ast: ^AST) -> (ok: bool) {
    save := parser_save(parser^)
    defer if !ok do parser_recover(parser, save)
    
    func_call : Func_Call
    id := parser_expect(parser, .ID) or_return
    
    if id.as.id not_in ast.functions {
        fmt.printf("(%v:%v): Function does not exists: %#v", id.row, id.column, id.as.id)
        return
    }
    for param in ast.functions[id.as.id].parameters {
        expr: [dynamic]Instruction
        parse_expr(parser, &expr, ast, {}) or_return
        append(&func_call.args, expr)
    }
    func_call.name = id.as.id
    append(&ast.func_calls, func_call)
    
    return true
}

parse :: proc(tokens: []Token, ast: ^AST) -> (ok: bool) {
    parser := parser_init(tokens)
    
    for {
        next := parser_peek(&parser)
        if next.kind == .EOF {
            break
        }
        if next.kind == .EOL {
            parser_next(&parser)
            continue
        }

        if try_parse_definition_parameterless(&parser, ast) do continue
        if try_parse_definition(&parser, ast) do continue
        if try_parse_func_call(&parser, ast) do continue

        return
    }
    return true
}

generate_expr_asm :: proc(buffer: ^strings.Builder, expr: []Instruction, params: []Func_Parameter, ast: ^AST) -> int {
    nums_count := 0
    for inst in expr {
        switch inst in inst{
        case Push_Num:
            nums_count += 1
            fmt.sbprintf(buffer, "        push qword %v\n", inst)
        case Push_Op:
            nums_count -= 2
            assert(nums_count >= 0)
            
            fmt.sbprintf(buffer, "        pop rbx\n")
            fmt.sbprintf(buffer, "        pop rax\n")
            
            switch inst {
            case .PLUS:
                fmt.sbprintf(buffer, "        add rax, rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
                nums_count += 1
            case .MINUS: 
                fmt.sbprintf(buffer, "        sub rax, rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
                nums_count += 1
            case .MULTIPLY: 
                fmt.sbprintf(buffer, "        imul rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
                nums_count += 1
            case .DIVISON:
                fmt.sbprintf(buffer, "        idiv rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
                nums_count += 1

            case .PRIME: 
                unimplemented()
            case: 
                unreachable()
            }
        case Push_Arg:
            for param, i in params {
                if strings.compare(param.name, inst) == 0 {
                    fmt.sbprintf(buffer, "        push qword [rbp + %v]\n", (len(params) - i + 1)*8)
                    nums_count += 1
                    break
                }
            }
        case Func_Call:
            for arg in inst.args {
                nums_count += generate_expr_asm(buffer, arg[:], ast.functions[inst.name].parameters[:], ast)
            }
            assert(nums_count >= len(inst.args))
            if strings.compare(inst.name, "print") == 0 {
                fmt.sbprintf(buffer, "        mov rax, [rsp]\n")
                fmt.sbprintf(buffer, "        call print\n")
                fmt.sbprintf(buffer, "        add rsp, %v\n", 8 * len(inst.args))
                fmt.sbprintf(buffer, "        ; Write syscall\n")
                fmt.sbprintf(buffer, "        mov rax, 1\n")
                fmt.sbprintf(buffer, "        mov rdi, 1\n")
                fmt.sbprintf(buffer, "        mov rsi, newline\n")
                fmt.sbprintf(buffer, "        mov rdx, 1\n")
                fmt.sbprintf(buffer, "        syscall\n")
            }
            else {
                fmt.sbprintf(buffer, "        call %v\n", inst.name)
                fmt.sbprintf(buffer, "        add rsp, %v\n", 8 * len(inst.args))
                fmt.sbprintf(buffer, "        push rax\n")
                nums_count += 1 - len(inst.args)
            }
        }
    }
    return nums_count
}

generate_asm :: proc(ast: ^AST) {
    out, error := os.open("output.nasm", os.O_WRONLY | os.O_TRUNC | os.O_CREATE, 0o666)
    if error != nil {
        fmt.printf("Error during file creation: %v", error)
        return
    }
    
    buffer: strings.Builder
    fmt.sbprintf(&buffer, "global _start\n")
    fmt.sbprintf(&buffer, "section .data\n")
    fmt.sbprintf(&buffer, "buffer db 32\n")
    fmt.sbprintf(&buffer, "lsqpar db '['\n")
    fmt.sbprintf(&buffer, "rsqpar db ']'\n")
    fmt.sbprintf(&buffer, "commaspace db ', '\n")
    fmt.sbprintf(&buffer, "newline db 10\n")
    fmt.sbprintf(&buffer, "section .text\n")
    fmt.sbprintf(&buffer, "print:\n")
    fmt.sbprintf(&buffer, "        push rbp\n")
    fmt.sbprintf(&buffer, "        mov rbp, rsp\n")
    fmt.sbprintf(&buffer, "        push qword 10 ; rbp - 8\n")
    fmt.sbprintf(&buffer, "        push qword 0  ; rbp - 16\n")
    fmt.sbprintf(&buffer, "        push rax\n")
    fmt.sbprintf(&buffer, "\n")
    fmt.sbprintf(&buffer, ".lp:\n")
    fmt.sbprintf(&buffer, "        cqo\n")
    fmt.sbprintf(&buffer, "        idiv qword [rbp - 8]\n")
    fmt.sbprintf(&buffer, "        cmp rdx, 0\n")
    fmt.sbprintf(&buffer, "        jge .skip1\n")
    fmt.sbprintf(&buffer, "        neg rdx\n")
    fmt.sbprintf(&buffer, ".skip1:\n")
    fmt.sbprintf(&buffer, "        add rdx, '0'\n")
    fmt.sbprintf(&buffer, "        sub rsp, 1\n")
    fmt.sbprintf(&buffer, "        mov [rsp], dl\n")
    fmt.sbprintf(&buffer, "        inc qword [rbp - 16]\n")
    fmt.sbprintf(&buffer, "        cmp rax, 0\n")
    fmt.sbprintf(&buffer, "        jne .lp\n")
    fmt.sbprintf(&buffer, "        cmp qword [rbp-24], 0\n")
    fmt.sbprintf(&buffer, "        jge .skip\n")
    fmt.sbprintf(&buffer, "        sub rsp, 1\n")
    fmt.sbprintf(&buffer, "        mov [rsp], byte '-'\n")
    fmt.sbprintf(&buffer, "        inc qword [rbp - 16]\n")
    fmt.sbprintf(&buffer, ".skip:\n")
    fmt.sbprintf(&buffer, "        ; Write syscall\n")
    fmt.sbprintf(&buffer, "        mov rax, 1\n")
    fmt.sbprintf(&buffer, "        mov rdi, 1\n")
    fmt.sbprintf(&buffer, "        mov rsi, rsp\n")
    fmt.sbprintf(&buffer, "        mov rdx, [rbp - 16]\n")
    fmt.sbprintf(&buffer, "        syscall\n")
    fmt.sbprintf(&buffer, "\n")
    fmt.sbprintf(&buffer, "        mov rsp, rbp\n")
    fmt.sbprintf(&buffer, "        pop rbp\n")
    fmt.sbprintf(&buffer, "        ret\n")

    for name, def in ast.functions {
        if strings.compare(name, "print") == 0 do continue
        fmt.sbprintf(&buffer, "%v:\n", name)
        fmt.sbprintf(&buffer, "        push rbp\n")
        fmt.sbprintf(&buffer, "        mov rbp, rsp\n")
        nums_count := generate_expr_asm(&buffer, def.body[:], def.parameters[:], ast)
        assert(nums_count == 1)
        fmt.sbprintf(&buffer, "        pop rax\n")
        fmt.sbprintf(&buffer, "        mov rsp, rbp\n")
        fmt.sbprintf(&buffer, "        pop rbp\n")
        fmt.sbprintf(&buffer, "        ret\n")
    }
    
    fmt.sbprintf(&buffer, "_start:\n")
    
    for f, i in ast.func_calls {
        nums_count: int
        for arg in f.args {
            nums_count += generate_expr_asm(&buffer, arg[:],ast.functions[f.name].parameters[:], ast)
        }
        assert(nums_count == len(f.args))
        if strings.compare(f.name, "print") == 0 {
            fmt.sbprintf(&buffer, "        mov rax, [rsp]\n")
            fmt.sbprintf(&buffer, "        call print\n")
            fmt.sbprintf(&buffer, "        add rsp, %v\n", 8 * nums_count)
            fmt.sbprintf(&buffer, "        ; Write syscall\n")
            fmt.sbprintf(&buffer, "        mov rax, 1\n")
            fmt.sbprintf(&buffer, "        mov rdi, 1\n")
            fmt.sbprintf(&buffer, "        mov rsi, newline\n")
            fmt.sbprintf(&buffer, "        mov rdx, 1\n")
            fmt.sbprintf(&buffer, "        syscall\n")
        }
        else {
            fmt.sbprintf(&buffer, "        call %v\n", f.name)
            fmt.sbprintf(&buffer, "        add rsp, %v\n", 8 * nums_count)
            fmt.sbprintf(&buffer, "        push rax\n")
        }
    }
    
    fmt.sbprintf(&buffer, "        mov rax, 0x3c\n")
    fmt.sbprintf(&buffer, "        mov rdi, 0\n")
    fmt.sbprintf(&buffer, "        syscall\n")
    os.write(out, buffer.buf[:])

    os.close(out)
}

run :: proc() -> (main_ok: bool) {
    if len(os.args) < 2 {
        fmt.printf("Usage: ijaq <file>")
        return
    } 
    filename := os.args[1]
    
    file, ok := os.read_entire_file_from_filename(filename)
    if !ok {
        fmt.printf("Couldn't open a file: %v", filename)
        return
    }

    lexer := lexer(cast(string)file)
    tokens := lexer_collect(&lexer) or_return
    ast: AST
    def: Func_Defenition
    append(&def.parameters, Func_Parameter{type = .INTEGER})   
    ast.functions["print"] = def
    parse(tokens[:], &ast) or_return
    generate_asm(&ast)
    return true
}

main :: proc() {
    if run() {
        os.exit(0)
    }
    os.exit(69)
}

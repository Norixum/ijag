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
    EQ,
    PLUS,
    LPAR,
    RPAR,
    LSQPAR,
    RSQPAR,
    DDOT,
    PRIME,
    NEWLINE,
    MINUS,
    MULTIPLY,
    DIVISON,
}

Sum :: struct {
    a, b: Token_Id,
}

List :: struct {
    start, end: int
}

Func_call :: struct{
    name: string,
    arg: Token_Id,
}


Token :: struct {
    row: int,
    column: int,
    type: Token_Type,
}

Token_Type :: union{
    Token_Id,
    Token_Num,
    Token_Op,
}

AST :: struct {
    variables: map[string][dynamic]Instruction,
    func_calls: [dynamic]Func_call,
}

PushNum :: int
PushOp :: Token_Op
PushVal :: Token_Id

Instruction :: union {
    PushNum,
    PushOp,
    PushVal,
}

next_token :: proc(tokens: []Token, index: ^int) -> ^Token
{   
    if index^ < len(tokens)-1 {
        index^ += 1
        return &tokens[index^]
    }
    return nil
}

expect_num :: proc(tokens: []Token, index: ^int) -> (result: Token_Num, ok: bool) {
    current := tokens[index^]
    next := next_token(tokens[:], index)
    if next == nil {
        fmt.printf("(%v:%v): Expected <num> but found nothing", current.row, current.column)
        return
    }
    if _num, ok := next.type.(Token_Num); !ok { 
        fmt.printf("(%v:%v): Expected <num> but found: %v", next.row, next.column, next.type)
        return
    }
    else {
        result = _num
    }
    return result, true
}

expect_id :: proc(tokens: []Token, index: ^int) -> (result: ^Token, ok: bool){
    current := tokens[index^]
    next := next_token(tokens[:], index)
    if next == nil {
        fmt.printf("(%v:%v): Expected <id> but found nothing", current.row, current.column)
        return
    }
    if _, ok := next.type.(Token_Id); !ok { 
        fmt.printf("(%v:%v): Expected <id> but found: %v", next.row, next.column, next.type)
        return
    }
    else {
        result = next
    }
    return result, true
}

expect_existing_id :: proc(tokens: []Token, index: ^int, variables:map[string][dynamic]Instruction) -> (result:Token_Id, ok:bool) {
    id := expect_id(tokens, index) or_return
    if id.type.(Token_Id) not_in variables {
        fmt.printf("(%v:%v): Variable is not defined: %v", id.row, id.column, id.type.(Token_Id))
        return
    }
    return id.type.(Token_Id), true
}

expect_op :: proc(tokens: []Token, index: ^int) -> (result: ^Token, ok: bool){
    current := tokens[index^]
    next := next_token(tokens[:], index)
    if next == nil {
        fmt.printf("(%v:%v): Expected <op> but found nothing", current.row, current.column)
        return
    }
    result = next
    if _op, ok := next.type.(Token_Op); !ok { 
        fmt.printf("(%v:%v): Expected <op> but found: %v", next.row, next.column, next.type)
        return
    }
    return result, true
}
expect_exact_op :: proc(tokens: []Token, index: ^int, expected: Token_Op) -> bool{
    op := expect_op(tokens, index) or_return
    if op.type != expected {
        fmt.printf("(%v:%v): Expected `%v` but found: %v", op.row, op.column, expected, op.type)
        return false
    }
    return true
}

parse_expr :: proc(tokens: []Token, i: ^int) -> (expr: [dynamic]Instruction, ok: bool) {
    for {
        t := &tokens[i^]
        if t.type == .NEWLINE {
            i^ += 1
            break;
        }
        switch ttype in t.type {
        case Token_Id:
            unimplemented()
        case Token_Num:
            expect_op(tokens, i) or_return
            i^ -= 1
            append(&expr, ttype)
        case Token_Op:
            switch ttype {
            case .PLUS: fallthrough
            case .MINUS: fallthrough
            case .MULTIPLY: fallthrough
            case .DIVISON:
                if len(expr) == 0 {
                    fmt.printf("(%v:%v): Expected <num> but got <op>", tokens[i^].row, tokens[i^].column)
                    return
                }
                expect_num(tokens, i) or_return
                i^ -= 1
                append(&expr, ttype)
                
            case .LPAR: 
                unimplemented()
            case .RPAR:
                unimplemented()
            case .PRIME:
                unimplemented()
            case .DDOT:
                unimplemented()
            case .EQ:
                unimplemented()
            case .LSQPAR:
                unimplemented()
            case .RSQPAR:
                unimplemented()
            case .NEWLINE:
                unreachable()
            }
        }
        i^ += 1
    }
    return expr, true
}

op_prio :: proc(op: Token_Op) -> (prio:int) {
    switch op {
    case .PLUS: fallthrough
    case .MINUS: return 1
    case .MULTIPLY: fallthrough
    case .DIVISON: return 2
    
    case .EQ: fallthrough
    case .LSQPAR: fallthrough
    case .RSQPAR: fallthrough
    case .LPAR: fallthrough
    case .RPAR: fallthrough
    case .DDOT: fallthrough
    case .PRIME: fallthrough
    case .NEWLINE:
        fmt.panicf("Error <%v> in expresion", op)
    case: 
        unreachable()
    }
}

lex :: proc(file: string) -> (tokens: [dynamic]Token, ok: bool) {
    row := 1
    column := 1
    for i := 0; i < len(file); {
        for i < len(file) && unicode.is_white_space(cast(rune)file[i]) {
            if file[i] == '\n' { 
                row += 1
                column = 1
                append(&tokens, Token{row, column, .NEWLINE})
            }
            else {
                column += 1
            }
            i += 1
        }
        if i >= len(file) { break }
        if unicode.is_letter(cast(rune)file[i]) {
            istart := i
            for i < len(file) && unicode.is_letter(cast(rune)file[i]) {
                i += 1
            }
            tid := cast(Token_Id)file[istart:i]
            append(&tokens,Token{row, column, tid})
            column += i - istart
        }
        else if unicode.is_digit(cast(rune)file[i]) {
            istart := i
            for i < len(file) && unicode.is_digit(cast(rune)file[i]) {
                i += 1
            }
            num, ok := strconv.parse_int(cast(string)file[istart:i])
            if !ok {
                fmt.printf("(%v:%v): Invalid number", row, column)
                return
            }
            tnum := cast(Token_Num)num
            append(&tokens, Token{row, column, tnum})
            column += i - istart
        }
        else if file[i] == '=' {
            append(&tokens, Token{row, column, Token_Op.EQ})
            i += 1
            column += 1
        }
        else if file[i] == '+' {
            append(&tokens, Token{row, column, Token_Op.PLUS})
            i += 1
            column += 1
        }
        else if file[i] == '(' {
            append(&tokens, Token{row, column, Token_Op.LPAR})
            i += 1
            column += 1
        }
        else if file[i] == ')' {
            append(&tokens, Token{row, column, Token_Op.RPAR})
            i += 1
            column += 1
        }
        else if file[i] == '[' {
            append(&tokens, Token{row, column, Token_Op.LSQPAR})
            i += 1
            column += 1
        }
        else if file[i] == ']' {
            append(&tokens, Token{row, column, Token_Op.RSQPAR})
            i += 1
            column += 1
        }
        else if file[i] == '*' {
            append(&tokens, Token{row, column, Token_Op.MULTIPLY})
            i += 1
            column += 1
        }
        else if file[i] == '-' {
            append(&tokens, Token{row, column, Token_Op.MINUS})
            i += 1
            column += 1
        }
        else if file[i] == '\'' {
            append(&tokens, Token{row, column, Token_Op.PRIME})
            i += 1
            column += 1
        }
        else if file[i] == '/' {
            append(&tokens, Token{row, column, Token_Op.DIVISON})
            i += 1
            column += 1
        }
        else if i + 1 < len(file) && strings.compare(cast(string)file[i:i+2], "..") == 0 {
            append(&tokens, Token{row, column, Token_Op.DDOT})
            i += 2
            column += 2
        }
        else {
            fmt.printf("(%v:%v): Unknown symbol: %v", row, column, cast(rune)file[i])
            return
        }
    }
    return tokens, true
}

parse :: proc(tokens: []Token) -> (ast: AST, ok: bool) {
    for i := 0; i < len(tokens); {
        curr_token := &tokens[i]
        switch t in tokens[i].type {
        case Token_Id:
            op, ok := expect_op(tokens, &i)
            if !ok { return }
            switch op.type.(Token_Op) {
            case .EQ:
                if t in ast.variables {
                    fmt.printf("(%v:%v): Variable redeclaration is prohibited! F@ck U!", curr_token.row, curr_token.column)
                    return
                }
                i += 1
                expr := parse_expr(tokens, &i) or_return
                ast.variables[t] = expr
            case .LPAR:
                if strings.compare(cast(string)t, "print") != 0 {
                    fmt.printf("(%v:%v): Function does not exists: %#v", curr_token.row, curr_token.column, t)
                    return
                }
                arg := expect_existing_id(tokens, &i, ast.variables) or_return

                expect_exact_op(tokens, &i, .RPAR) or_return       
                append(&ast.func_calls, Func_call{"print", arg} )
            case .MINUS:
                unreachable()
            case .DIVISON:
                unreachable()
            case .NEWLINE:
                unreachable()
            case .MULTIPLY:
                unreachable()
            case .PRIME:
                unreachable()
            case .RPAR:
                unreachable()
            case .PLUS:
                unreachable()
            case .LSQPAR:
                unreachable()
            case .RSQPAR:
                unreachable()
            case .DDOT:
                unreachable()
            case:
                unimplemented()
            }
            i += 1
        case Token_Num:
            unimplemented()
        case Token_Op:
            unimplemented()
        case:
            unreachable()
        }
        
    }
    return ast, true
}

generate_asm :: proc(ast: AST) {
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
    fmt.sbprintf(&buffer, "_start:\n")

    // fmt.sbprintf(&buffer, "        mov qword [rbp-%v], %v\n",(i+1)*8, exp)
    // a := var_find(ast.variables[:], exp.a)
    // b := var_find(ast.variables[:], exp.b)
    // fmt.sbprintf(&buffer, "        mov rax, [rbp-%v]\n", (a+1)*8)
    // fmt.sbprintf(&buffer, "        add rax, [rbp-%v]\n", (b+1)*8)
    // fmt.sbprintf(&buffer, "        mov [rbp-%v], rax\n", (i+1)*8)
    
    for f, i in ast.func_calls {
        expr, ok := ast.variables[f.arg]
        if !ok {
            fmt.printf("Variable is not defined: %v", f.arg)
        }

        nums_count := 0
        ops: [dynamic]Token_Op
        for inst in expr {
            switch inst in inst{
            case PushNum:
                nums_count += 1
                fmt.sbprintf(&buffer, "        push qword %v\n", inst)
                
            case PushOp:
                prio := op_prio(inst)
                if len(ops) == 0 || prio > op_prio(slice.last(ops[:])) {
                    append(&ops, inst)
                    continue
                }
                // dual poping
                for len(ops) > 0 && prio <= op_prio(slice.last(ops[:])) {
                    nums_count -= 2
                    assert(nums_count >= 0)
                    
                    fmt.sbprintf(&buffer, "        pop rax\n")
                    fmt.sbprintf(&buffer, "        pop rbx\n")
                    op := pop(&ops)

                    switch op {
                    case .PLUS:
                        fmt.sbprintf(&buffer, "        add rax, rbx\n")
                        fmt.sbprintf(&buffer, "        push rax\n")
                        nums_count += 1
                    case .MINUS: 
                        fmt.sbprintf(&buffer, "        sub rbx, rax\n")
                        fmt.sbprintf(&buffer, "        push rbx\n")
                        nums_count += 1
                    case .MULTIPLY: 
                        fmt.sbprintf(&buffer, "        imul rbx\n")
                        fmt.sbprintf(&buffer, "        push rax\n")
                        nums_count += 1
                    case .DIVISON:
                        fmt.sbprintf(&buffer, "        idiv rbx\n")
                        fmt.sbprintf(&buffer, "        push rax\n")
                        nums_count += 1
    
                    case .EQ: fallthrough
                    case .LSQPAR: fallthrough
                    case .RSQPAR: fallthrough
                    case .LPAR: fallthrough
                    case .RPAR: fallthrough
                    case .DDOT: fallthrough
                    case .PRIME: fallthrough
                    case .NEWLINE:
                        fmt.panicf("Error <%v> in expresion", op)
                    case: 
                        unreachable()
                    }
                }
                append(&ops, inst)
                
            case PushVal:
                unimplemented()
            }
        }
        
        for len(ops) > 0 {
            nums_count -= 2
            assert(nums_count >= 0)
            
            fmt.sbprintf(&buffer, "        pop rax\n")
            fmt.sbprintf(&buffer, "        pop rbx\n")
            op := pop(&ops)
            
            switch op {
            case .PLUS:
                fmt.sbprintf(&buffer, "        add rax, rbx\n")
                fmt.sbprintf(&buffer, "        push rax\n")
                nums_count += 1
            case .MINUS: 
                fmt.sbprintf(&buffer, "        sub rbx, rax\n")
                fmt.sbprintf(&buffer, "        push rbx\n")
                nums_count += 1
            case .MULTIPLY: 
                fmt.sbprintf(&buffer, "        imul rbx\n")
                fmt.sbprintf(&buffer, "        push rax\n")
                nums_count += 1
            case .DIVISON:
                fmt.sbprintf(&buffer, "        idiv rbx\n")
                fmt.sbprintf(&buffer, "        push rax\n")
                nums_count += 1

            case .EQ: fallthrough
            case .LSQPAR: fallthrough
            case .RSQPAR: fallthrough
            case .LPAR: fallthrough
            case .RPAR: fallthrough
            case .DDOT: fallthrough
            case .PRIME: fallthrough
            case .NEWLINE:
                fmt.panicf("Error <%v> in expresion", op)
            case: 
                unreachable()
            }
        }
        
        fmt.sbprintf(&buffer, "        mov rax, [rsp]\n")
        fmt.sbprintf(&buffer, "        call print\n")
        fmt.sbprintf(&buffer, "        add rsp, %v\n", 8 * nums_count)
        fmt.sbprintf(&buffer, "        ; Write syscall\n")
        fmt.sbprintf(&buffer, "        mov rax, 1\n")
        fmt.sbprintf(&buffer, "        mov rdi, 1\n")
        fmt.sbprintf(&buffer, "        mov rsi, newline\n")
        fmt.sbprintf(&buffer, "        mov rdx, 1\n")
        fmt.sbprintf(&buffer, "        syscall\n")
        
        // for op, i in ops {
        //     fmt.sbprintf(&buffer, "        mov rax, [rsp+%v]\n", 8 * (nums_count - i))
        //     fmt.sbprintf(&buffer, "        mov rbx, [rsp+%v]\n", 8 * (nums_count - i - 1))
    
        //     switch op {
        //     case .PLUS:
        //         fmt.sbprintf(&buffer, "        add rax, rbx\n")
        //         fmt.sbprintf(&buffer, "        mov [rsp+%v], rax\n", 8 * (nums_count - i - 1))
        //         nums_count += 1
        //     case .MINUS: 
        //         fmt.sbprintf(&buffer, "        sub rbx, rax\n")
        //         fmt.sbprintf(&buffer, "        mov [rsp+%v], rbx\n", 8 * (nums_count - i - 1))
        //         nums_count += 1
        //     case .MULTIPLY: 
        //         fmt.sbprintf(&buffer, "        imul rbx\n")
        //         fmt.sbprintf(&buffer, "        mov [rsp+%v], rax\n", 8 * (nums_count - i - 1))
        //         nums_count += 1
        //     case .DIVISON:
        //         fmt.sbprintf(&buffer, "        idiv rbx\n")
        //         fmt.sbprintf(&buffer, "        mov [rsp+%v], rax\n", 8 * (nums_count - i - 1))
        //         nums_count += 1

        //     case .EQ: fallthrough
        //     case .LSQPAR: fallthrough
        //     case .RSQPAR: fallthrough
        //     case .LPAR: fallthrough
        //     case .RPAR: fallthrough
        //     case .DDOT: fallthrough
        //     case .PRIME: fallthrough
        //     case .NEWLINE:
        //         fmt.panicf("Error <%v> in expresion", op)
        //     case: 
        //         unreachable()
        //     }
            
        // }
        // case int:
        // switch t in ast.variables[arg].expr {
        // case Sum:
        //     fmt.sbprintf(&buffer, "        mov rax, [rbp-%v]\n", (arg+1)*8)
        //     fmt.sbprintf(&buffer, "        call print\n")
        //     fmt.sbprintf(&buffer, "        ; Write syscall\n")
        //     fmt.sbprintf(&buffer, "        mov rax, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rdi, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rsi, newline\n")
        //     fmt.sbprintf(&buffer, "        mov rdx, 1\n")
        //     fmt.sbprintf(&buffer, "        syscall\n")
        // case int:
        //     fmt.sbprintf(&buffer, "        mov rax, [rbp-%v]\n", (arg+1)*8)
        //     fmt.sbprintf(&buffer, "        call print\n")
        //     fmt.sbprintf(&buffer, "        ; Write syscall\n")
        //     fmt.sbprintf(&buffer, "        mov rax, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rdi, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rsi, newline\n")
        //     fmt.sbprintf(&buffer, "        mov rdx, 1\n")
        //     fmt.sbprintf(&buffer, "        syscall\n")
        // case List:
        //     fmt.sbprintf(&buffer, "        ; Write syscall\n")
        //     fmt.sbprintf(&buffer, "        mov rax, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rdi, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rsi, lsqpar\n")
        //     fmt.sbprintf(&buffer, "        mov rdx, 1\n")
        //     fmt.sbprintf(&buffer, "        syscall\n")
            
        //     fmt.sbprintf(&buffer, "        push qword %v\n", t.start)
        //     fmt.sbprintf(&buffer, ".printlist%v:\n", i)
        //     fmt.sbprintf(&buffer, "        mov rax, [rsp]\n")
        //     fmt.sbprintf(&buffer, "        call print\n")
            
        //     fmt.sbprintf(&buffer, "        mov rax, %v\n", t.end)
        //     fmt.sbprintf(&buffer, "        cmp [rsp], rax\n")
        //     fmt.sbprintf(&buffer, "        je .skipcommaspace%v\n", i)
        //     fmt.sbprintf(&buffer, "        ; Write syscall\n")
        //     fmt.sbprintf(&buffer, "        mov rax, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rdi, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rsi, commaspace\n")
        //     fmt.sbprintf(&buffer, "        mov rdx, 2\n")
        //     fmt.sbprintf(&buffer, "        syscall\n")
            
        //     fmt.sbprintf(&buffer, ".skipcommaspace%v:\n", i)
        //     fmt.sbprintf(&buffer, "        inc qword [rsp]\n")
        //     fmt.sbprintf(&buffer, "        mov rax, %v\n", t.end)
        //     fmt.sbprintf(&buffer, "        cmp [rsp], rax\n")
        //     fmt.sbprintf(&buffer, "        jbe .printlist%v\n", i)
            
        //     fmt.sbprintf(&buffer, "        ; Write syscall\n")
        //     fmt.sbprintf(&buffer, "        mov rax, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rdi, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rsi, rsqpar\n")
        //     fmt.sbprintf(&buffer, "        mov rdx, 1\n")
        //     fmt.sbprintf(&buffer, "        syscall\n")
            
        //     fmt.sbprintf(&buffer, "        ; Write syscall\n")
        //     fmt.sbprintf(&buffer, "        mov rax, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rdi, 1\n")
        //     fmt.sbprintf(&buffer, "        mov rsi, newline\n")
        //     fmt.sbprintf(&buffer, "        mov rdx, 1\n")
        //     fmt.sbprintf(&buffer, "        syscall\n")
        //     fmt.sbprintf(&buffer, "        add rsp, 8\n")
        // } 
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

    tokens := lex(cast(string)file) or_return
    ast := parse(tokens[:]) or_return
    generate_asm(ast)
    
    return true
}

main :: proc() {
    if run() {
        os.exit(0)
    }
    os.exit(69)
}

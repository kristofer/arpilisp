; -*- mode: asm; fill-column: 88; tab-width: 8; -*-

;
; ez80lisp - Lisp interpreter for Agon eZ80
; Translated from ARM Raspberry Pi version (arpilisp.s)
;
; This implementation runs on the Agon eZ80 platform using the eZ80 processor
; in ADL mode (24-bit addressing) for maximum memory utilization.
;

        .assume adl=1                   ; Use ADL mode for 24-bit addressing

	INCLUDE "mos_api.inc"

        ORG $40000                      ; Standard Agon program start address
        JP	_start                  ; Jump over MOS header to actual code
        
        ; Pad to ensure MOS header is at exactly offset $40
        ORG $40000 + $40
        DB	"MOS"           ; Flag for MOS - to confirm this is a valid MOS command
	DB	0               ; MOS header version 0
	DB	1               ; Flag for run mode (0: Z80, 1: ADL)

; eZ80 Register Mapping from ARM:
; ARM r0-r3  -> eZ80 A, BC, DE, HL (function args/return values)  
; ARM r4     -> eZ80 IX (general purpose)
; ARM r5     -> eZ80 IY (lambda argument values)
; ARM r6     -> eZ80 BC' (Lisp stack pointer, using alternate BC)
; ARM r7     -> eZ80 DE' (current S-expression / syscall number)
; ARM r8     -> eZ80 HL' (S-expression being evaluated)
; ARM r9     -> eZ80 IX' (environment - association list)
; ARM r10-r11-> eZ80 stack-based (saved/restored as needed)
; ARM r12    -> eZ80 A' (scratch register using alternate A)
; ARM r13-r15-> eZ80 SP, return addresses handled differently

; Memory Layout Constants
OBARRAYMAX:     EQU 2000        ; Symbol table size
CELLMAX:        EQU 10000       ; Maximum number of cons cells
LISPSTACKMAX:   EQU 1000        ; Lisp stack size

; Object type constants
NIL:            EQU 0           ; Null pointer
SYMMASK:        EQU 1           ; Symbol identifier bit
MARKMASK:       EQU 2           ; GC mark bit
EOF_MARKER:     EQU $FFFF       ; Special marker for EOF (not a valid pointer)

; Agon system call numbers (MOS API)
MOS_GETCHAR:    EQU $00         ; Get character from input
MOS_PUTCHAR:    EQU $01         ; Put character to output
MOS_EXIT:       EQU $00         ; Exit program

; Token types for tokenizer
TOKEN_EOF:      EQU 0           ; End of input
TOKEN_LPAREN:   EQU 1           ; (
TOKEN_RPAREN:   EQU 2           ; )
TOKEN_SYMBOL:   EQU 3           ; Symbol/identifier
TOKEN_NUMBER:   EQU 4           ; Number (future use)
TOKEN_DOT:      EQU 5           ; . (future use)

; Token buffer constants
TOKENBUFMAX:    EQU 100         ; Maximum tokens in buffer
TOKENMAX:       EQU 50          ; Maximum token data length


;===============================================================================
; Data Section
;===============================================================================

; Global variables
obarray:        DS OBARRAYMAX * 3       ; Symbol hash table (24-bit pointers)
cells:          DS CELLMAX * 6          ; Cons cell pool (car, cdr pairs)
lispstack:      DS LISPSTACKMAX * 3     ; Lisp evaluation stack
freelist:       DS 3                    ; Pointer to free cell list
lispsp:         DS 3                    ; Lisp stack pointer
cellcount:      DS 3                    ; Number of allocated cells

; String constants
foo:            DB "foo", $0D, $0A, 0
greeting:       DB "Agon eZ80 Lisp", $0D, $0A, 0
prompt:         DB "> ", 0
error_msg:      DB "Error: ", 0
goodbye_msg:    DB "Goodbye!", $0D, $0A, 0
immediate_exit_msg: DB $0D, $0A, "Exiting to MOS...", $0D, $0A, 0
debug_char_msg: DB "[", 0

; Tokenizer data structures
token_buffer:   DS TOKENBUFMAX * 4      ; Token buffer: type(1) + length(1) + data_ptr(2)
token_data:     DS TOKENBUFMAX * TOKENMAX ; Storage for token data
token_count:    DS 1                    ; Number of tokens in buffer
token_index:    DS 1                    ; Current token being parsed
current_line:   DS TOKENMAX             ; Current input line buffer
line_pos:       DS 1                    ; Position in current line

;===============================================================================
; Program Entry Point
;===============================================================================

; Save original stack pointer for clean exit
original_sp:    DS 3

_start:
        ; Preserve MOS state and initialize stack for ADL mode
        ; Save original stack pointer for clean exit
        ld      (original_sp), sp
        
        ; Initialize basic registers only
        ld      hl, 0
        ld      bc, 0
        ld      de, 0
        
        ; Test basic output first
        ld      hl, foo
        call    print_string
        
        ; Initialize memory system carefully
        call    init_memory
                
        ; Initialize symbol table
        call    init_symbols
        
        ; Initialize built-in symbols (quote, atom, car, cdr, etc.)
        call    init_builtin_symbols
               
        ; Display greeting
        ld      hl, greeting
        call    print_string
        
        ; Start REPL (Read-Eval-Print Loop)
        call    repl
        
        ; Exit cleanly
        ret                             ; Return to MOS instead of custom exit

;===============================================================================
; Memory Management
;===============================================================================

init_memory:
        ; Initialize free list of cons cells with simple linking
        ld      hl, cells               ; Start of cell pool
        ld      (freelist), hl          ; First free cell
        ld      bc, CELLMAX - 1         ; Use BC as counter
        
init_cell_loop:
        ; Current cell at HL, calculate next cell address
        push    bc                      ; Save counter
        push    hl                      ; Save current cell
        ld      de, 6                   ; Cell size
        add     hl, de                  ; HL = next cell address
        push    hl
        pop     de                      ; DE = next cell address
        pop     hl                      ; Restore current cell
        
        ; Store next cell address in current cell's cdr field (offset +3)  
        push    hl
        push    de
        ld      de, 3
        add     hl, de
        pop     de
        ld      (hl), de                ; Store next cell pointer
        pop     hl
        
        ; Move to next cell
        push    de
        pop     hl
        pop     bc                      ; Restore counter
        dec     bc
        
        ; Check if more cells to process
        ld      a, c
        or      b
        jr      nz, init_cell_loop
        
        ; Last cell cdr points to NIL
        push    hl
        ld      de, 3
        add     hl, de
        ld      (hl), NIL
        pop     hl
        
        ; Initialize Lisp stack pointer  
        ld      hl, lispstack
        ld      (lispsp), hl
        
        ; Initialize cell counter to zero
        ld      hl, 0
        ld      (cellcount), hl
        
        ret

; Allocate a new cons cell
cons:   ; HL = car, DE = cdr, returns new cell in HL
        push    bc
        push    ix
        
        ; Save car and cdr values
        push    hl                      ; Save car
        push    de                      ; Save cdr
        
        ; Get free cell
        ld      hl, freelist
        ld      ix, (hl)
        push    ix
        pop     hl
        ld      bc, 3
        add     hl, bc
        ld      bc, (hl)                ; Next free cell
        ld      hl, freelist
        ld      (hl), bc                ; Update free list
        
        ; Initialize cell
        pop     de                      ; Restore cdr
        pop     hl                      ; Restore car
        ld      (ix), hl                ; Set car
        push    ix
        pop     hl
        ld      bc, 3
        add     hl, bc
        ld      (hl), de                ; Set cdr
        
        ; Return cell address
        push    ix
        pop     hl
        
        ; Update cell count
        ld      bc, (cellcount)
        inc     bc
        ld      (cellcount), bc
        
        pop     ix
        pop     bc
        ret

; Get car of cons cell
car:    ; HL = cons cell, returns car in HL
        push    hl
        pop     ix
        ld      hl, (ix)
        ret

; Get cdr of cons cell  
cdr:    ; HL = cons cell, returns cdr in HL
        push    hl
        pop     ix
        ld      hl, (ix + 3)
        ret

; Test if object is atom (not a cons cell)
atom:   ; HL = object, returns Z flag set if atom
        ld      a, h
        or      l                       ; Test if NIL
        ret     z                       ; NIL is an atom
        
        ; Check if it's a symbol (bit 0 set)
        ld      a, l
        and     SYMMASK
        ret     nz                      ; Symbol is an atom
        
        ; Otherwise it's a cons cell
        or      1                       ; Clear Z flag
        ret

;===============================================================================
; Lisp Stack Operations
;===============================================================================

pushlisp:       ; Push HL onto Lisp stack
        push    ix
        ld      ix, (lispsp)
        ld      (ix), hl
        push    ix
        pop     hl
        ld      bc, 3
        add     hl, bc
        push    hl
        pop     ix
        ld      (lispsp), ix
        pop     ix
        ret

poplisp:        ; Pop from Lisp stack into HL
        push    ix
        ld      ix, (lispsp)
        push    ix
        pop     hl
        ld      bc, -3
        add     hl, bc
        push    hl
        pop     ix
        ld      (lispsp), ix
        ld      hl, (ix)
        pop     ix
        ret

;===============================================================================
; I/O Functions
;===============================================================================

print_string:   ; Print null-terminated string at HL
        push    hl
        
print_loop:
        ld      a, (hl)                 ; Load character
        or      a                       ; Check for null terminator
        jr      z, print_done
        
        ; Output character via MOS (character already in A)
        rst.lil $10                     ; MOS character output
        
        inc     hl                      ; Next character
        jr      print_loop
        
print_done:
        pop     hl
        ret

getchar:        ; Get character from input, return in A
        ; Simplified approach - just get a key and return ASCII
        push    bc                      ; Save BC
        
getchar_loop:
        MOSCALL mos_getkey              ; Get key from keyboard
        or      a                       ; Check if zero (no key)
        jr      z, getchar_loop         ; Keep waiting if no key
        
        ; The key is now in A - check if it's printable ASCII or special chars we want
        cp      127                     ; Reject high ASCII
        jr      nc, getchar_loop
        cp      32                      ; Accept printable chars (32-126)
        jr      nc, valid_char
        ; Accept specific control chars we need
        cp      13                      ; CR (Enter)
        jr      z, valid_char
        cp      10                      ; LF 
        jr      z, valid_char
        cp      26                      ; Ctrl-Z (exit)
        jr      z, valid_char
        cp      4                       ; Ctrl-D (EOF)
        jr      z, valid_char
        jr      getchar_loop            ; Skip other control chars
        
valid_char:
        pop     bc                      ; Restore BC
        
        ; Check for Ctrl-Z (immediate exit)
        cp      26                      ; Ctrl-Z?
        jr      z, immediate_exit
        
        ; Echo the character back to screen for printable chars
        cp      32                      ; Printable characters (>= space)
        jr      nc, getchar_echo        ; Echo printable chars
        
        ; Handle special control characters
        cp      13                      ; Carriage return?
        jr      z, getchar_echo_cr      ; Echo CR + LF
        jr      getchar_no_echo         ; Skip echo for other control chars
        
getchar_echo:
        push    af                      ; Save character
        rst.lil $10                     ; Echo to screen
        pop     af                      ; Restore character
        jr      getchar_no_echo
        
getchar_echo_cr:
        ; Echo carriage return and line feed
        push    af                      ; Save character
        ld      a, 13                   ; CR
        rst.lil $10
        ld      a, 10                   ; LF
        rst.lil $10
        pop     af                      ; Restore original character
        
getchar_no_echo:
        ; Store character in lookbuffer
        ld      (lookbuffer), a
        ret

immediate_exit:
        ; Print exit message and return to MOS immediately
        ld      hl, immediate_exit_msg
        call    print_string
        ; Jump directly to the end of the program instead of returning
        jp      finish

;===============================================================================
; Tokenizer Functions
;===============================================================================

; Read a complete line of input into current_line buffer
read_line:
        ld      hl, current_line
        ld      b, 0                    ; Character count
        
read_line_loop:
        call    getchar
        cp      13                      ; Carriage return?
        jr      z, read_line_done
        cp      4                       ; Ctrl-D (EOF)?
        jr      z, read_line_eof
        cp      26                      ; Ctrl-Z (immediate exit)?
        jr      z, immediate_exit
        
        ; Store character in line buffer
        ld      (hl), a
        inc     hl
        inc     b
        
        ; Check buffer overflow
        ld      a, b
        cp      TOKENMAX - 1
        jr      c, read_line_loop
        
read_line_done:
        ; Null terminate the line
        ld      (hl), 0
        ld      a, b
        ld      (line_pos), a           ; Store line length
        xor     a                       ; Return success
        ret
        
read_line_eof:
        ; EOF encountered
        ld      (hl), 0
        ld      a, 1                    ; Return EOF flag
        ret

; Tokenize the current line into token_buffer
tokenize_line:
        ld      hl, current_line
        ld      bc, 0                   ; BC = position in line
        ld      de, 0                   ; DE = token count
        
tokenize_loop:
        ; Skip whitespace
        call    skip_whitespace_in_line
        
        ; Check for end of line
        ld      a, (hl)
        or      a
        jr      z, tokenize_done
        
        ; Check for special characters
        cp      '('
        jr      z, tokenize_lparen
        cp      ')'
        jr      z, tokenize_rparen
        cp      '.'
        jr      z, tokenize_dot
        
        ; Must be a symbol
        call    tokenize_symbol
        jr      tokenize_loop
        
tokenize_lparen:
        ld      a, TOKEN_LPAREN
        call    add_simple_token
        inc     hl
        jr      tokenize_loop
        
tokenize_rparen:
        ld      a, TOKEN_RPAREN
        call    add_simple_token
        inc     hl
        jr      tokenize_loop
        
tokenize_dot:
        ld      a, TOKEN_DOT
        call    add_simple_token
        inc     hl
        jr      tokenize_loop
        
tokenize_done:
        ; Add EOF token
        ld      a, TOKEN_EOF
        call    add_simple_token
        ld      a, e
        ld      (token_count), a
        ret

; Skip whitespace in line, HL points to current position
skip_whitespace_in_line:
skip_ws_loop:
        ld      a, (hl)
        cp      ' '
        jr      z, skip_ws_next
        cp      9                       ; Tab
        jr      z, skip_ws_next
        ret                             ; Non-whitespace found
        
skip_ws_next:
        inc     hl
        jr      skip_ws_loop

; Add a simple token (no data) - token type in A
add_simple_token:
        push    hl
        push    de
        
        ; Calculate token buffer position
        ld      h, 0
        ld      l, e                    ; E contains token count
        add     hl, hl
        add     hl, hl                  ; HL = token_count * 4
        ld      bc, token_buffer
        add     hl, bc                  ; HL points to token slot
        
        ; Store token: type, length=0, data_ptr=0
        ld      (hl), a                 ; Token type
        inc     hl
        ld      (hl), 0                 ; Length = 0
        inc     hl
        ld      (hl), 0                 ; Data ptr low = 0
        inc     hl
        ld      (hl), 0                 ; Data ptr high = 0
        
        inc     e                       ; Increment token count
        pop     de
        pop     hl
        ret

; Tokenize a symbol starting at HL
tokenize_symbol:
        push    hl                      ; Save start position
        ld      c, 0                    ; Symbol length counter
        push    hl                      ; Save start for later
        
symbol_length_loop:
        ld      a, (hl)
        ; Debug: print the character being checked
        push    af
        rst.lil $10
        pop     af
        
        call    is_symbol_char
        jr      z, symbol_length_done   ; Z flag set = not valid char
        inc     hl
        inc     c
        jr      symbol_length_loop
        
symbol_length_done:
        ; C contains symbol length, HL points after symbol
        push    hl                      ; Save end position
        
        ; Calculate destination in token_data
        ld      a, e                    ; Token count
        ld      h, 0
        ld      l, a
        push    de                      ; Save DE (token count in E)
        ld      de, TOKENMAX
        call    multiply_hl_de          ; HL = token_count * TOKENMAX
        ld      de, token_data
        add     hl, de                  ; HL points to token data space
        
        ; Set up for copy: HL=dest, BC=length, source on stack
        ld      d, h
        ld      e, l                    ; DE = destination
        ld      b, 0                    ; BC = symbol length
        pop     hl                      ; HL = end position (discard)
        pop     hl                      ; HL = start position
        
        ; Copy the symbol data
        ld      a, c
        or      a
        jr      z, symbol_copy_done     ; Skip if zero length
        ldir                            ; Copy BC bytes from HL to DE
        
symbol_copy_done:
        ; Null terminate the symbol
        ld      a, 0
        ld      (de), a
        
        ; Create token entry
        pop     de                      ; Restore DE (token count in E)
        pop     hl                      ; HL = end position after symbol
        ld      a, TOKEN_SYMBOL
        ld      b, c                    ; B = symbol length
        call    add_symbol_token
        ret

; Add symbol token - type in A, length in B, data ptr calculated
add_symbol_token:
        push    hl
        push    de
        
        ; Calculate token buffer position
        ld      h, 0
        ld      l, e                    ; E contains token count
        add     hl, hl
        add     hl, hl                  ; HL = token_count * 4
        ld      de, token_buffer
        add     hl, de                  ; HL points to token slot
        
        ; Store token type
        ld      (hl), a
        inc     hl
        
        ; Store length
        ld      (hl), b
        inc     hl
        
        ; Calculate and store data pointer
        ld      a, e                    ; Token count
        push    hl
        ld      h, 0
        ld      l, a
        ld      de, TOKENMAX
        call    multiply_hl_de          ; HL = token_count * TOKENMAX
        ld      de, token_data
        add     hl, de                  ; HL = data pointer
        pop     de                      ; DE = token slot position
        push    hl                      ; Save data pointer
        push    de
        pop     ix                      ; IX = token slot position
        pop     hl                      ; HL = data pointer
        ld      (ix), l                 ; Store data ptr low
        ld      (ix + 1), h             ; Store data ptr high
        
        inc     e                       ; Increment token count
        pop     de
        pop     hl
        ret

; Check if character in A is valid for symbols
; Return: Z flag set if NOT valid, Z flag clear if valid
is_symbol_char:
        cp      '('
        ret     z                       ; Not valid (Z set)
        cp      ')'
        ret     z                       ; Not valid (Z set)
        cp      '.'
        ret     z                       ; Not valid (Z set)
        cp      ' '
        ret     c                       ; Control chars not valid (C set, Z varies)
        cp      127
        ret     nc                      ; DEL and above not valid (C clear, Z varies)
        ; Valid symbol character - clear Z flag
        cp      a                       ; Always clears Z flag (makes it NZ)
        ret

; Multiply HL by DE, result in HL (simple version)
multiply_hl_de:
        ld      c, l
        ld      b, h                    ; BC = HL
        ld      hl, 0
mult_loop:
        ld      a, c
        or      b
        ret     z
        add     hl, de
        dec     bc
        jr      mult_loop

;===============================================================================
; Token-based Parser Functions
;===============================================================================

; Get current token info
; Output: A = token type, B = token length, HL = data pointer
get_current_token:
        ld      a, (token_index)
        ld      h, 0
        ld      l, a
        add     hl, hl
        add     hl, hl                  ; HL = token_index * 4
        ld      de, token_buffer
        add     hl, de                  ; HL points to token
        
        ld      a, (hl)                 ; A = token type
        inc     hl
        ld      b, (hl)                 ; B = token length
        inc     hl
        ld      e, (hl)                 ; E = data ptr low
        inc     hl
        ld      d, (hl)                 ; D = data ptr high
        ld      h, d
        ld      l, e                    ; HL = data pointer
        ret

; Advance to next token
next_token:
        ld      a, (token_index)
        inc     a
        ld      (token_index), a
        ret

; New read_expr using tokenizer
; Output: HL = parsed expression
read_expr_new:
        call    get_current_token
        
        ; Check token type
        cp      TOKEN_EOF
        jr      z, read_eof_new
        cp      TOKEN_LPAREN
        jr      z, read_list_new
        cp      TOKEN_RPAREN
        jr      z, read_error_new
        cp      TOKEN_SYMBOL
        jr      z, read_symbol_new
        
        ; Unknown token
        jr      read_error_new
        
read_eof_new:
        ld      hl, EOF_MARKER
        ret
        
read_error_new:
        ld      hl, NIL
        ret
        
read_symbol_new:
        ; B = length, HL = data pointer
        ; Copy symbol data to internbuffer
        ld      c, b                    ; C = length
        ld      de, internbuffer
        ld      b, 0                    ; BC = length (zero high byte)
        ld      (internbufferlen), bc
        
        ; Copy the symbol data
        ld      a, c
        or      a
        jr      z, symbol_copied        ; Skip if zero length
        ldir                            ; Copy BC bytes from HL to DE
        
symbol_copied:
        ; Null terminate
        ld      a, 0
        ld      (de), a
        
        call    next_token
        call    intern
        ret
        
read_list_new:
        call    next_token              ; Skip opening (
        
        ; Check for empty list
        call    get_current_token
        cp      TOKEN_RPAREN
        jr      z, read_empty_list_new
        
        ; Read first element
        call    read_expr_new
        push    hl                      ; Save first element
        
        ; Create list starting with first element
        push    hl
        pop     bc                      ; BC = first element
        ld      de, NIL
        call    lisp_cons
        push    hl
        pop     ix                      ; IX = list start
        push    hl
        pop     iy                      ; IY = list end
        
read_list_loop_new:
        call    get_current_token
        
        ; Check for end of list
        cp      TOKEN_RPAREN
        jr      z, read_list_done_new
        cp      TOKEN_EOF
        jr      z, read_error_new       ; Unexpected EOF
        
        ; Read next element
        call    read_expr_new
        
        ; Append to list
        push    hl
        pop     bc                      ; BC = new element
        ld      de, NIL
        call    lisp_cons               ; Create new cons cell
        
        ; Link to end of list (need to handle 24-bit addressing)
        push    ix                      ; Save list start
        push    iy
        pop     hl                      ; HL = current list end
        ld      bc, 3
        add     hl, bc                  ; HL points to CDR of last cell
        push    hl
        pop     ix                      ; IX = CDR location
        ld      (ix), l                 ; Store new cell pointer low
        ld      (ix + 1), h             ; Store new cell pointer high
        ld      (ix + 2), 0             ; Clear high byte for 24-bit
        
        push    hl
        pop     iy                      ; Update list end
        pop     ix                      ; Restore list start
        
        jr      read_list_loop_new
        
read_list_done_new:
        call    next_token              ; Skip closing )
        push    ix
        pop     hl                      ; Return list start
        ret
        
read_empty_list_new:
        call    next_token              ; Skip closing )
        ld      hl, NIL
        ret

; New main read function
; Output: HL = parsed expression or EOF_MARKER
read_expression:
        ; Read a line of input
        call    read_line
        or      a
        jr      nz, read_expr_eof       ; EOF
        
        ; Tokenize the line
        call    tokenize_line
        
        ; Reset token index
        xor     a
        ld      (token_index), a
        
        ; Parse expression from tokens
        call    read_expr_new
        ret
        
read_expr_eof:
        ld      hl, EOF_MARKER
        ret

print_hex_byte:
        ; Print byte in A as two hex digits
        push    af
        rrca
        rrca
        rrca
        rrca
        and     $0F
        call    print_hex_digit
        pop     af
        and     $0F
        call    print_hex_digit
        ld      a, ']'
        rst.lil $10
        ret

print_hex_digit:
        ; Print hex digit in A (0-15)
        cp      10
        jr      c, print_digit
        add     a, 'A' - 10
        rst.lil $10
        ret
print_digit:
        add     a, '0'
        rst.lil $10
        ret

;===============================================================================
; Symbol System - Obarray and Interning
;===============================================================================

; Symbol interning buffer
INTERNMAX:      EQU 32
internbufferlen: DS 3
internbuffer:   DS INTERNMAX

; Pre-defined symbols in obarray
init_symbols:
        ; Properly initialize the obarray by clearing it
        ld      hl, obarray
        ld      bc, OBARRAYMAX * 3      ; Size of obarray in bytes
        ld      de, obarray + 1
        ld      (hl), 0                 ; Clear first byte
        ldir                            ; Copy 0 to entire obarray
        
        ; Clear the intern buffer length
        ld      hl, 0
        ld      (internbufferlen), hl
        ret

init_builtin_symbols:
        ; Create built-in symbols in obarray and update their references
        
        ; Create "quote" symbol
        ld      hl, quote_str
        ld      bc, 5                   ; length of "quote"
        ld      (internbufferlen), bc
        ld      hl, quote_str
        ld      de, internbuffer
        ldir                            ; Copy "quote" to internbuffer
        call    intern
        ld      (quote_symbol), hl
        
        ; Create "atom" symbol  
        ld      hl, atom_str
        ld      bc, 4                   ; length of "atom"
        ld      (internbufferlen), bc
        ld      hl, atom_str
        ld      de, internbuffer
        ldir                            ; Copy "atom" to internbuffer
        call    intern
        ld      (atom_symbol), hl
        
        ; Create "car" symbol
        ld      hl, car_str
        ld      bc, 3                   ; length of "car"
        ld      (internbufferlen), bc
        ld      hl, car_str
        ld      de, internbuffer
        ldir                            ; Copy "car" to internbuffer
        call    intern
        ld      (car_symbol), hl
        
        ; Create "cdr" symbol
        ld      hl, cdr_str
        ld      bc, 3                   ; length of "cdr"
        ld      (internbufferlen), bc
        ld      hl, cdr_str
        ld      de, internbuffer
        ldir                            ; Copy "cdr" to internbuffer
        call    intern
        ld      (cdr_symbol), hl
        
        ret

; Intern a symbol from internbuffer
; Output: HL = symbol reference with SYMMASK set
intern:
        push    bc
        push    de
        push    ix
        
        ; Search for existing symbol in obarray
        ld      ix, obarray
        
search_loop:
        ; Check if we've reached end of obarray
        ld      a, (ix)
        or      (ix + 1)
        or      (ix + 2)
        jr      z, create_new_symbol    ; Found empty slot
        
        ; Compare symbol lengths
        ld      hl, (ix)                ; Length in obarray
        ld      bc, (internbufferlen)   ; Length to intern
        ld      a, h
        cp      b
        jr      nz, next_symbol
        ld      a, l
        cp      c
        jr      nz, next_symbol
        
        ; Compare symbol strings
        push    ix
        push    ix
        pop     de
        push    de
        ld      hl, 3
        push    hl
        pop     bc
        pop     hl
        add     hl, bc
        push    hl
        pop     de
        ld      hl, internbuffer
        ld      bc, (internbufferlen)
        call    compare_strings
        pop     ix
        jr      z, found_symbol         ; Strings match
        
next_symbol:
        ; Move to next symbol slot
        ld      bc, (ix)                ; Get length
        push    ix                      ; Skip string
        pop     hl
        add     hl, bc
        push    hl
        pop     ix
        push    ix                      ; Skip length field
        pop     hl
        ld      bc, 3
        add     hl, bc
        push    hl
        pop     ix
        ; Align to 4-byte boundary (simplified)
        push    ix
        pop     hl
        ld      a, l
        and     3
        jr      z, search_loop
        ld      bc, 4
        push    hl
        and     a
        sbc     hl, hl
        ld      l, a
        push    hl
        pop     de
        pop     hl
        add     hl, bc
        and     a
        sbc     hl, de
        push    hl
        pop     ix
        jr      search_loop

create_new_symbol:
        ; Create new symbol at IX
        ld      bc, (internbufferlen)
        ld      (ix), bc                ; Store length
        
        ; Copy string
        ld      hl, internbuffer
        push    ix
        pop     de
        push    de
        ld      hl, 3
        push    hl
        pop     bc
        pop     hl
        add     hl, bc
        push    hl
        pop     de
        ldir                            ; Copy BC bytes
        
found_symbol:
        ; Return symbol reference with mask
        push    ix
        pop     hl
        ld      bc, obarray
        and     a
        sbc     hl, bc                  ; Offset from obarray base
        ld      a, l
        or      SYMMASK                 ; Set symbol bit
        ld      l, a
        
        pop     ix
        pop     de
        pop     bc
        ret

; Compare strings: HL vs DE, length BC
; Output: Z flag set if equal
compare_strings:
        push    hl
        push    de
        push    bc
        
compare_loop:
        ld      a, b
        or      c
        jr      z, strings_equal        ; Length reached, equal
        
        ld      a, (hl)
        ld      b, a
        ld      a, (de)
        cp      b
        jr      nz, strings_not_equal
        
        inc     hl
        inc     de
        dec     bc
        jr      compare_loop
        
strings_equal:
        xor     a                       ; Set Z flag
        jr      compare_done
        
strings_not_equal:
        or      1                       ; Clear Z flag
        
compare_done:
        pop     bc
        pop     de
        pop     hl
        ret

;===============================================================================
; Read-Eval-Print Loop
;===============================================================================

; Global environment variable
global_env:     DS 3

repl:
        ; Initialize global environment
        ld      hl, NIL
        ld      (global_env), hl
        
        ; Print prompt and read-eval-print loop
repl_loop:
        ; Print prompt
        ld      hl, prompt
        call    print_string
        
        ; Read expression using new tokenizer
        call    read_expression
        
        ; Check for EOF marker
        ld      bc, EOF_MARKER
        and     a
        sbc     hl, bc
        jr      z, repl_exit
        add     hl, bc                  ; Restore HL
        
        ; Save expression for evaluation
        push    hl
        
        ; Evaluate expression with global environment
        ld      de, (global_env)        ; Load global environment
        call    eval
        
        ; Print result
        call    print_expr
        
        ; Print newline
        ld      a, $0D
        rst.lil $10
        ld      a, $0A
        rst.lil $10
        
        ; Clean up stack
        pop     hl
        
        ; Continue REPL
        jr      repl_loop

repl_exit:
        ld      hl, goodbye_msg
        call    print_string
        ret

; Association list lookup for environments
; Input: HL = key (symbol), DE = environment (alist)
; Output: HL = value or NIL if not found
assoc:
        push    bc
        push    ix
        
        push    de                      ; IX = current alist position
        pop     ix
        
assoc_loop:
        ; Check if end of list
        ld      a, ixh
        or      ixl
        jr      z, assoc_not_found
        
        ; Get car of current pair
        ld      bc, (ix)               ; BC = current pair
        push    bc
        pop     hl
        ld      de, (hl)               ; DE = key of current pair
        
        ; Compare keys
        push    hl
        call    lisp_eq                ; Compare HL and DE
        pop     hl
        jr      z, assoc_found
        
        ; Move to next pair
        ld      ix, (ix + 3)           ; IX = cdr of alist
        jr      assoc_loop
        
assoc_found:
        ; Return the cdr of the matching pair
        push    bc
        pop     hl
        ld      de, 3
        add     hl, de
        ld      hl, (hl)               ; HL = value
        jr      assoc_done
        
assoc_not_found:
        ld      hl, NIL
        
assoc_done:
        pop     ix
        pop     bc
        ret

;===============================================================================
; Core Lisp Evaluation Functions
;===============================================================================

; Test if object is an atom (symbol or NIL)
; Input: HL = object to test
; Output: Z flag set if atom, cleared if list
lisp_atom:
        push    af
        
        ; Test for NIL
        ld      a, h
        or      l
        jr      z, atom_yes             ; NIL is an atom
        
        ; Test for symbol (bit 0 set)
        ld      a, l
        and     SYMMASK
        jr      nz, atom_yes            ; Symbol is an atom
        
        ; It's a cons cell (not an atom)
        or      1                       ; Clear Z flag
        pop     af
        ret

atom_yes:
        xor     a                       ; Set Z flag
        pop     af
        ret

; Get car (first element) of cons cell
; Input: HL = cons cell
; Output: HL = car value, or error if applied to symbol
lisp_car:
        push    bc
        push    de
        
        ; Check if it's a symbol (error case)
        ld      a, l
        and     SYMMASK
        jr      nz, car_error
        
        ; Check for NIL
        ld      a, h
        or      l
        jr      z, car_done             ; car of NIL is NIL
        
        ; Get car from cons cell
        push    hl
        pop     bc                  ; BC = cons cell address
        push    bc
        pop     hl
        ld      hl, (hl)                ; HL = car value
        
car_done:
        pop     de
        pop     bc
        ret

car_error:
        ; Return NIL on error (simplified error handling)
        ld      hl, NIL
        jr      car_done

; Get cdr (rest) of cons cell
; Input: HL = cons cell  
; Output: HL = cdr value, or error if applied to symbol
lisp_cdr:
        push    bc
        push    de
        
        ; Check if it's a symbol (error case)
        ld      a, l
        and     SYMMASK
        jr      nz, cdr_error
        
        ; Check for NIL
        ld      a, h
        or      l
        jr      z, cdr_done             ; cdr of NIL is NIL
        
        ; Get cdr from cons cell
        push    hl
        pop     bc                  ; BC = cons cell address
        push    bc
        pop     hl
        ld      de, 3
        add     hl, de
        ld      hl, (hl)                ; HL = cdr value (offset 3 for 24-bit)
        
cdr_done:
        pop     de
        pop     bc
        ret

cdr_error:
        ; Return NIL on error (simplified error handling)
        ld      hl, NIL
        jr      cdr_done

; Create new cons cell
; Input: BC = car value, DE = cdr value
; Output: HL = new cons cell, or NIL if out of memory
lisp_cons:
        push    ix
        push    af
        
        ; Get free cell from free list
        ld      ix, (freelist)
        push    ix
        pop     hl                      ; HL = freelist value
        ld      a, h                    ; Check if freelist is NIL
        or      l
        jr      nz, cons_allocate       ; Have free cells, proceed
        
        ; No free cells - trigger garbage collection
        ; Save arguments on Lisp stack
        call    pushlisp_bc             ; Push BC (car)
        call    pushlisp_de             ; Push DE (cdr)
        
        ; Run garbage collection
        call    collectgarbage
        
        ; Restore arguments from Lisp stack  
        call    poplisp_de              ; Pop DE (cdr)
        call    poplisp_bc              ; Pop BC (car)
        
        ; Check if GC freed any cells
        ld      ix, (freelist)
        push    ix
        pop     hl
        ld      a, h
        or      l
        jr      z, cons_panic           ; Still no memory - panic
        
cons_allocate:
        ; Update free list to next cell
        ld      hl, (ix + 3)            ; Get next free cell
        ld      (freelist), hl          ; Update freelist
        
        ; Initialize new cell
        ld      (ix), bc                ; Set car
        ld      (ix + 3), de            ; Set cdr
        
        ; Return new cell address
        push    ix
        pop     hl                      ; HL = new cell address
        
        ; Update cell count
        ld      hl, (cellcount)
        inc     hl
        ld      (cellcount), hl
        
        ; Cell address is already in HL from above
        
        pop     af
        pop     ix
        ret

cons_panic:
        ; Out of memory even after GC
        ld      hl, panic_memory_msg
        call    print_string
        jp      finish

; Helper functions for pushing/popping specific registers to Lisp stack
pushlisp_bc:
        push    hl
        push    bc
        pop     hl
        call    pushlisp
        pop     hl
        ret

pushlisp_de:
        push    hl
        push    de
        pop     hl  
        call    pushlisp
        pop     hl
        ret

poplisp_bc:
        push    hl
        call    poplisp
        push    hl
        pop     bc
        pop     hl
        ret

poplisp_de:
        push    hl
        call    poplisp
        push    hl
        pop     de
        pop     hl
        ret

; Test if two objects are equal
; Input: HL = object1, DE = object2
; Output: Z flag set if equal
lisp_eq:
        push    bc
        
        ; Compare the two values directly
        ld      a, h
        cp      d
        jr      nz, eq_not_equal
        
        ld      a, l  
        cp      e
        jr      nz, eq_not_equal
        
        ; They are equal
        xor     a                       ; Set Z flag
        pop     bc
        ret

eq_not_equal:
        or      1                       ; Clear Z flag
        pop     bc
        ret

; Main evaluation function
; Input: HL = expression to evaluate, DE = environment
; Output: HL = evaluated result
eval:
        push    bc
        push    ix
        push    iy
        
        ; Save expression and environment
        push    hl                      ; IX = expression
        pop     ix
        push    de                      ; IY = environment
        pop     iy
        
        ; Check if expression is an atom
        call    lisp_atom
        jr      nz, eval_list           ; Not an atom, evaluate as list
        
        ; It's an atom - check if it's NIL
        ld      a, h
        or      l
        jp      z, eval_done            ; NIL evaluates to itself
        
        ; Check if it's a symbol
        ld      a, l
        and     SYMMASK
        jp      z, eval_done            ; Not a symbol, return as-is
        
        ; It's a symbol - look up in environment
        push    iy                      ; DE = environment
        pop     de
        call    assoc                   ; Look up symbol
        jp      eval_done

eval_list:
        ; It's a list - get the first element (operator)
        push    ix                      ; HL = expression
        pop     hl
        call    lisp_car                ; Get first element
        push    hl
        pop     bc                  ; BC = operator
        
        ; Check for quote special form
        ld      de, quote_symbol
        push    bc
        pop     hl
        call    lisp_eq
        jp      z, eval_quote
        
        ; Check for atom built-in
        ld      de, atom_symbol
        push    bc
        pop     hl
        call    lisp_eq
        jp      z, eval_atom_builtin
        
        ; Check for car built-in
        ld      de, car_symbol
        push    bc
        pop     hl
        call    lisp_eq
        jp      z, eval_car_builtin
        
        ; Check for cdr built-in
        ld      de, cdr_symbol
        push    bc
        pop     hl
        call    lisp_eq
        jp      z, eval_cdr_builtin
        
        ; Check for cons built-in
        ld      de, cons_symbol
        push    bc
        pop     hl
        call    lisp_eq
        jp      z, eval_cons_builtin
        
        ; Check for lambda special form
        ld      de, lambda_symbol
        push    bc
        pop     hl
        call    lisp_eq
        jp      z, eval_lambda
        
        ; Function application - evaluate function and arguments, then apply
        push    ix                      ; HL = full expression
        pop     hl
        call    lisp_car                ; Get function part
        push    iy                      ; DE = environment
        pop     de
        call    eval                    ; Evaluate function
        push    hl                      ; Save evaluated function
        
        push    ix                      ; HL = full expression
        pop     hl
        call    lisp_cdr                ; Get argument list
        call    evlis                   ; Evaluate arguments
        
        pop     bc                      ; BC = evaluated function
        call    apply                   ; Apply function to arguments
        jp      eval_done

eval_quote:
        ; Quote form - return the argument unevaluated
        push    ix                      ; HL = expression
        pop     hl
        call    lisp_cdr                ; Get arguments
        call    lisp_car                ; Get first argument
        jp      eval_done

eval_atom_builtin:
        ; Evaluate (atom expr)
        push    ix                      ; HL = expression
        pop     hl
        call    lisp_cdr                ; Get arguments
        call    lisp_car                ; Get first argument
        push    iy                      ; DE = environment
        pop     de
        call    eval                    ; Evaluate recursively
        call    lisp_atom               ; Test if atom
        jr      z, eval_atom_true
        ld      hl, NIL
        jp      eval_done
eval_atom_true:
        ld      hl, t_symbol
        jp      eval_done

eval_car_builtin:
        ; Evaluate (car expr)
        push    ix
        pop     hl
        call    lisp_cdr
        call    lisp_car
        push    iy
        pop     de
        call    eval
        call    lisp_car
        jp      eval_done

eval_cdr_builtin:
        ; Evaluate (cdr expr)  
        push    ix
        pop     hl
        call    lisp_cdr
        call    lisp_car
        push    iy
        pop     de
        call    eval
        call    lisp_cdr
        jp      eval_done

eval_cons_builtin:
        ; Evaluate (cons expr1 expr2)
        push    ix
        pop     hl
        call    lisp_cdr                ; Get arg list
        push    hl                      ; Save arg list
        call    lisp_car                ; Get first arg
        push    iy
        pop     de
        call    eval                    ; Evaluate first arg
        push    hl                      ; Save result
        
        pop     hl                      ; Get arg list back
        call    lisp_cdr                ; Get second arg
        call    lisp_car
        push    iy
        pop     de
        call    eval                    ; Evaluate second arg
        
        push    hl
        pop     de                  ; DE = second result
        pop     bc                      ; BC = first result
        call    lisp_cons               ; Create cons cell
        jp      eval_done

eval_lambda:
        ; Lambda expressions evaluate to themselves
        push    ix                      ; Return the lambda expression
        pop     hl
        jp      eval_done

eval_done:
        pop     iy
        pop     ix
        pop     bc
        ret

;===============================================================================
; Lambda Support Functions
;===============================================================================

; Evaluate list of expressions (evlis)
; Input: HL = list of unevaluated expressions, IY = environment
; Output: HL = list of evaluated values
evlis:
        push    bc
        push    de
        push    ix
        
        ; Save original list
        push    hl
        pop     ix
        
        ; Check if list is empty
        ld      a, h
        or      l
        jr      z, evlis_empty
        
        ; Evaluate first expression
        call    lisp_car                ; Get first expression
        push    iy                      ; Environment
        pop     de
        call    eval                    ; Evaluate it
        push    hl                      ; Save evaluated first
        
        ; Get rest of list and recursively evaluate
        push    ix                      ; Original list
        pop     hl
        call    lisp_cdr                ; Get rest
        call    evlis                   ; Recursively evaluate rest
        
        ; Cons evaluated first with evaluated rest
        push    hl
        pop     de                  ; DE = evaluated rest
        pop     bc                      ; BC = evaluated first
        call    lisp_cons               ; Create (first . rest)
        jr      evlis_done
        
evlis_empty:
        ld      hl, NIL
        
evlis_done:
        pop     ix
        pop     de
        pop     bc
        ret

; Apply function to arguments
; Input: BC = function (lambda), HL = list of argument values, IY = environment
; Output: HL = result of applying function
apply:
        push    de
        push    ix
        
        ; Save function and arguments
        push    bc                      ; IX = function
        pop     ix
        push    hl                      ; Save argument values
        
        ; Check if function is a lambda
        push    bc
        pop     hl                  ; HL = function
        call    lisp_car                ; Get first element
        ld      de, lambda_symbol
        call    lisp_eq
        jr      nz, apply_error         ; Not a lambda
        
        ; Save current environment
        push    iy
        
        ; Get parameter list
        push    ix                      ; HL = lambda
        pop     hl
        call    lisp_cdr                ; Skip 'lambda'
        call    lisp_car                ; Get parameter list
        push    hl
        pop     de                  ; DE = parameters
        
        ; Get argument values
        pop     hl                      ; HL = argument values
        
        ; Bind parameters to values
        call    pairlis                 ; Create bindings, extend environment
        
        ; Get lambda body
        push    ix                      ; HL = lambda
        pop     hl
        call    lisp_cdr                ; Skip 'lambda'
        call    lisp_cdr                ; Skip parameter list
        push    hl                      ; IX = body list
        pop     ix
        
        ; Evaluate body expressions sequentially
        ld      bc, NIL                 ; Default return value
        
apply_body_loop:
        push    ix                      ; HL = current body
        pop     hl
        ld      a, h
        or      l
        jr      z, apply_body_done      ; No more expressions
        
        call    lisp_car                ; Get current expression
        push    iy                      ; Current environment
        pop     de
        call    eval                    ; Evaluate expression
        push    hl
        pop     bc                  ; Save result
        
        ; Move to next expression in body
        push    ix                      ; HL = current body
        pop     hl
        call    lisp_cdr                ; Get rest of body
        push    hl                      ; Update body pointer
        pop     ix
        jr      apply_body_loop
        
apply_body_done:
        push    bc
        pop     hl                  ; Return last result
        
        ; Restore environment
        pop     iy
        jr      apply_done
        
apply_error:
        pop     hl                      ; Clean up argument values from stack
        ld      hl, NIL                 ; Return NIL on error
        
apply_done:
        pop     ix
        pop     de
        ret

; Pair parameters with values and extend environment
; Input: DE = parameter list, HL = value list, IY = current environment
; Output: IY = extended environment
pairlis:
        push    bc
        push    hl
        
pairlis_loop:
        ; Check if both lists are empty (success)
        ld      a, d
        or      e
        jr      z, pairlis_check_values
        
        ; Check if values list is empty but params remain (error)
        ld      a, h
        or      l
        jr      z, pairlis_error
        
        ; Get parameter and value
        push    de
        push    hl
        
        push    de
        pop     hl                  ; HL = parameter list
        call    lisp_car                ; Get parameter
        push    hl
        pop     bc                  ; BC = parameter
        
        pop     hl                      ; HL = value list
        push    hl
        call    lisp_car                ; Get value
        push    hl
        pop     de                  ; DE = value
        
        ; Create binding (parameter . value)
        call    lisp_cons               ; Create pair
        
        ; Add to environment
        push    hl
        pop     bc                  ; BC = new binding
        push    iy                      ; DE = current environment
        pop     de
        call    lisp_cons               ; Create extended environment
        push    hl                      ; Update environment
        pop     iy
        
        ; Move to next parameter and value
        pop     hl                      ; HL = value list
        call    lisp_cdr                ; Get rest of values
        push    hl                      ; Save rest of values
        
        pop     de                      ; DE = parameter list
        push    de
        pop     hl
        call    lisp_cdr                ; Get rest of parameters
        push    hl
        pop     de
        
        pop     hl                      ; HL = rest of values
        jr      pairlis_loop
        
pairlis_check_values:
        ; Parameters empty, check if values also empty
        ld      a, h
        or      l
        jr      nz, pairlis_error       ; Too many values
        jr      pairlis_done
        
pairlis_error:
        ; Argument count mismatch - keep current environment
        
pairlis_done:
        pop     hl
        pop     bc
        ret

;===============================================================================
; Mark-and-Sweep Garbage Collector
;===============================================================================

; Main garbage collection entry point
collectgarbage:
        push    bc
        push    de
        push    hl
        push    ix
        push    iy
        
        ; Mark all reachable objects from root set
        call    mark_roots
        
        ; Sweep through cell pool and reclaim unmarked cells
        call    sweep_cells
        
        pop     iy
        pop     ix
        pop     hl
        pop     de
        pop     bc
        ret

; Mark all objects reachable from the root set
mark_roots:
        ; Mark global environment
        ld      hl, (global_env)
        call    mark_object
        
        ; Mark current evaluation state (if any)
        ; Note: In a more complete implementation, we'd mark
        ; the current expression, environment, and value being evaluated
        
        ; Mark the entire Lisp stack
        call    mark_lisp_stack
        
        ; Mark the freelist to avoid corruption
        ld      hl, (freelist)
        call    mark_object
        
        ret

; Mark all objects on the Lisp stack
mark_lisp_stack:
        push    bc
        push    hl
        
        ; Get current stack position
        ld      bc, (lispsp)
        ld      hl, lispstack
        
mark_stack_loop:
        ; Check if we've reached the bottom of stack
        push    hl
        and     a
        sbc     hl, bc
        pop     hl
        jr      nc, mark_stack_done     ; BC >= HL, done
        
        ; Mark object at current stack position
        push    bc
        push    bc
        pop     hl
        ld      hl, (hl)                ; Get object at stack position
        call    mark_object
        pop     bc
        
        ; Move to next stack position
        ld      hl, 3
        push    bc
        pop     de
        add     hl, de
        push    hl
        pop     bc
        jr      mark_stack_loop
        
mark_stack_done:
        pop     hl
        pop     bc
        ret

; Mark a single object and all objects reachable from it
; Input: HL = object to mark
mark_object:
        push    bc
        push    de
        push    ix
        
mark_loop:
        ; Check if object is NIL
        ld      a, h
        or      l
        jr      z, mark_done            ; NIL - nothing to mark
        
        ; Check if object is a symbol (don't mark symbols)
        ld      a, l
        and     SYMMASK
        jr      nz, mark_done           ; Symbol - don't mark
        
        ; It's a cons cell - check if already marked
        push    hl                      ; IX = cell address
        pop     ix
        ld      bc, (ix)                ; BC = car value
        ld      a, c
        and     MARKMASK
        jr      nz, mark_done           ; Already marked
        
        ; Mark this cell by setting mark bit in car
        ld      a, c
        or      MARKMASK
        ld      c, a
        ld      (ix), bc                ; Store marked car
        
        ; Recursively mark the car (unmarked original value)
        ld      a, c
        and     255 - MARKMASK          ; Remove mark bit for recursion
        ld      c, a
        push    bc
        pop     hl                  ; HL = unmarked car
        call    mark_object
        
        ; Tail-recursively mark the cdr
        ld      hl, (ix + 3)            ; HL = cdr
        jr      mark_loop               ; Tail recursion
        
mark_done:
        pop     ix
        pop     de
        pop     bc
        ret

; Sweep through all cells, unmarking live ones and adding dead ones to freelist
sweep_cells:
        push    bc
        push    de
        push    hl
        push    ix
        
        ; Initialize new freelist
        ld      hl, NIL
        ld      (freelist), hl
        
        ; Start from beginning of cell pool
        ld      ix, cells
        ld      bc, cells
        ld      hl, CELLMAX * 6         ; Total size of cell pool
        add     hl, bc                  ; HL = end of cell pool
        push    hl
        pop     de                  ; DE = end address
        
sweep_loop:
        ; Check if we've reached end of cell pool
        push    ix
        push    de
        and     a
        sbc     hl, hl
        push    ix
        pop     de
        add     hl, de
        ex      de, hl
        sbc     hl, de
        pop     de
        pop     ix
        jr      nc, sweep_done          ; IX >= end, done
        
        ; Check if this cell is marked
        ld      bc, (ix)                ; BC = car
        ld      a, c
        and     MARKMASK
        jr      z, sweep_add_to_freelist ; Unmarked - add to freelist
        
        ; Cell is marked - unmark it and continue
        ld      a, c
        and     255 - MARKMASK          ; Clear mark bit
        ld      c, a
        ld      (ix), bc                ; Store unmarked car
        jr      sweep_next
        
sweep_add_to_freelist:
        ; Add this cell to freelist
        ld      bc, NIL                 ; Clear car
        ld      (ix), bc
        ld      bc, (freelist)          ; Get current freelist head
        ld      (ix + 3), bc            ; Point this cell to old head
        ld      (freelist), ix          ; Make this cell new head
        
sweep_next:
        ; Move to next cell (6 bytes per cell)
        ld      bc, 6
        push    ix
        pop     hl
        add     hl, bc
        push    hl
        pop     ix
        jr      sweep_loop
        
sweep_done:
        pop     ix
        pop     hl
        pop     de
        pop     bc
        ret

;===============================================================================
; S-Expression Reader
;===============================================================================

; Character classification
iswhite:        ; Check if character in A is whitespace
        cp      ' '
        ret     z                       ; Return Z set if space
        cp      9                       ; Tab
        ret     z                       ; Return Z set if tab
        cp      10                      ; Newline
        ret     z                       ; Return Z set if newline
        cp      13                      ; Carriage return
        ret     z                       ; Return Z set if carriage return
        ; Not whitespace - clear Z flag and return
        or      a                       ; Clear Z flag (NZ condition)
        ret

issym:          ; Check if character in A is valid for symbol
        cp      '('
        jr      z, issym_invalid        ; Not valid
        cp      ')'
        jr      z, issym_invalid        ; Not valid
        cp      '.'
        jr      z, issym_invalid        ; Not valid
        cp      ' '
        jr      c, issym_invalid        ; Control chars not valid
        cp      127
        jr      nc, issym_invalid       ; DEL and above not valid
        ; Valid symbol character - return Z flag
        xor     a                       ; Set Z flag (valid character)
        ret
        
issym_invalid:
        ; Invalid character - return NZ flag
        or      1                       ; Clear Z flag (invalid character)
        ret

; Skip whitespace
skipwhite:
        ; Skip whitespace characters, leaving first non-white in lookbuffer
skip_loop:
        call    getchar                 ; Read character and echo it
        ld      a, (lookbuffer)         ; Get the character
        call    iswhite                 ; Check if it's whitespace
        jr      z, skip_loop            ; If whitespace, read another
        ret                             ; Non-whitespace found, return

; Current input character  
lookbuffer:     db      0               ; Start with null character

; Read S-expression from input
; Output: HL = parsed expression
read_expr:
        call    skipwhite               ; This will call getchar and update lookbuffer
        ld      a, (lookbuffer)         ; Get the character that was read
        
        ; Check for EOF (Ctrl+D = 4)
        cp      4
        jr      z, read_eof
        
        ; Check for opening parenthesis - read list
        cp      '('
        jr      z, read_list
        
        ; Check for invalid characters
        cp      ')'
        jr      z, read_error
        cp      '.'
        jr      z, read_error
        
        ; Must be a symbol
        call    issym
        jr      nz, read_error
        
        ; Read symbol
        call    read_symbol
        ret

read_eof:
        ld      hl, EOF_MARKER
        ret

read_error:
        ld      hl, NIL                 ; Return NIL on error
        ret

read_list:
        ; Read opening parenthesis - already consumed by read_expr
        ; Don't call getchar again, just skip whitespace
        call    skipwhite
        
        ; Check for immediate closing parenthesis
        ld      a, (lookbuffer)
        cp      ')'
        jr      z, read_empty_list
        
        ; Read first element
        call    read_expr
        push    hl                      ; Save first element
        
        ; Create list starting with first element
        push    hl
        pop     bc                  ; BC = first element (car)
        ld      de, NIL                 ; DE = NIL (cdr)
        call    lisp_cons
        push    hl                      ; IX = list start
        pop     ix
        push    hl                      ; IY = list end
        pop     iy
        
read_list_loop:
        call    skipwhite
        ld      a, (lookbuffer)
        
        ; Check for end of list
        cp      ')'
        jr      z, read_list_done
        
        ; Read next element
        call    read_expr
        
        ; Append to list
        push    hl
        pop     bc                  ; BC = new element
        ld      de, NIL                 ; DE = NIL
        call    lisp_cons               ; Create new cons cell
        
        ; Link to end of list
        ld      (iy + 3), hl            ; Set cdr of last cell
        push    hl                      ; Update list end
        pop     iy
        
        jr      read_list_loop

read_list_done:
        call    getchar                 ; Consume ')'
        push    ix                      ; Return list start
        pop     hl
        ret

read_empty_list:
        call    getchar                 ; Consume ')'
        ld      hl, NIL
        ret

read_symbol:
        ; Read symbol into internbuffer
        ld      hl, internbuffer
        ld      bc, 0                   ; Character count
        
read_sym_loop:
        ld      a, (lookbuffer)
        call    issym
        jr      nz, read_sym_done       ; Not a symbol character
        
        ; Check buffer space
        ld      a, b
        or      a
        jr      nz, read_sym_skip       ; Buffer full
        ld      a, c
        cp      INTERNMAX
        jr      nc, read_sym_skip       ; Buffer full
        
        ; Store character
        ld      a, (lookbuffer)
        ld      (hl), a
        inc     hl
        inc     c
        
read_sym_skip:
        call    getchar                 ; Get next character
        jr      read_sym_loop

read_sym_done:
        ; Store length and intern symbol
        ld      (internbufferlen), bc
        call    intern
        ret

;===============================================================================
; Expression Printing (Basic implementation)
;===============================================================================

print_expr:
        push    bc
        
        ; Check if it's NIL
        ld      a, h
        or      l
        jr      z, print_nil
        
        ; Check if it's a symbol
        ld      a, l
        and     SYMMASK
        jr      nz, print_symbol
        
        ; It's a cons cell - print as list
        ld      a, '('
        rst.lil $10
        
        call    print_list_contents
        
        ld      a, ')'
        rst.lil $10
        
        jr      print_expr_done

print_nil:
        ld      hl, nil_str
        call    print_string
        jr      print_expr_done

print_symbol:
        ; TODO: Look up symbol name in symbol table
        ld      hl, symbol_str
        call    print_string
        jr      print_expr_done

print_list_contents:
        ; TODO: Implement proper list printing
        ld      hl, list_str
        call    print_string
        ret

print_expr_done:
        pop     bc
        ret

;===============================================================================
; String Constants and Test Data
;===============================================================================

nil_str:        DB "nil", 0
symbol_str:     DB "symbol", 0
list_str:       DB "list", 0
quote_str:      DB "quote", 0
atom_str:       DB "atom", 0
car_str:        DB "car", 0
cdr_str:        DB "cdr", 0
panic_memory_msg: DB "PANIC: Out of memory!", $0D, $0A, 0

; Built-in symbols (references to obarray entries - filled by init_builtin_symbols)
quote_symbol:   DS 3                            ; "quote" symbol reference
atom_symbol:    DS 3                            ; "atom" symbol reference  
car_symbol:     DS 3                            ; "car" symbol reference
cdr_symbol:     DS 3                            ; "cdr" symbol reference
cons_symbol:    DS 3                            ; "cons" symbol reference
lambda_symbol:  DS 3                            ; "lambda" symbol reference
t_symbol:       DS 3                            ; "t" symbol reference
nil_symbol:     DS 3                            ; "nil" symbol reference

;===============================================================================
; Error Handling and Program Termination
;===============================================================================

panic:  ; Print error message and exit
        ld      hl, error_msg
        call    print_string
        ; Fall through to finish

finish: ; Exit program
        ; Restore original stack pointer
        ld      sp, (original_sp)
        ret                             ; Return to MOS
        
        ; Should not reach here
        halt
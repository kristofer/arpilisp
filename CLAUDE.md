# EZ80 Lisp Interpreter Project - Claude Context File

## Project Overview

This project translates an ARM Raspberry Pi Lisp interpreter (arpilisp.s) to run on the Agon eZ80 platform. The goal is a fully functional interactive Lisp REPL with garbage collection, lambda support, and proper integration with the Agon MOS (Machine Operating System).

## Current Status

### What's Working ✅
- Basic symbol parsing and evaluation (`abc` → `nil`)
- Empty list parsing (`()` → `nil`) 
- Character input/output using MOS API
- Memory initialization (obarray, symbol table)
- Built-in symbol initialization (quote, atom, car, cdr)
- Ctrl-Z immediate exit to MOS
- Clean stack management for MOS return

### Current Issues ❌
- **Tokenizer hangs on symbols**: `abc` and `42` cause hangs in symbol tokenization
- **List parsing incomplete**: `(quote foo)` doesn't work yet
- Need to debug `tokenize_symbol` function

### Architecture Decisions Made

1. **eZ80 ADL Mode**: Using 24-bit addressing throughout
2. **Register Mapping**: ARM registers mapped to eZ80 equivalents
3. **MOS Integration**: Using mos_api.inc for system calls
4. **Tokenizer Approach**: Switched from character-by-character to tokenizer-based parsing
5. **Memory Layout**: Standard Agon layout with MOS header at $40040

## Key Files

- `ez80lisp.s` - Main Lisp interpreter (1800+ lines)
- `mos_api.inc` - MOS API definitions and macros
- `arpilisp.s` - Original ARM source (reference)
- `AgonNotes.md` - eZ80 development lessons learned

## Technical Architecture

### Memory Layout
```
$40000: Program start, jump to _start
$40040: MOS header ("MOS", version, ADL mode flag)
Data structures:
- obarray: Symbol hash table (2000 * 3 bytes)
- cells: Cons cell pool (10000 * 6 bytes)  
- lispstack: Evaluation stack (1000 * 3 bytes)
- token_buffer: Tokenizer buffer (100 * 4 bytes)
- token_data: Token string storage (100 * 50 bytes)
```

### Register Usage (eZ80 mapping from ARM)
```
ARM r0-r3  → eZ80 A, BC, DE, HL (args/return)
ARM r4     → eZ80 IX (general purpose)
ARM r5     → eZ80 IY (lambda args)
ARM r6     → eZ80 BC' (Lisp stack pointer)
ARM r7     → eZ80 DE' (current S-expression)
ARM r8     → eZ80 HL' (S-expression being evaluated)
ARM r9     → eZ80 IX' (environment)
ARM r13-15 → eZ80 SP, handled differently
```

### Tokenizer Design
```
Token Types:
- TOKEN_EOF (0): End of input
- TOKEN_LPAREN (1): (
- TOKEN_RPAREN (2): )
- TOKEN_SYMBOL (3): Identifiers
- TOKEN_NUMBER (4): Numbers (future)
- TOKEN_DOT (5): . (future)

Flow: read_line() → tokenize_line() → parse_tokens() → eval()
```

## Known eZ80 ADL Mode Issues Fixed

### Assembly Syntax (agon-ez80asm v2.0)
- Use `LABEL: EQU value` (not `.equ`)
- Use `assume adl=1` for 24-bit mode
- Use `INCLUDE "file"` (not `.include`)

### Invalid Register Operations Fixed
```assembly
# INVALID → VALID
ld bc, hl     → ld c, l / ld b, h
ld ix, hl     → push hl / pop ix
ld (de), a    → push de / pop ix / ld (ix), a
add de, bc    → Use HL for arithmetic: add hl, bc
```

### Memory Initialization Critical
```assembly
# REQUIRED: Zero all data structures
ld hl, obarray
ld bc, OBARRAYMAX * 3
ld de, obarray + 1
ld (hl), 0
ldir                    # Clear entire array
```

## Current Debugging Focus

### Immediate Issue: Tokenizer Hang
Location: `tokenize_symbol` function around line 527
Problem: Infinite loop or character processing issue
Debug: Added character output in `symbol_length_loop`

### Recently Fixed Issues
1. Character consumption race conditions (switched to tokenizer)
2. Invalid register transfers (using push/pop)
3. Indirect addressing (using IX/IY instead of DE/BC)
4. Uninitialized obarray (causing symbol corruption)
5. EOF marker confusion (using $FFFF instead of NIL)

## Lisp Features Implemented

### Core Functions
- `cons`: Create cons cells with garbage collection
- `car`/`cdr`: Access list elements  
- `atom`: Test for atomic values
- `eq`: Equality testing
- `quote`: Prevent evaluation

### Evaluation Engine
- `eval`: Main evaluation function with special form handling
- `apply`: Function application
- `evlis`: Evaluate argument lists
- `assoc`: Environment lookup

### Memory Management
- Mark-and-sweep garbage collector
- Automatic GC triggering on memory exhaustion
- Free list management for cons cells

### Built-in Special Forms
- `quote`: Return unevaluated argument
- `atom`: Test if argument is atomic
- `car`/`cdr`: List access functions
- `cons`: Create new cons cell
- `lambda`: Create anonymous functions (implemented)

## Development Workflow

### Testing Sequence
1. Test basic symbols: `abc` → `nil`
2. Test empty list: `()` → `nil`
3. Test quoted symbols: `(quote foo)` → `foo`
4. Test simple lists: `(car (quote (a b)))` → `a`
5. Test lambda expressions: `((lambda (x) x) (quote hello))` → `hello`

### Debugging Approach
1. Add character output traces: `ld a, 'X' / rst.lil $10`
2. Use different characters for different code paths
3. Check assembly errors line by line
4. Verify register usage in ADL mode
5. Test incrementally (symbols → lists → evaluation)

## Common Pitfalls to Avoid

1. **Don't use character-by-character parsing** - leads to consumption races
2. **Always initialize data structures** - garbage data causes hangs
3. **Use IX/IY for indirect addressing** - DE/BC don't work in ADL mode
4. **Preserve stack for clean MOS exit** - save/restore original SP
5. **Check assembly syntax carefully** - agon-ez80asm is strict
6. **Use proper MOS API patterns** - follow mos_api.inc conventions

## Next Steps

1. **Fix tokenizer hang** - Debug `symbol_length_loop` infinite loop
2. **Complete list parsing** - Test `(quote foo)` functionality  
3. **Implement print functions** - Show actual symbol names, not "symbol"
4. **Test garbage collection** - Verify mark-and-sweep works
5. **Add more built-ins** - Implement additional Lisp functions
6. **Performance optimization** - Optimize critical paths

## Resources

- **mos_api.inc**: MOS function definitions and system variables
- **AgonNotes.md**: eZ80 development best practices and gotchas
- **Agon documentation**: https://github.com/breakintoprogram/agon-docs
- **eZ80 manual**: Zilog eZ80 CPU User Manual for instruction reference

## Success Criteria

- [ ] Interactive REPL working (`> ` prompt, evaluate expressions)
- [ ] Basic arithmetic and list operations
- [ ] Lambda expressions and function calls
- [ ] Garbage collection under memory pressure
- [ ] Clean exit to MOS (Ctrl-Z)
- [ ] Error handling for malformed input
- [ ] Symbol table persistence across expressions

---

**Key Message for Claude**: This is a complex eZ80 assembly project translating ARM Lisp to Agon platform. Focus on fixing the current tokenizer hang, then work through the testing sequence systematically. Use the lessons learned in AgonNotes.md to avoid previous pitfalls. The architecture is sound - just need to debug the remaining parsing issues.
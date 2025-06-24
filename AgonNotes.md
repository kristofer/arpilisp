# Claude Code Tips: ez80 Assembly for Agon Light2

## Hardware/Platform Specifics

### CPU Modes - CRITICAL
- **ADL Mode (24-bit)**: Default mode, all registers and addresses are 24-bit
- **Z80 Mode (16-bit)**: Compatibility mode, 16-bit operations
- **Mode switching**: Use `.ASSUME ADL=1` or `.ASSUME ADL=0` directives
- **Always check current mode** - many bugs come from wrong assumptions about register width

### Memory Map (Agon Light2)
```
0x000000-0x01FFFF: Flash ROM (128KB)
0x040000-0x0BFFFF: RAM (512KB) 
0x0B0000-0x0BFFFF: MOS variables and buffers
0x040000-0x0AFFFF: User RAM space
```

### Key Hardware Addresses
- **MOS API entry**: RST.LIL 08h (always use .LIL suffix)
- **Video RAM**: Starts at 0x0B2000 (varies by video mode)
- **Stack pointer**: Initialize to top of user RAM (around 0x0AFFFF)

### Register Differences from x86
- **24-bit registers in ADL mode**: HL, DE, BC, IX, IY are all 24-bit
- **No general-purpose registers**: Must use HL, DE, BC creatively
- **Accumulator-centric**: A register is primary, most operations go through A
- **Index registers**: IX, IY are powerful for addressing but slower than HL

## Syntax and Assembler Quirks

### Critical Suffixes
- **RST.LIL 08h**: ALWAYS use .LIL for MOS calls
- **JP.LIL, CALL.LIL**: Use for 24-bit addressing in ADL mode
- **LD.LIL**: For 24-bit loads when in mixed mode

### Addressing Modes
```assembly
LD A, 42        ; Immediate
LD A, (HL)      ; Indirect through HL
LD A, (0x1000)  ; Direct memory
LD HL, label    ; Label addressing (assembler resolves)
```

### Label and Scope Rules
- **Global labels**: Start at column 1, no leading space
- **Local labels**: Start with dot (.loop, .done) - scoped to next global label
- **Case sensitive**: Keep consistent casing throughout

### Data Definitions
```assembly
DB 'Hello',0    ; Byte string with null terminator  
DW 0x1234       ; 16-bit word
DL 0x123456     ; 24-bit long (ez80 specific)
DS 100          ; Reserve 100 bytes
```

## Common Patterns and Idioms

### Function Structure
```assembly
my_function:
    ; Prologue - save registers you'll modify
    PUSH HL
    PUSH DE
    
    ; Function body
    ; ...
    
    ; Epilogue - restore in reverse order
    POP DE
    POP HL
    RET
```

### Parameter Passing Conventions
- **Simple values**: Pass in A, HL, DE registers when possible
- **Complex data**: Pass pointer in HL, data follows
- **Multiple params**: Use stack, but remember ez80 stack grows down
- **Return values**: A for status/small values, HL for pointers/large values

### String Operations Pattern
```assembly
string_copy:
    LD A, (DE)      ; Get source byte
    LD (HL), A      ; Store to destination  
    OR A            ; Test for zero
    RET Z           ; Return if end of string
    INC HL          ; Advance destination
    INC DE          ; Advance source
    JR string_copy  ; Continue
```

### Efficient Loops
```assembly
; Count in BC, process HL pointer
.loop:
    LD A, (HL)      ; Get data
    ; Process A
    INC HL          ; Next item
    DEC BC          ; Count down
    LD A, B         ; Check if BC = 0
    OR C
    JR NZ, .loop    ; Continue if not zero
```

## MOS API Integration

### Standard MOS Call Pattern
```assembly
    LD A, function_number   ; MOS function in A
    ; Set up other registers as needed
    RST.LIL 08h            ; ALWAYS use .LIL suffix
    ; Check return values
```

### Common MOS Functions
- **0x01**: Print character (A = char)
- **0x02**: Print string (HL = pointer to null-terminated string)  
- **0x08**: Get character (returns in A)
- **0x1A**: File open (HL = filename, B = mode)
- **0x1B**: File close (A = handle)
- **0x1C**: File read (A = handle, HL = buffer, BC = bytes)

### Error Handling Pattern
```assembly
    RST.LIL 08h
    OR A                ; Check A for error code
    JR NZ, error_handler ; Non-zero = error
    ; Success path
    JR continue
    
error_handler:
    ; Handle error in A
    RET
    
continue:
    ; Normal execution
```

## Debugging and Development

### Common Pitfalls
- **Forgetting .LIL suffix** on MOS calls - will crash or behave strangely
- **Wrong ADL mode assumptions** - check if registers are 16 or 24-bit
- **Stack imbalance** - every PUSH must have matching POP
- **Uninitialized stack pointer** - set SP early in program
- **Case sensitivity** - labels and mnemonics are case-sensitive

### Register Usage Tracking
```assembly
; Always comment register usage
; Entry: HL = source pointer, DE = dest pointer, BC = count
; Exit:  A = status, HL = end of source, DE = end of dest
; Uses:  AF, BC (modified), HL, DE (advanced)
```

### Memory Safety
- **Always bounds check** - ez80 has no memory protection
- **Initialize variables** - RAM content is random at startup
- **Check buffer overruns** - especially with string operations

## Code Templates

### Minimal Program
```assembly
    .ASSUME ADL=1           ; 24-bit mode
    .ORG 0x040000          ; Start of user RAM

start:
    LD SP, 0x0AFFFF        ; Set stack pointer
    
    ; Your code here
    LD A, 'H'
    RST.LIL 08h            ; Print character
    
    RET                    ; Return to MOS

    .END start
```

### File I/O Template
```assembly
open_file:
    LD HL, filename        ; Filename pointer
    LD B, 1               ; Read mode
    LD A, 0x1A            ; File open
    RST.LIL 08h
    OR A
    RET NZ                ; Return error code
    LD (file_handle), A   ; Save handle
    XOR A                 ; Success
    RET

filename: DB "test.txt", 0
file_handle: DB 0
```

### Interrupt Service Routine
```assembly
my_isr:
    ; Save ALL registers
    PUSH AF
    PUSH BC  
    PUSH DE
    PUSH HL
    PUSH IX
    PUSH IY
    
    ; ISR code here
    
    ; Restore in reverse order
    POP IY
    POP IX
    POP HL
    POP DE
    POP BC
    POP AF
    
    EI                    ; Re-enable interrupts
    RETI                  ; Return from interrupt
```

## Performance Considerations

### Instruction Timing
- **8-bit operations** are fastest (A register)
- **16-bit operations** on HL, DE, BC are efficient  
- **IX, IY operations** are slower - avoid in tight loops
- **Memory access** is slower than register operations

### Optimization Patterns
```assembly
; Fast: Use A register for calculations
LD A, value1
ADD A, value2
LD result, A

; Efficient: HL for 16/24-bit math
LD HL, value1
LD DE, value2  
ADD HL, DE

; Slow: Avoid IX/IY in loops
; Use HL instead and reload IX/IY as needed
```

### Loop Optimization
- **Decrement loops** are faster than increment (test for zero is free)
- **Unroll short loops** when possible
- **Keep loop bodies small** - better cache behavior

## Assembly Best Practices

### Code Organization
```assembly
; Constants first
BUFFER_SIZE: EQU 256
MOS_PRINT:   EQU 0x01

; Variables in RAM
    .ORG 0x040000
buffer:     DS BUFFER_SIZE
counter:    DW 0

; Code section  
    .ORG 0x040100
start:
    ; Program starts here
```

### Documentation Standards
- **Function headers**: Document entry/exit conditions and register usage
- **Inline comments**: Explain non-obvious operations
- **Block comments**: Describe algorithm purpose
- **Register maps**: Document what each register contains during complex operations

---

## Additional eZ80 ADL Mode Issues (from real project experience)

### Assembly Syntax Specific to agon-ez80asm v2.0

**Critical syntax requirements:**
- Use `LABEL: EQU value` format (not `.equ` or `equ`)
- Use `assume adl=1` for 24-bit addressing mode
- Use `INCLUDE "filename"` (not `.include`)
- Use `ORG address` (not `.org`)
- Use `DB`, `DW`, `DS` (not `.db`, `.dw`, `.ds`)

### Invalid Register Operations in ADL Mode
```assembly
; INVALID - Cannot transfer between 16-bit register pairs directly
LD BC, HL        ; Causes "operand not matching mnemonic"
LD IX, HL        ; Invalid instruction

; VALID alternatives
LD C, L          ; Transfer byte by byte
LD B, H
; OR
PUSH HL          ; Use stack for transfer
POP IX
```

### Indirect Addressing Restrictions
```assembly
; INVALID - DE/BC cannot be used for indirect addressing in ADL mode
LD (DE), A       ; "operand not matching mnemonic"
LD (BC), HL      ; Invalid
LD HL, (DE)      ; Invalid

; VALID - Use IX/IY for indirect addressing
PUSH DE          ; Transfer DE to IX
POP IX
LD (IX), A       ; Now valid
LD (IX + 1), H   ; With offset
```

### Arithmetic Operation Limitations
```assembly
; INVALID - Limited register combinations for ADD
ADD DE, BC       ; Not supported
ADD IX, HL       ; Not supported

; VALID - HL is the primary arithmetic register
ADD HL, BC       ; Supported
ADD HL, DE       ; Supported

; WORKAROUND for other combinations
PUSH DE          ; Transfer to HL for arithmetic
POP HL
ADD HL, BC       ; Do the addition
PUSH HL          ; Transfer back if needed
POP DE
```

### Jump Distance Issues
```assembly
; ERROR: "Relative jump too large" for distances > ±126 bytes
JR far_label     ; May fail

; SOLUTION: Use absolute jumps for long distances
JP far_label     ; Always works
```

### Memory Initialization Critical Issues

**CRITICAL**: Always initialize data structures to zero! Garbage data causes unpredictable hangs and crashes.
```assembly
; Initialize symbol table/arrays
LD HL, obarray
LD BC, OBARRAYMAX * 3
LD DE, obarray + 1
LD (HL), 0       ; Zero first byte
LDIR             ; Copy zero to entire array
```

### MOS API Integration Specifics

**Using mos_api.inc properly:**
```assembly
INCLUDE "mos_api.inc"

; Use MOSCALL macro instead of direct RST calls
MOSCALL mos_getkey              ; Get keyboard input
MOSCALL mos_sysvars             ; Get system variables

; Check system variables for keyboard state
LD A, (IX + sysvar_vkeycode)    ; Virtual key code
OR A                            ; Zero = normal key
JR NZ, skip_virtual_key         ; Skip virtual keys

LD A, (IX + sysvar_keyascii)    ; Get ASCII code
```

### Character-by-Character Parsing Problems

**Major issue**: Multiple functions consuming the same input characters leads to hangs.

**Solution**: Use tokenizer approach instead:
1. Read complete line into buffer
2. Tokenize entire line into array of tokens
3. Parse tokens (no character consumption timing issues)

```assembly
; Tokenizer structure avoids character consumption bugs
read_expression:
    CALL read_line              ; Read complete line
    CALL tokenize_line          ; Create token array
    CALL parse_tokens           ; Parse without character issues
```

### Common Hang/Crash Causes from Real Project

1. **Uninitialized obarray/symbol table** - Causes invalid symbol references
2. **Character consumption race conditions** - Multiple functions calling getchar
3. **Invalid register transfers** - Silent failures leading to corrupted pointers
4. **Missing stack preservation in exit functions** - Need to restore original SP
5. **EOF marker confusion** - Using NIL as EOF causes normal NIL results to exit

### Stack Management for Clean Exit
```assembly
; Save original stack pointer for clean MOS return
original_sp:    DS 3

_start:
    LD (original_sp), SP    ; Save MOS stack pointer
    ; ... program code ...

finish:
    LD SP, (original_sp)    ; Restore original stack
    RET                     ; Clean return to MOS
```

### Debugging Techniques for Complex Issues
```assembly
; Simple character tracing for debugging hangs
LD A, 'X'           ; Use different characters for different code paths
RST.LIL $10         ; Output to console

; Trace token processing
LD A, '@'           ; Mark entry to functions
RST.LIL $10
```

## Quick Reference Checklist

Before running any code, verify:
- [ ] Stack pointer initialized (LD SP, 0x0AFFFF)
- [ ] ADL mode set correctly (.ASSUME ADL=1)
- [ ] MOS calls use .LIL suffix (RST.LIL 08h)
- [ ] All PUSHes have matching POPs
- [ ] Variables initialized if needed
- [ ] Buffer bounds checked
- [ ] Error handling for MOS calls implemented
- [ ] **All data structures initialized to zero**
- [ ] **No direct register pair transfers (use push/pop)**
- [ ] **No indirect addressing with DE/BC (use IX/IY)**
- [ ] **Character input handling doesn't have consumption races**

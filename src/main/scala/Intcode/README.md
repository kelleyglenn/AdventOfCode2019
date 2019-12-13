# Intcode Assembler

## Description
Intcode machine language is defined as a sequence of large (potentially Long) integers. The operations and their behavior are described in days 2, 5, 7 and 9.

Here, we have defined an assembly language to specify an Intcode program. The Assembler will take the source code as a sequence of instructions (possibly read from a file) and assemble the machine language from it. That machine language can then be fed to an Intcode computer.

## Supported Op Codes
* ADD  Op(1)  - 2 read, 1 write parameters
* MULT Op(2)  - 2 read, 1 write parameters
* IN   Op(3)  - 1 write parameter
* OUT  Op(4)  - 1 read parameter
* JNEZ Op(5)  - 2 read parameters
* JEQZ Op(6)  - 2 read parameters
* LESS Op(7)  - 2 read, 1 write parameters
* EQ   Op(8)  - 2 read, 1 write parameters
* INCB Op(9)  - 1 read parameter
* END  Op(99) - 0 parameters
* NOP0 Op(0)  - 0 parameters
* NOP1 Op(0)  - 1 parameter
* NOP2 Op(0)  - 2 parameters
* NOP3 Op(0)  - 3 parameters

Note: The "NOPx" op codes are not formally defined in the source material.
They are implied, however, because some programs have op code positions with an initial value of 0 (In a properly-written program, they are overwritten at runtime before they are evaluated as op codes.)

## Supported Modes
* Positional (mode 0) - denoted with a '@' before the parameter value
* Immediate  (mode 1) - the default mode in this assembly language
* Relative   (mode 2) - augments mode 0 by also adding "+B" after the parameter value

## Labels
Any position (op codes or parameters) within the program can be marked with a label that can be referenced by any parameter in the program.

To define a label, use ':' followed by the label name immediately after the op code or parameter.
The label will resolve to the index of that position.

To dereference a label as a parameter value, simply use the label name. Labels can be used in any mode, just as an integer can.

Note: Labels are also not defined in the source material, but may be a useful device when writing assembly code by hand.

## Comments
Comments start with ';' and go until the end of the line. Lines with operations can end with a comment.
Lines without operations must have a comment. Blank lines are not allowed.
Comments have no effect on the output machine language.

## Line Positions
All lines with op codes (not comment-only lines) must begin with their index position, followed by ':'.
The first position is 0. All subsequent lines must begin with the index position of the previous line, plus total number of values expressed in the previous line.
The total number of values equals 1 for the op code plus 1 for each parameter. 

This is primarily to assist with writing and reading of the assembly code, though the assembler will validate correctness.

## Example
    ; This example results in the following machine code
    ; 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,100,0,6,101,0,99
     0: INCB:begin 1       ; this label is at position 0, so it is given then value 0
     2: OUT @-1+B          ; this parameter evaluates to the value at position (-1 + the relative base)
     4: ADD @100 1 @100
     8: EQ @100 16 @101
    12: JEQZ @100, begin   ; this label revolves to an immediate parameter with the value 0
    15: JEQZ @101, @begin  ; this label resolves to a positional parameter with the value 0
    18: END

## EBNF
    letter      = "a".."z" | "A".."Z" ;    
    digit       = "0".."9" ;  
    ws          = " " | "\x09" ;
    symbol      = "!".."/" | ":".."@" | "[".."`" | "{".."~" ;
    unsigned    = digit { digit } ;
    bytePos     = { ws } unsigned ":" { ws } ;
    integer     = [ "-" ] unsigned ;
    label       = ( letter | "_" ) { letter | digit | "_" } ;
    defineLabel = ":" label ;
    immediate   = ( label | integer ) ;
    positional  = "@" ( immediate ) ;
    relative    = positional "+B" ;
    paramSepar  = { ws } ( ws | "," ) { ws } ;
    readParam   = paramSepar ( immediate | positional | relative ) [ defineLabel ] ;
    writeParam  = paramSepar ( positional | relative ) [ defineLabel ] ;
    OPw0param   = ("NOP0" | "END") [ defineLabel ] ;
    OPw1read    = ("NOP1" | "OUT" | "INCB") [ defineLabel ] readParam ;
    OPw1write   = "IN" [ defineLabel ] writeParam ;
    OPw2read    = ("NOP2" | "JNEZ" | "JEQZ") [ defineLabel ] readParam readParam;
    OPw3read    = "NOP3" [ defineLabel ] readParam readParam ;
    OPw2rd1wr   = ("ADD" | "MULT" | "LESS" | "EQ") [ defineLabel ] readParam readParam writeParam ;
    instruction = OPw0param | OPw1read | OPw1write | OPw2read | OPw3read | OPw2rd1wr ;
    comment     = ";" { letter | digit | ws | symbol } ;
    line        = (bytePos instruction [{ ws } comment]) | comment ;

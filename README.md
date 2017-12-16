# coreWars
## Haskell assignment (CS4012)
### Introduction
*"Core War is a game in which programs written in a specialised assembly language (called
Redcode) are executed on a simulated computer. Each programs object is to disable all other
programs running on the simulator without itself becoming disabled."*

### Assignment checklist:
> 1. Define a data type to represent Redcode programs, and a parser that can parse a program from a text file. ✔️
> 2. Provide a multi-threaded implementation of the MARS virtual machine. Give each Redcode program it's own thread. _Sorta_
>       + Implemented Mars Virtual Machine ✔️
>       + Give each Redcode program a thread ✔️
>       + Have working instructions with addressing modes ❌
>            +  ADD, MOV, SUB instructions work, only for certain addressing modes. J, JMP etc need to be further developed.
> 3. Equip your MARS simulator with an interface that allows the user to see the progress of the various Redcode programs. _Sorta_
>   +Very simple user interface.

### Design Rational:
#### Parser:
I chose to implement by making the addressing modes and opcodes and instructions datatypes, whilst making the field and programs type variables. The rest of the functions in the RedCode file are simple functions for parsing the lines of the program.txt files into the embedded DSL.

#### Mars Memory:
The circular memory was originally hardcoded to 8000 lines, although for testing purposes I trimmed this down to 70 lines. The *circular* memory was accomplished by by modular division. ie: 'mod' <size of memory>.

The main idea of the program running was to assign a thread to an executeProgram function call. The function would take a program counter and the shared Mars memory VM, as a reference to a TVar. Using a transactional variable allowed for the atomic read and writing between threads.
So if I have five programs loaded in memory, 1000 spaces apart, I can pass in a program counter of their locations in memory,
and forkIO a thread for each call.


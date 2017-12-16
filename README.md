# coreWars
## Haskell assignment (CS4012)
### Introduction
*"Core War is a game in which programs written in a specialised assembly language (called
redcode) are executed on a simulated computer. Each programs object is to disable all other
programs running on the simulator without itself becoming disabled."*

### Assingment checklist:
 > 1. Define a data type to represent Redcode programs, and a parser that can parse a program from a text file. ✔️
 > 2. Provide a multi-threaded implementation of the MARS virtual machine. Give each Redcode program it's own thread. _Sorta_
 >       + Implemented Mars Virtual Machine ✔️
 >       + Give each redcode program a thread ✔️
 >       + Have working instructions with addressing modes ❌
 >             +  ADD, MOV, SUB instructions work, only for certain addressing modes. J, JMP etc need to be further developed.
 > 3. Equip your MARS simulator with an interface that allows the user to see the progress of the various Redcode programs. _Sorta_
 >   +Very simple user interface.

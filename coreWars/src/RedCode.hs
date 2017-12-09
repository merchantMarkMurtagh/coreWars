module RedCode where 

data Instruction = Identity
                 | Opcode Mode, Mode
                 | Opcode Nothing Mode
                 | Opcode Mode Nothing
                   deriving(Show, Read)
 

type Mode = "#" ++ Integer
           |"@" ++ Integer
           | ">" ++ Integer
           | Integer
           | Nothing
             deriving(Show, Read)

data Opcode = Identity
            | MOV 
            | DAT
            | ADD 
            | SUB 
            | JMP
            | JMZ 
            | JMN 
            | DJN 
            | CMP 
            | SPL
              deriving(Show, Read)
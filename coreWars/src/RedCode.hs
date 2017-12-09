module RedCode where 
import System.IO 

data AddrMode =  Direct
               | Indirect
               | Immediate
               | AutoDec
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

data Instruction = Opcode (Maybe AddrInt) (Maybe AddrInt) 

type AddrInt = (AddrMode, Integer)

type Program = [Instruction]

addInstruction :: Instruction ->  [Instruction] -> [Instruction]
addInstruction instr x = instr:x 

readProgramFile = do
                handle <- openFile "src/program.txt" ReadMode  
                contents <- hGetContents handle  
                putStr contents  
                hClose handle  


makeProgram
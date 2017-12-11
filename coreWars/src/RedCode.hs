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

type Instruction = (Opcode, AddrInt, AddrInt) 

type AddrInt = (AddrMode, Integer)

type Program = [Instruction]

addInstruction :: Instruction ->  [Instruction] -> [Instruction]
addInstruction instr x = instr:x 

readProgramFile = do
                withFile "src/program.txt" ReadMode (\handle -> do  
                    contents <- hGetContents handle 
                    let x = lines contents

                    putStr contents) 


makeProgram :: [String] -> Program
makeProgram x = [makeInstruction x]
makeProgram x:xs = makeInstruction x:(makeProgram xs)


makeInstruction :: [String] -> Instruction
makeInstruction [a, b, c] = (op, (aField), (bField))
                                where op = getOp a
                                      aField = makeAddrInt (getAddrMode $ splitAddr b) (getL $ splitAddr b)
                                      bField = makeAddrInt (getAddrMode $ splitAddr c) (getL $ splitAddr c)


getOp :: String -> Opcode
getOp "MOV" = MOV
getOp "DAT" = DAT
getOp "ADD" = ADD
getOp "SUB" = SUB
getOp "JMP" = JMP
getOp "JMZ" = JMZ
getOp "JMN" = JMN
getOp "DJN" = DJN
getOp "CMP" = CMP
getOp "SPL" = SPL

getAddrMode :: [String] -> AddrMode
getAddrMode ["#", _ ] = Immediate
getAddrMode ["@", _ ] = Indirect
getAddrMode ["<", _ ] = AutoDec
getAddrMode ["", _ ] = Direct

makeAddrInt :: AddrMode -> Integer -> AddrInt
makeAddrInt a b =  (a, b)

getL :: [String] -> Integer
getL [_, b, _] = (read b :: Integer)
getL [_, b] = (read b :: Integer)

getMode :: [String] -> String
getMode [a, _, _] = a
getMode [a, _] = a

splitAddr :: String -> [String]
splitAddr s = words s












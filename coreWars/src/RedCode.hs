module RedCode where 
import System.IO 

data AddrMode =  Direct
               | Indirect
               | Immediate
               | AutoDec
               | Error
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

type Instruction = (Opcode, Maybe Field, Maybe Field) 

type Field = (AddrMode, Integer)

type Program = [Instruction] 

addInstruction :: Instruction ->  [Instruction] -> [Instruction]
addInstruction instr x = instr:x 

readProgramFile = do
                withFile "src/program.txt" ReadMode (\handle -> do  
                    contents <- hGetContents handle 
                    let x = fmap words $ lines contents
                    let a = makeProgram x
                    putStr $ show a) 


makeProgram :: [[String]] -> Program
makeProgram a = fmap makeInstruction a

makeInstruction :: [String] -> Instruction 
makeInstruction [a, b, c] = (op, (aField), (bField))
                                where op = getOp a
                                      aField = makeAddrInt (getAddrMode $ getAddr $  b) (getNum $ noComma b)
                                      bField = makeAddrInt (getAddrMode $ getAddr $  c) (getNum $ noComma c)

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

getAddrMode :: Char -> AddrMode
getAddrMode '#' = Immediate
getAddrMode '@' = Indirect
getAddrMode '<' = AutoDec
getAddrMode (_)= Direct

makeAddrInt :: AddrMode -> Integer -> Maybe Field
makeAddrInt a b = Just (a, b)

getNum :: String -> Integer
getNum (x:xs) = read xs 
getNum x = read x 

getAddr :: String -> Char
getAddr (x:xs) = x

noComma :: String -> String
noComma a = filter (\c -> c /= ',') a














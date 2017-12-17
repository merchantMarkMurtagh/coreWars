module RedCode where 
import System.IO 
import System.Random

-- Embedded DSL for describing a RedCode instruction. 
-- Addressing modes and opcodes are described using data types.
-- Instruction is also a data type, and can be represented three differetn ways. The reason for the three different fields is to distiguish 
-- whether an instruction is a regular two fielded instruction ie: MOV #4, 3, or a one field, like a DAT or JMP instruction.
-- Different field specified using the different constructors... N ('Normal'), D ('DAT')  or J ('JMP').

data AddrMode =  Direct
               | Indirect
               | Immediate
               | AutoDec
               | Error
                 deriving(Show, Read, Eq)

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
              deriving(Show, Read, Eq)

data Instruction =   N Opcode Field Field
                   | J Opcode Field
                   | D Opcode Field 
                   deriving(Show, Read)


-- Field field is a tuple of an addressing mode and an int.
type Field = (AddrMode, Int)

-- Program type is a of list of instructions
type Program = [Instruction] 

-- Programs type is a list of 'Programs' 
type Programs = [Program]


-- ** Functions for taking string representations of functions and turning them into instructions using the DSL above. **

{- 
  Read a file at the given path, initially intended to be used to read several programs from the one file, however changed to just read in 
  different programs from different 'txt' files.
-}
readPrograms :: String -> IO Program
readPrograms path  = readProgramFile path

-- read file, break into lines, fmap words over $ lines contents to get a list of the form x = [[String]].
-- Pass to makeProgram
readProgramFile :: String -> IO Program
readProgramFile path = do
                    contents <- readFile path 
                    let x = fmap words $ lines contents
                    return $ (makeProgram x)
                     
-- Map make instruction over every entry of a list.
makeProgram :: [[String]] -> Program
makeProgram a = fmap makeInstruction a

-- Pattern match for different types of instructions
makeInstruction :: [String] -> Instruction 
makeInstruction ["JMP", b] = (J op aField)
                                where op = getOp "JMP"
                                      aField = (makeAddrInt (getAddrMode $ getAddr b) (getNum $ noComma b))
makeInstruction ["DAT", b] = (D op aField)
                                where op = getOp "DAT"
                                      aField = (makeAddrInt (getAddrMode $ getAddr b) (getNum $ noComma b))
makeInstruction [a, b, c] = (N op aField bField)
                                where op = getOp a
                                      aField = (makeAddrInt (getAddrMode $ getAddr b) (getNum $ noComma b))
                                      bField = (makeAddrInt (getAddrMode $ getAddr c) (getNum $ noComma c))

-- *** Functions for isolating specific desired variables of read in lines ***

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

makeAddrInt :: AddrMode -> Int -> Field
makeAddrInt a b = (a, b)

getNum :: String -> Int
getNum (x:xs) 
            | length (x:xs) == 1 = read (x:xs)
            | otherwise = read xs

getAddr :: String -> Char
getAddr (x:xs) = x

noComma :: String -> String
noComma a = filter (\c -> c /= ',') a


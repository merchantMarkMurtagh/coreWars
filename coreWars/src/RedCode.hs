module RedCode where 
import System.IO 
import System.Random

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

type Field = (AddrMode, Int)

type Program = [Instruction] 

type Programs = [Program]

readPrograms :: String -> IO Program
readPrograms path  = readProgramFile path


readProgramFile :: String -> IO Program
readProgramFile path = do
                    contents <- readFile path 
                    let x = fmap words $ lines contents
                    return $ (makeProgram x)
                     

makeProgram :: [[String]] -> Program
makeProgram a = fmap makeInstruction a

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

-- noComments :: String -> String
-- noComments str = head $ splitOn ";" str

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


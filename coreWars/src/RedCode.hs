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

data Instruction =   N Opcode Field Field
                   | J Opcode Field
                   | D Opcode Field 
                   deriving(Show, Read)

type Field = (AddrMode, Integer)

type Program = [Instruction] 


readProgramFile = do
                withFile "src/program.txt" ReadMode (\handle -> do  
                    contents <- hGetContents handle 
                    let x = fmap words $ lines contents
                    let a = makeProgram x
                    putStr $ show a
                    ) 


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

getAddrMode :: Char -> AddrMode
getAddrMode '#' = Immediate
getAddrMode '@' = Indirect
getAddrMode '<' = AutoDec
getAddrMode (_)= Direct

makeAddrInt :: AddrMode -> Integer -> Field
makeAddrInt a b = (a, b)

getNum :: String -> Integer
getNum (_:xs) = read xs 
getNum (x) = read x  --!!!!!!!!!!!!!!!!!!!!!!! doesn't work for plain integers

getAddr :: String -> Char
getAddr (x:xs) = x

noComma :: String -> String
noComma a = filter (\c -> c /= ',') a

-- getConstructor :: String -> Constructor
-- getConstructor "JMP" = J
-- getConstructor "DAT" = D
-- getConstructor _ = N










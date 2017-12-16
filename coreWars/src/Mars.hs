module Mars where
import Control.Concurrent
import RedCode
import Prelude hiding (lookup)
import System.Random
import Data.List.Index
import Data.Map
import Data.Maybe
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Debug.Trace
import Debug.Trace 

type MapM = Map Int Instruction
type Memory = TVar (MapM) 

makeMemory :: [Instruction]
makeMemory = replicate 70 (makeInstruction ["DAT", "0"])

makeTvar :: MapM -> IO Memory
makeTvar m = atomically (newTVar (m)) 

-- makeMemmory
makeM :: Program -> MapM
makeM prog = fromList $ indexed prog

--make prog a map
makeProgramM ::  Program -> Int -> MapM 
makeProgramM p posFac = fromList $ changePos posFac $ indexed p 

-- take program, make it a map, union with memory
insertProgram :: MapM -> Program -> Int -> MapM
insertProgram mem prog posFac = union x mem
                              where x = makeProgramM prog posFac

changePos :: Int -> [(Int, a)] -> [(Int, a)]
changePos a l= fmap (\(x,y) -> (x+a*10, y)) l

-- Executing the program --------------------------------------------------------

runProg :: Memory -> Programs -> IO [ThreadId]
runProg mem progs = do
  mapM (\(i,x) -> (forkIO $ executeProgram (i*10) mem)) (zip [1..] progs)



-- execute instruction, program count++, excute instruction

executeProgram :: Int -> Memory -> IO ()	 
executeProgram pCount sharedV = do
	                   
	                   (nMem, instr)<- atomically $ executeInstruction pCount sharedV --need to be done atomically
	                   case getOpType (instr) of 
	                   	"DAT" -> do id <- myThreadId
	                   	            putStrLn $ "Killin " ++ (show id)
	                   	            killThread id
	                   	_     -> do id <- myThreadId
	                   	            putStrLn $ (show id) ++ " is performing " ++ (show instr)
	                   	            threadDelay 1000000
	                   	            let nCount = pCount+1
	                   	            executeProgram nCount nMem
	                   	            return ()

executeInstruction :: Int -> Memory -> STM (Memory, Instruction)                  -- get instruction, execute, return
executeInstruction count sharedV = do 
	                      mem <- readTVar sharedV
	                      let instr = getInstr count mem
	                      let newMem = doInstruction count (instr) mem
	                      writeTVar sharedV newMem
	                      return (sharedV, instr)


doInstruction :: Int -> Instruction -> MapM -> MapM
doInstruction pCount (N op aField bField) mem 
                                     | op == MOV && getAMF(aField) == Immediate = (insert offset (newVal) mem)
                                      where offset = calculateOffset pCount (getAddrF bField)
                                            prevInst = fromJust $ lookup (getAddrF bField) mem
                                            newVal = (changeTo DAT $ changeBField prevInst $ getAddrF aField)
doInstruction pCount (N op aField bField) mem 
                                     | op == MOV = (insert offset (newVal) mem)
                                      where offset = calculateOffset pCount (getAddrF bField)
                                            newVal = fromJust $ lookup (getAddrF aField) mem
doInstruction pCount (N op aField bField) mem
                                     | op == ADD = (insert offset (newInstr) mem)
                                       where offset = calculateOffset pCount (getAddrF bField)
                                             val1 = getAddrF $ getAField $ fromJust $ lookup (getAddrF aField) mem
                                             val2 = getAddrF $ getBField $ fromJust $ lookup (getAddrF bField) mem
                                             newVal = val1 + val2
                                             oldInstr = fromJust $ lookup (getAddrF bField) mem
                                             newInstr = changeBField oldInstr newVal

doInstruction pCount (N op aField bField) mem
                                      | op == SUB = insert offset (newInstr) mem
                                       where offset = calculateOffset pCount (getAddrF bField)
                                             val1 = getAddrF $ getAField $ fromJust $ lookup (getAddrF aField) mem
                                             val2 = getAddrF $ getBField $ fromJust $ lookup (getAddrF bField) mem
                                             newVal = val1 - val2
                                             oldInstr = fromJust $ lookup (getAddrF bField) mem
                                             newInstr = changeBField oldInstr newVal
doInstruction pCount _ mem = mem

-- doInstruction pCount (J op aField) mem
--                                       | op == JMP = excuteInstruction (getAddrF aField) mem
-- doInstruction pCount (N JMN aField (a,x)) mem
--                                       | x > 0 = excuteInstruction (getAddrF aField) mem
-- doInstruction pCount (N JMN aField (a,0)) mem  = excuteInstruction (pc) mem
--                                                where pc = pCount + 1


calculateOffset :: Int -> Int -> Int
calculateOffset pCounter val = (pCounter + val) `mod` 8000 

getInstr :: Int -> MapM ->  Instruction --Maybe Instruction
getInstr i mem = fromJust $ lookup i mem

getAddrF :: Field -> Int
getAddrF (a, i) = i

getAMF :: Field -> AddrMode
getAMF (a, i) = a

changeBField :: Instruction -> Int -> Instruction
changeBField (N op a b) val = (N op a (getAMF b, val)) 
changeBField (D op a) val = (D op (getAMF a, val))

getOpType :: Instruction -> String
getOpType (N MOV a b ) = "MOV"
getOpType (N ADD a b ) = "ADD"
getOpType (N SUB a b ) = "SUB"
getOpType (D DAT a ) = "DAT"


changeTo :: Opcode -> Instruction -> Instruction
changeTo DAT (N op aField bField) = (D DAT aField) 
changeTo DAT (D op aField) = (D op aField)

getAField :: Instruction -> Field
getAField (N op aField bField) = aField
getAField (D op aField) = aField

getBField :: Instruction -> Field
getBField (N op aField bField) = bField
getBField (D op aField) = aField












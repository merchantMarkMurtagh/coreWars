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
makeMemory = replicate 100 (makeInstruction ["DAT", "0"])

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
  mapM (\(i,x) -> (forkIO $ executeProgram (i*10) mem)) (zip [1..] progs)  -- t1 execute 1000 memory, t2 execute 2000 memory

executeProgram :: Int -> Memory -> IO ()                        -- excute 1000 mem, execute 1001 mem, execute 1002 mem ...
executeProgram a sharedV = do
	                   print $ show a
	                   mem <- atomically (readTVar sharedV)
	                   let memNew = executeInstruction a mem
	                   print $ show memNew
	                   atomically (writeTVar sharedV memNew)


executeInstruction :: Int -> MapM -> MapM                    -- get instruction, execute, count ++, get instruction execute
executeInstruction count mem =  executeInstruction a $ doInstruction count instr mem
								where a = count+1
								      instr = getInstr count mem
excuteInstruction count mem 
                           | test == "DAT" = mem
                           where test = getOpType $ getInstr count mem


doInstruction :: Int -> Instruction -> MapM -> MapM
doInstruction pCount (N op aField bField) mem 
                                     | op == MOV && getAMF(aField) == Immediate = insert offset (newVal) mem
                                      where offset = calculateOffset pCount (getAddrF bField)
                                            prevInst = fromJust $ lookup (getAddrF bField) mem
                                            newVal = (changeTo DAT $ changeBField prevInst $ getAddrF aField)
doInstruction pCount (N op aField bField) mem 
                                     | op == MOV = insert offset (newVal) mem
                                      where offset = calculateOffset pCount (getAddrF bField)
                                            newVal = fromJust $ lookup (getAddrF aField) mem
doInstruction pCount (N op aField bField) mem
                                     | op == ADD = insert offset (newInstr) mem
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
doInstruction pCount (J op aField) mem
                                      | op == JMP = excuteInstruction (getAddrF aField) mem
doInstruction pCount (N JMN aField (a,x)) mem
                                      | x > 0 = excuteInstruction (getAddrF aField) mem
doInstruction pCount (N JMN aField (a,0)) mem  = excuteInstruction (pc) mem
                                               where pc = pCount + 1


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
getOpType (D DAT a ) = "DAT"

changeTo :: Opcode -> Instruction -> Instruction
changeTo DAT (N op aField bField) = (D DAT aField) 
changeTo DAT (D op aField) = (D op aField)

getAField :: Instruction -> Field
getAField (N op aField bField) = aField

getBField :: Instruction -> Field
getBField (N op aField bField) = bField












module Mars where
import Control.Concurrent
import RedCode
import Prelude hiding (lookup)
import System.Random
import Data.List.Index
import System.Console.ANSI
import Data.Map
import Data.Maybe
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Debug.Trace
import Data.Hashable

{- 
  
  The shared "mars memory virtual machine" is used using a map. Each entry is an instruction. This makes it easier to index into 'memory'
  and change a desired field/entry.
  This is wrapped in a TVar, so only one thread could change the shared memory at any given time. 
 
 -}

type MapM = Map Int Instruction
type Memory = TVar (MapM) 

-- Initialise memory to 70 lines of 'DAT #0' instructions
makeMemory :: [Instruction]
makeMemory = replicate 70 (makeInstruction ["DAT", "0"])

-- put memory (Map) into a transactional variable.
makeTvar :: MapM -> IO Memory
makeTvar m = atomically (newTVar (m)) 

-- Takes in a program and turns it into a Map
makeM :: Program -> MapM
makeM prog = fromList $ indexed prog

-- Change spawning postion of program
makeProgramM ::  Program -> Int -> MapM 
makeProgramM p posFac = fromList $ changePos posFac $ indexed p 

-- Take program, make it a map, union with memory
insertProgram :: MapM -> Program -> Int -> MapM
insertProgram mem prog posFac = union x mem
                              where x = makeProgramM prog posFac

-- Actual implementation of changing position in memory
changePos :: Int -> [(Int, a)] -> [(Int, a)]
changePos a l= fmap (\(x,y) -> (x+a*10, y)) l

-- **** Executing the program ****

-- Fork a thread for each call to execute program. So every program is run with a seperate thread.
runProg :: Memory -> Programs -> IO [ThreadId]
runProg mem progs = do
  mapM (\(i,x) -> (forkIO $ executeProgram (i*10) mem)) (zip [1..] progs)


-- Take the program counter and the shared memory. Flow : Execute instruction at PC -> PC++ execute next instruction.
-- Kill Thread if DAT instruction is encountered. 
executeProgram :: Int -> Memory -> IO ()	 
executeProgram pCount sharedV = do
	                   
	                   (nMem, instr)<- atomically $ executeInstruction pCount sharedV --need to be done atomically
	                   case getOpType (instr) of 
	                   	"DAT" -> do id <- myThreadId
	                   	            putStrLn $  (show id) ++ " Died! :O"
	                   	            killThread id
	                   	_     -> do id <- myThreadId
	                   	            putStrLn $ (show id) ++ " is performing " ++ (show instr)
	                   	            threadDelay 2000000
	                   	            let nCount = pCount+1
	                   	            executeProgram nCount nMem
	                   	            return ()

-- Seperate function to allow atomic read, perfrom action, write back.
-- Execute instruction and return the shared TVar, along with the executed instruction for UI purposed.
executeInstruction :: Int -> Memory -> STM (Memory, Instruction)                  
executeInstruction count sharedV = do 
	                      mem <- readTVar sharedV
	                      let instr = getInstr count mem
	                      let newMem = doInstruction count (instr) mem
	                      writeTVar sharedV newMem
	                      return (sharedV, instr)


-- Actually perfrom augmentation to shared memory
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




{-
  Function for calculating offset of current instruction, relative to current program counter.
  Mod result by size of memory to allow for circular memory effect
-}
calculateOffset :: Int -> Int -> Int
calculateOffset pCounter val = (pCounter + val) `mod` 70 

-- Get an instruction from memory given a program counter
getInstr :: Int -> MapM ->  Instruction --Maybe Instruction
getInstr i mem = fromJust $ lookup i mem

-- ** Functions for isolating desired variables from inststruction fields
getAddrF :: Field -> Int
getAddrF (a, i) = i

getAMF :: Field -> AddrMode
getAMF (a, i) = a

getAField :: Instruction -> Field
getAField (N op aField bField) = aField
getAField (D op aField) = aField

getBField :: Instruction -> Field
getBField (N op aField bField) = bField
getBField (D op aField) = aField


-- Change the bField or a filed of an instrucntion. To be used for MOV and ADD operations
changeBField :: Instruction -> Int -> Instruction
changeBField (N op a b) val = (N op a (getAMF b, val)) 
changeBField (D op a) val = (D op (getAMF a, val))

getOpType :: Instruction -> String
getOpType (N MOV a b ) = "MOV"
getOpType (N ADD a b ) = "ADD"
getOpType (N SUB a b ) = "SUB"
getOpType (D DAT a ) = "DAT"


-- For placing a DAT in memory, MOV instruction
changeTo :: Opcode -> Instruction -> Instruction
changeTo DAT (N op aField bField) = (D DAT aField) 
changeTo DAT (D op aField) = (D op aField)

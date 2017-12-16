module Main where
import RedCode
import System.IO
import Data.Text.Lazy hiding (zip)
import Mars
import RedCode
import Control.Concurrent
import RedCode
import Prelude hiding (lookup)
import System.Random
import Data.List.Index
import Data.Map
import Data.Maybe
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

main = do 
    x <- readPrograms "src/program.txt"
    y <- readPrograms "src/program2.txt"
    let mem = makeM makeMemory
    let z = unions [makeM x, makeM y] -- Union of all progs as map
    let mem2 = insertProgram (mem) x 1
    let mem3 = insertProgram (mem2) y 3
    let progs = [x, y]
    sharedM <- makeTvar mem3
    ts <- runProg sharedM progs
    after <- atomically $ readTVar sharedM
    print $ show after
    --print $ show after


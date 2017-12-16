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
    xx <- readPrograms "src/program3.txt"
    yy <- readPrograms "src/program3.txt"
    xxx <- readPrograms "src/program5.txt"
    let mem = makeM makeMemory
    let z = unions [makeM x, makeM y, makeM xx, makeM yy, makeM xxx] -- Union of all progs as map
    let mem2 = insertProgram (mem) x 1
    let mem3 = insertProgram (mem2) y 2
    let mem4 = insertProgram (mem3) xx 3
    let mem5 = insertProgram (mem3) yy 4
    let memF = insertProgram (mem3) xxx 5
    let progs = [x, y, xx, yy, xxx]
    sharedM <- makeTvar memF
    ts <- runProg sharedM progs
    threadDelay 20000000
    after <- atomically $ readTVar sharedM
    print $ show after
    --print $ show afters


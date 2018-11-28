-- author = Christian Ouwehand 
-- January 2015

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent
import Control.Monad
import Data.Array.IArray
import Data.List as L
import Data.Set as S hiding (map)
import System.Console.GetOpt
import System.Environment
import System.Process


type Cell     = (Integer, Integer)
type World    = Set Cell
type ViewPort = (Integer, Integer, Integer, Integer)
data Options  = Options { level    :: FilePath
                        , alive    :: Char
                        , dead     :: Char
                        , interval :: Int
                        , viewport :: ViewPort
                        , clean    :: Bool
                        , help     :: Bool}


defaultOptions  = Options
  { level    = "simple.level"
  , alive    = 'x'
  , dead     = ' '
  , interval = 100
  , viewport = (0,0,20,20)
  , clean    = True
  , help     = False}

-- EXERCISE
-- Implement the function nextGeneration that computes the next iteration of the World
-- according to the rules of Game of Life.
-- You may wish to use an auxiliary function for computing dead cells.

-- | Algorithm implementation
nextGeneration :: World -> World
nextGeneration w = undefined

-- | Helper functions
-- deadCells :: World -> World
-- deadCells w = undefined

neighborCountIn :: Cell -> World -> Int
neighborCountIn c w = size
                    $ intersection w
                    $ neighbors c


neighbors :: Cell -> World
neighbors l@(x,y) = fromList
                  $ L.filter (/=l)
                  $ (,) <$> r x <*> r y
  where r x = [x-1 .. x+1]


-- | IO functions
showWorld :: Options -> World -> String
showWorld (Options _ a d _ (xs,ys,xe,ye) _ _) w =
    unlines $ map showCell <$> visibleCells
  where visibleCells = row <$> [ys..ye]
        row y = (\x -> (x,y)) <$> [xs..xe]
        showCell c = if c `member` w then a else d


readWorldFromFile :: FilePath -> IO World
readWorldFromFile f        = readFile f >>= (return . fromList . readLines 0 . lines)
  where readLines y []     = []
        readLines y (h:t)  = readLine 0 y h ++ readLines (y+1) t
        readLine x y []    = []
        readLine x y (h:t) = let next = readLine (x+1) y t
                             in  if h == 'x' then (x,y):next else next


playGame :: Options -> World -> IO ()
playGame o@(Options _ _ _ i _ c _) world =
    printWorld world >> threadDelay (i*1000) >> clear >> playGame o world'
  where printWorld          = putStrLn . (showWorld o)
        clear               = if c then system "clear" >> return () else return ()
        world'              = nextGeneration world


-- | Main and option parsing
main :: IO ()
main = do
  args  <- getArgs
  let usage = usageInfo usageHeader options
  case parseOptions args of
    Right opts -> if help opts
                  then putStrLn usage
                  else readWorldFromFile (level opts) >>= playGame opts
    Left  err  -> putStrLn $ err ++ usage
  where usageHeader = "Usage: gameoflife [OPTIONS...]"


parseOptions :: [String] -> Either String Options
parseOptions args =
  case getOpt Permute options args of
     (o,_,[]) -> Right $ L.foldl (flip id) defaultOptions o
     (_,_,e)  -> Left  $ concat e


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"]
      (NoArg (\o -> o {help = True}))
      "print this help screen"
  , Option ['f','l'] ["file","level"]
      (ReqArg (\f o -> o {level = f}) "FILE")
      "level file to load"
  , Option ['s'] ["speed"]
      (ReqArg ((\s o -> o {interval = s}) . read) "SPEED")
      "game speed in ms"
  , Option ['a'] ["alive"]
      (ReqArg ((\c o -> o {alive = c}) . head) "CHAR")
      "character used for living cells"
  , Option ['d'] ["dead"]
      (ReqArg ((\c o -> o {dead = c}) . head) "CHAR")
      "character used for dead cells"
  , Option ['v'] ["viewport"]
      (ReqArg ((\v o -> o {viewport = v}) . read) "(xs,ys,xe,ye)")
      "define the viewport (rectangle) to view to world, from (xs,xe) to (ys,ye)"
  , Option ['c'] ["clean"]
      (NoArg (\o -> o {clean = not (clean defaultOptions)}))
      "toggle cleaning of the window, between frames (uses the *nix clean command)"
  ]

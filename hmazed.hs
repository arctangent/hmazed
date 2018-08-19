-- Simple maze generator in Haskell
-- Jacob Conrad Martin
-- http://jacobconradmartin.com

import System.Random
import Debug.Trace
import Control.Monad.State

data Location    = Location { x::Int, y::Int } deriving (Eq)
data Path        = Path { from::Location, to::Location } deriving (Eq)
data Cell        = Cell { location::Location, neighbours::[Location] } deriving (Eq,Show)
data Globals     = Globals { width::Int, height::Int, start::Location, end::Location, begin::Location, r::[Int] } deriving (Show)
data Maze        = Maze { cells::[Cell], paths::[Path], stack::[Location], visited::[Location], counter::Int  } deriving (Show)
data Display     = Display { rows::[DisplayRow] }
data DisplayRow  = DisplayRow { cols::[DisplayCol] }
data DisplayCol  = DisplayCol { displayLocation::Location, element::ElementType }
data ElementType = Wall | Space | Start | End

instance Show Display where
    show d = "\n" ++ (show $ rows d) ++ "\n"
instance Show DisplayRow where
    show r = "\n" ++ (show $ cols r)
instance Show DisplayCol where
    show d = (show $ element d)
instance Show ElementType where
    show Wall  = "#"
    show Space = " "
    show Start = "S"
    show End   = "E"
instance Show Location where
    show loc = "(" ++ (show $ x loc) ++ "," ++ (show $ y loc) ++ ")"
instance Show Path where
    show path = (show $ from path) ++ "->" ++ (show $ to path)
    

initialiseMaze :: Globals -> Maze
initialiseMaze g = Maze { cells = c, paths = [], stack = [begin g], visited = [begin g], counter = 1 }
    where c = [ defineCell g x y | x <- [1..width g], y <- [1..height g] ]

    
generateDisplay :: Globals -> Maze -> Display
generateDisplay g m = Display [ getDisplayRow g m c | c <- [1..(1 + 2 * width g)] ]


getDisplayRow :: Globals -> Maze -> Int -> DisplayRow
getDisplayRow g m c = DisplayRow [ getDisplayCol g m c r | r <- [1..(1 + 2 * height g)] ]


getDisplayCol :: Globals -> Maze -> Int -> Int -> DisplayCol
getDisplayCol g m c r
    | isStart      = DisplayCol (Location c r) Start
    | isEnd        = DisplayCol (Location c r) End
    | isCell       = DisplayCol (Location c r) Space
    | isWall       = DisplayCol (Location c r) Wall
    | isEdge       = DisplayCol (Location c r) Wall
    | isPath       = DisplayCol (Location c r) Space
    | otherwise    = DisplayCol (Location c r) Wall
    where isStart  = (c `div` 2 == x (start g)) && (r `div` 2 == y (start g)) && isCell
          isEnd    = (c `div` 2 == x (end g)) && (r `div` 2 == y (end g)) && isCell
          isCell   = (c `mod` 2 == 0) && (r `mod` 2 == 0)
          isWall   = (c `mod` 2 == 1) && (r `mod` 2 == 1)
          isEdge   = (c == 1) || (r == 1) || (c == 1 + 2 * width g) || (r == 1 + 2 * height g)
          isPath   = checkPaths g m c r

     
checkPaths :: Globals -> Maze -> Int -> Int -> Bool
checkPaths g m c r
    -- | trace ((show c) ++ " " ++ (show r)) False = undefined
    | (c `mod` 2 == 1) && (r `mod` 2 == 0)  = ( (Path left right) `elem` (paths m)) || ( (Path right left) `elem` (paths m))
    | (c `mod` 2 == 0) && (r `mod` 2 == 1)  = ( (Path up down) `elem` (paths m)) || ( (Path down up) `elem` (paths m))
    | otherwise                             = False
    where up        = Location (c `div` 2) ((r-1) `div` 2)
          down      = Location (c `div` 2) ((r+1) `div` 2)
          left      = Location ((c-1) `div` 2) (r `div` 2)
          right     = Location ((c+1) `div` 2) (r `div` 2)


unvisitedNeighbour :: Globals -> Maze -> Cell -> Location
unvisitedNeighbour g m c = (unvisited) !! ourSpecialInt
    where unvisited = [ z | z <- neighbours c, z `notElem` (visited m) ]
          ourSpecialInt = ((r g) !! (counter m)) `mod` (length unvisited)


generateMaze :: Globals -> Maze -> Maze
generateMaze g m
    | (stack m) == [] = error "Stack empty... This should never happen!" 
    | allCellsVisited = m    -- We have visited all the cells and so we are now done
    | needToPopStack  = generateMaze g $ Maze { cells = cells m, paths = paths m, stack = tail $ stack m, visited = visited m, counter = counter m }
    | otherwise       = generateMaze g $ Maze { cells = cells m, paths = newPath : paths m, stack = newCellLocation : stack m, visited = newCellLocation : visited m, counter = (counter m)+1}
    where allCellsVisited = (width g) * (height  g) == (length $ visited m)
          needToPopStack  = [ z | z <- neighbours currentCell, z `notElem` (visited m) ] == []
          currentCell     = defineCell g (x topOfStack) (y topOfStack)
          newCellLocation = unvisitedNeighbour g m currentCell
          newPath         = Path topOfStack newCellLocation
          topOfStack      = head $ stack m


-- Define a cell
defineCell :: Globals -> Int -> Int -> Cell
defineCell g x y = Cell { location = loc, neighbours = n }
    where loc = Location x y
          n   = getNeighbours g x y

-- Get a list of the locations adjacent to a particular cell
getNeighbours :: Globals -> Int -> Int -> [Location]
getNeighbours g x y = [ location c | c <- candidateCells, inMaze g c ]
    where candidateCells = up ++ down ++ left ++ right
          up         = [defineCell g (x+1) y]
          down       = [defineCell g (x-1) y]
          left       = [defineCell g x (y-1)]
          right      = [defineCell g x (y+1)]


-- Test whether a candidate cell is in the maze
inMaze :: Globals -> Cell -> Bool
inMaze g c = ((location c) `elem` validLocations)
    where validLocations    = [ Location x y | x <- [1..(width g)], y <- [1..(height g)] ]






main = do
    seed <- getStdGen
    let width  = 40
    let height = 40
    let randomInts = randomRs (0,3) seed :: [Int]
    let start = Location (width `div` 2) 1
    let end   = Location (width `div` 2) height
    putStrLn (show start ++ " " ++ show end)
    let begin = end -- Location (width `div` 2) (height `div` 2)
    let g = Globals width height start end begin randomInts
    let initialMaze = initialiseMaze g
    let finalMaze = generateMaze g initialMaze
    -- putStrLn "\nHERE ARE THE PATHS:"
    -- putStrLn (show $ reverse $ paths finalMaze)
    
    
    let display = generateDisplay g finalMaze
    putStrLn "\nHERE IS THE MAZE:"
    putStrLn (show display)
    

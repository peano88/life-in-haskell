import System.IO 
import System.Environment
import System.Process
import Control.Monad
import Control.Concurrent

atSafe :: [a] -> Int -> Maybe a
atSafe [] _ = Nothing
atSafe (x:xs) 0 = Just x
atSafe (x:xs) n
    | n < 0 = Nothing
    | otherwise = atSafe xs (n - 1)

atSafe2d :: [[a]] -> Int -> Int -> Maybe (Maybe a)
atSafe2d xxs i j =  atSafe <$> atSafe xxs i <*> Just j

at :: [[a]] -> Int -> Int -> Maybe a
at xxs i j =
    case atSafe2d xxs i j of    Nothing -> Nothing
                                Just Nothing -> Nothing
                                Just (Just a) -> Just a  

neighbours :: [[a]] -> Int -> Int -> [Maybe a]
neighbours xxs i j =
    let
        above = [ at xxs (i-1) (j-1) , at xxs (i-1) j, at xxs (i-1) (j+1) ]
        sameRow = [ at xxs i (j-1), at xxs i (j+1) ]
        below = [ at xxs (i+1) (j-1) , at xxs (i+1) j, at xxs (i+1) (j+1) ]
    in above ++ sameRow ++ below

data Cell = Alive | Dead deriving (Eq, Read)

instance Show Cell where
        show Alive = "*"
        show Dead = "."

countCellsAlive :: [Maybe Cell] -> Int
countCellsAlive cs =  foldr ((+) . toInt)  0 cs
    where toInt Nothing = 0
          toInt (Just Dead) = 0
          toInt (Just Alive) = 1  

nextStateCell :: [[Cell]] -> Int -> Int -> Cell
nextStateCell xxs i j
    | at xxs i j == Just Alive && (aliveAround == 2 || aliveAround == 3) = Alive
    | at xxs i j == Just Dead && (aliveAround == 3) = Alive
    | otherwise = Dead
    where
        aliveAround = countCellsAlive $ neighbours xxs i j

coordinates :: [[a]] -> [[(a, Int, Int)]]
coordinates xxs = map expandRow (expandTable xxs)
    where   expandTable xxs = zipWith (,) xxs [0..]
            expandRow (xs,z) = zipWith (\x y -> (x,y,z)) xs [0..]

mapTable :: (a -> b) -> [[a]] -> [[b]]
mapTable f xxs = map (map f) xxs

nextState :: [[Cell]] -> [[Cell]]
nextState xxs = mapTable (\(_, x, y) -> nextStateCell xxs x y) (coordinates xxs)


-- Read state from file
readStateFromFile :: String -> IO [[Cell]]
readStateFromFile filename = do
        contents <- readFile filename
        let     cellsString = map words $ lines contents
        return (mapTable (\w -> read w :: Cell) cellsString)

-- Display Cells panel
displayPanel :: [[Cell]] -> String
displayPanel xxs = foldl (\res row -> res ++ "\n" ++ show row) "" xxs

-- Main
main = do
        args <- getArgs
        cells <- readStateFromFile $ head args 
        putStrLn $ displayPanel cells
        forever $ do
                        threadDelay 1000000
                        System.Process.callCommand "clear"
                        putStrLn $ displayPanel $ nextState cells
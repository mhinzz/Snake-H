module Snake where
-- namespace qualifier -> here it is R
import qualified System.Random as R
import System.IO
import System.Console.ANSI

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Data.Monoid ((<>))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (takeDirectory)

type Position = (Int, Int)
type Snake = [Position]

-- data Direction = Up | Dn | Rt | Lf | SO deriving (Show, Eq)
data Direction = Up | Dn | Rt | Lf deriving (Show, Eq)

data Command = Go Direction deriving (Show, Eq)

data GameState = Playing State | GameOver deriving (Show)

-- Define the state object
data State = State {
    snake     :: Snake,
    food      :: Position,
    direction :: Direction,
    rand      :: R.StdGen,
    limits    :: (Int, Int),
    score     :: Int
} deriving (Show)

takeUntilAfter :: Monad m => (a -> Bool) -> Pipe a a m ()
takeUntilAfter p = do
    v <- await
    yield v
    if p v then return () else takeUntilAfter p

deltas :: Monad m => Pipe a (a,a) m ()
deltas = do
    first <- await
    P.scan remember (first, first) id
    where
        remember (_, a) b = (a, b)

transitions game =
    P.scan nextState game id
    >-> P.map toGameState
    >-> takeUntilAfter isGameOver
    >-> deltas
    where
        isGameOver GameOver = True
        isGameOver (Playing _) = False

-- Sets the rate of the game taking an int as a multiplyer which is default 1
rateLimit :: Int -> Pipe b b IO ()
rateLimit t = forever $ do
    lift $ threadDelay (t * 250000)
    await >>= yield

-- Prevent reversal of direction
opposite :: Direction -> Direction
opposite d = case d of
    Up -> Dn
    Dn -> Up
    Rt -> Lf
    Lf -> Rt
    -- SO -> SO

-- Take the current directon of position of the head of the snake and return the new position 
move :: Direction -> Position -> Position
move d (row, col) = case d of
    Up -> (row - 1, col)
    Dn -> (row + 1, col)
    Rt -> (row, col + 1)
    Lf -> (row, col - 1)
    -- SO -> (row, col)

moveSnake s d = (move d $ head s):(init s)

-- Add the position of the food to the length of the snake 
eat :: Snake -> Direction -> Snake
eat snake dir = (move dir $ head snake):snake

-- Return random position that is not the snake
randFreePosition :: R.RandomGen g => (Int, Int) -> g -> Snake -> (Position, g)
randFreePosition lim g s =
    head $ dropWhile inSnake (randPositions g)
    where
        inSnake (x, _) = x `elem` s
        randPositions h = r:randPositions g'
            where r@(_, g') = randPosition lim h

-- return a random position on the board
randPosition :: R.RandomGen g => (Int, Int) -> g -> (Position, g)
randPosition (maxr, maxc) g =
    let (row, g1) = R.randomR (1, maxr) g
        (col, g2) = R.randomR (1, maxc) g1
    in ((row, col), g2)

-- Take the state and direction of the snake and determin the next state
nextState :: State -> Direction -> State
nextState state newDir
    | newDir == opposite (direction state) = state
    -- | newDir == SO = startState
    | (move newDir $ head $ snake state) == (food state) = eaten
    | otherwise = movedSnake
    where
        movedSnake = state { 
            snake = moveSnake (snake state) newDir,
            direction = newDir
        }
        eaten = state {
            snake = eat (snake state) newDir,
            direction = newDir,
            food = newFood,
            rand = newRand,
            score = (score state)+1
        }
        (newFood, newRand) = randFreePosition (limits state) (rand state) $ snake eaten

-- determin if the game given the current state should be over 
    -- if the head of the snake is in the body of the snake
    -- if the head of the snake is outside the play area
toGameState :: State -> GameState
toGameState state
    | collision $ snake state = GameOver
    | any (outside $ limits state) (snake state) = GameOver
    | otherwise = Playing state
    where
        collision (x:xs) = any (== x) (tail xs)
        outside (maxr, maxc) (row, col) =
            row < 1 || row > maxr || col < 1 || col > maxc

-- Determine the next direction for the snake
getDirections :: Producer Direction IO ()
getDirections =
    getCommands
    >-> P.map fromCommand
    >-> removeOpposites
    where fromCommand (Go x) = x

-- Always grab the first char input by the user and parse it into an action
getCommands :: Producer Command IO ()
getCommands = forever $ do
    c <- lift getChar
    case parseCommand c of
        Nothing -> return ()
        Just x -> yield x

-- Read command and run the appropriate command
parseCommand :: Char -> Maybe Command
parseCommand c = case c of
    'w' -> Just $ Go Up
    'a' -> Just $ Go Lf
    's' -> Just $ Go Dn
    'd' -> Just $ Go Rt
    -- 'c' -> Just $ Go SO
    _   -> Nothing

-- Remove the chance of reversing direction
removeOpposites :: Monad m => Pipe Direction Direction m r
removeOpposites = do
    first <- await
    yield first
    loop first
    where
        loop prev = do
            next <- await
            if next == opposite prev
            then loop prev
            else yield next >> loop next

-- Set up the start screen
startScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen

-- Initialize the state to start the game
startState = State { 
    snake = [(x, 21) | x <- [15..19]],
    food = (11, 21),
    direction = Up,
    rand = R.mkStdGen 0,
    limits = (21, 41),
    score = 0
}

-- Create the border of the play area 
drawBorder :: State -> IO ()
drawBorder state = do
    let (row, col) = limits state
    mapM_ (draw '\9608') [(0, x)     | x <- [0..col+1]]
    mapM_ (draw '\9608') [(row+1, x) | x <- [0..col+1]]
    mapM_ (draw '\9608') [(x, 0)     | x <- [0..row+1]]
    mapM_ (draw '\9608') [(x, col+1) | x <- [0..row+1]]

    let scr = " Score: 000"
    setCursorPosition 0 (col+2)
    putStrLn scr

    let hscr = " High Score: 000"
    setCursorPosition 1 (col+2)
    putStrLn hscr
    
    highscr <- readFile "Scores.txt"
    let highScore = read highscr :: Int
    setCursorPosition 1 (col+2 + (length hscr) - (length highscr))
    putStrLn highscr

    setCursorPosition (row+2) 0

-- Take the old and new gamestates and and draw the new output
drawUpdate :: (GameState, GameState) -> IO ()
drawUpdate (Playing old, Playing new) = do 
    clearState old
    drawState new
    highscr <- readFile "Scores.txt"
    let 
        highScore = read highscr :: Int
        (row, col) = limits new
    if score old >= highScore then do
        let scoreHStr = show (score new)
        setCursorPosition 1 (col + 18 - length scoreHStr)
        putStrLn scoreHStr
        writeFile "Scores.txt" scoreHStr
    else return ()
    let scoreStr = show (score new)
    setCursorPosition 0 (col + 13 - length scoreStr)
    putStrLn scoreStr
    setCursorPosition (row+2) 0

drawUpdate (Playing state, GameOver) = do
    highscr <- readFile "Scores.txt"
    let 
        highScore = read highscr :: Int
        (row, col) = limits state
    if score state >= highScore then do 
        let scoreHStr = show (score state)
        setCursorPosition 1 (col + 18 - length scoreHStr)
        putStrLn scoreHStr
        writeFile "Scores.txt" scoreHStr
    else return ()
    let text = "Game Over"
    setCursorPosition ((row `div` 2) + 0) (((col - length text) `div` 2) + 1)
    putStrLn text
    let text = "Press any key then Enter to start over."
    setCursorPosition ((row `div` 2) + 1) (((col - length text) `div` 2) + 1)
    putStrLn text
    let text = "Or press Ctrl + C to close."
    setCursorPosition ((row `div` 2) + 2) (((col - length text) `div` 2) + 1)
    putStrLn text
    setCursorPosition (row+2) 0
    showCursor

-- Set the characters for the snake and food
drawState  = renderState '\9608' '\9632'
-- Set the characters to clear the game area
clearState = renderState ' ' ' '

-- draw the state given either the draw characters or the cleared characters
renderState :: Char -> Char -> State -> IO ()
renderState snk fud state = do
    draw fud (food state)
    mapM_ (draw snk) (reverse $ snake state)
    cursorBackward 1

-- Put the given character at the given position
draw :: Char -> Position -> IO ()
draw char (row, col) = do
    setCursorPosition row col
    putChar char

-- Create High Score file if it does not exist
initializeScore :: FilePath -> String -> IO ()
initializeScore path content = do
    createDirectoryIfMissing False $ takeDirectory path
    existornot <- (doesFileExist path)
    if existornot then
        return ()
    else writeFile path content 

-- Main
main :: IO (Async (), ())
main = do
    setTitle "Snake"
    hideCursor
    startScreen
    initializeScore "Scores.txt" "0"
    drawBorder startState
    drawState startState
    let startDir = direction startState
        run p = async $ runEffect p >> performGC
        from = fromInput
        to = toOutput

    (mO, mI) <- spawn unbounded
    (dO, dI) <- spawn $ latest startDir

    inputTask <- run $ getDirections >-> to (mO <> dO)
    delayedTask <- run $ from dI >-> rateLimit 1 >-> to mO
    drawingTask <- run $ for
        (from mI >-> transitions startState)
        (lift . drawUpdate)

    waitAny [inputTask, drawingTask]

-- to start the game, load Snake.hs and type 'go'
go :: IO ()
go =
    do
        startScreen
        setCursorPosition 0 0
        putStrLn("\n\nWelcome to Snake! \n\n-- Control the snake's direction with your keyboard: \n 'w' for up \n 'a' for left \n 's' for down \n 'd' for right  \n-- Gain points by directing the snake to food items (squares) that appear on the board. \n-- The game ends when your snake runs into itself, the other snake, or the edge of the board.  \n\nTo start the game, press Enter.")
        goHelp

goHelp :: IO ()
goHelp = do 
    line <- getLine
    
    main
    goHelp
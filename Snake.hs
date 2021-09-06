module Snake where

    import Control.Concurrent (threadDelay)
    import System.Console.Terminfo (Terminal, LinesAffected, TermOutput, runTermOutput, setupTermFromEnv, getCapability, clearScreen)
    import System.IO ( hReady, stdout, BufferMode (NoBuffering, BlockBuffering), stdin, hSetBuffering, hFlush, hWaitForInput, hSetEcho )

    -- Model

    data Point = Point {
        x :: Int,
        y :: Int
    } deriving (Show)

    data Direction = Up | Right | Down | Left deriving (Show)

    data Snake = Snake {
        parts :: [Point],
        direction :: Direction
    } deriving (Show)

    data Game = Game {
        snake :: Snake,
        food :: Point
    } deriving (Show)

    -- Constants

    frameDelay :: Int
    frameDelay = 100000

    clearLinesAffected :: Int
    clearLinesAffected = 20

    arenaWidth :: Int
    arenaWidth = 20

    arenaHeight :: Int
    arenaHeight = 20

    wallChar :: Char
    wallChar = ':'

    snakeChar :: Char
    snakeChar = '▅'

    startingSnake :: Snake
    startingSnake = Snake {
        parts = [
            Point {x = 10, y = 8},
            Point {x = 10, y = 9},
            Point {x = 10, y = 10}
        ],
        direction = Down
    }

    startingGameState :: Game
    startingGameState = Game { snake = startingSnake, food = Point {x = 0, y = 0} }

    -- Logic

    main = do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout (BlockBuffering (Just $ (arenaWidth+3) * (arenaHeight+2) - 1))
        hSetEcho stdout False
        startGame

    startGame :: IO ()
    startGame = do
        terminal <- setupTermFromEnv
        case getCapability terminal clearScreen of
            Nothing ->
                putStrLn "ERROR: Cannot find clear screen terminal capability"
            Just clr -> do
                loop terminal (clr clearLinesAffected) startingGameState

    loop :: Terminal -> TermOutput -> Game -> IO ()
    loop terminal clr gameState = do
        threadDelay frameDelay
        runTermOutput terminal clr
        draw gameState
        hFlush stdout
        c <- getLastChar
        case c of
            'q' -> do putStrLn "Bye"
            _   -> do loop terminal clr (nextGameState gameState c)

    getLastChar :: IO Char
    getLastChar = do
        isInput <- hWaitForInput stdin 1
        if isInput then do
            c1 <- getChar
            c2 <- getLastChar
            if c2 == ' ' then return c1 else return c2
        else do
            return ' '

    draw :: Game -> IO ()
    draw gameState = do
        drawPixel gameState 0 0
        print gameState

    drawPixel :: Game -> Int -> Int -> IO ()
    drawPixel gameStatus x y
        | x == arenaWidth + 1 && y == arenaHeight + 1 = do
            putStr $ wallChar:"\n"
            return ()
        | x == arenaWidth + 1 = do
            putStr $ wallChar:"\n"
            drawPixel gameStatus 0 (y + 1)
        | x == 0 || y == 0 || y == arenaHeight + 1 = do
            putChar wallChar
            drawPixel gameStatus (x + 1) y
        | otherwise = do
            putChar $ if any (\Point {x = pX, y = pY} -> pX == x && pY == y) parts then snakeChar else ' '
            drawPixel gameStatus (x + 1) y
        where Game { snake = Snake { parts = parts, direction = Down }, food = food } = gameStatus

    nextGameState :: Game -> Char -> Game
    nextGameState Game { snake = Snake { parts = (Point { x = headX, y = headY }):t, direction = Down }, food = food } input =
        Game { snake = Snake { parts = (Point {x = headX, y = (headY + 1) `mod` arenaHeight}):(Point {x = headX, y = headY}):init t, direction = Down }, food = food }

    -- Probabily something to use for random
    -- import System.CPUTime (getCPUTime)
    -- time <- liftIO getCPUTime
    -- outputStrLn $ "CPUTIme" ++ show time

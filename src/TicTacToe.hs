module TicTacToe where

import Graphics.Gloss.Interface.Pure.Game
import Data.List (transpose)

-- | Запустить игру «Крестики-нолики».
runTicTacToe :: IO ()
runTicTacToe = do
  play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Flappy Lambda" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Фишки игроков.
data Mark = X | O
  deriving (Eq, Show)

-- | Клетка игрового поля.
type Cell = Maybe Mark

-- | Игровое поле.
type Board = [[Cell]]

-- | Состояние игры.
data Game = Game
  { gameBoard  :: Board       -- ^ Игровое поле.
  , gamePlayer :: Mark        -- ^ Чей ход?
  , gameWinner :: Maybe Mark  -- ^ Победитель.
  }

-- | Начальное состояние игры.
-- Игровое поле — пусто.
-- Первый игрок ходит за крестики.
initGame :: Game
initGame = Game
  { gameBoard  = replicate boardHeight (replicate boardWidth Nothing)
  , gamePlayer = X
  , gameWinner = Nothing
  }

-- | Отобразить игровое поле.
drawGame :: Game -> Picture
drawGame game = translate (-w) (-h) (scale c c (pictures
  [ drawGrid
  , drawBoard (gameWinner game) (gameBoard game)
  ]))
  where
    c = fromIntegral cellSize
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Сетка игрового поля.
drawGrid :: Picture
drawGrid = color white (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [1..m - 1]
    vs = map (\i -> line [(i, 0), (i, m)]) [1..n - 1]

    n = fromIntegral boardWidth
    m = fromIntegral boardHeight

-- | Нарисовать фишки на игровом поле.
drawBoard :: Maybe Mark -> Board -> Picture
drawBoard win board = pictures (map pictures drawCells)
  where
    drawCells = map drawRow (zip [0..] board)
    drawRow (j, row) = map drawCellAt (zip [0..] row)
      where
        drawCellAt (i, cell) = translate (0.5 + i) (0.5 + j) (drawCell win cell)

-- | Нарисовать фишку в клетке поля (если она там есть).
drawCell :: Maybe Mark -> Cell -> Picture
drawCell _ Nothing       = blank
drawCell win (Just mark) = color markColor (drawMark mark)
  where
    markColor
      | win == Just mark = light orange
      | otherwise        = white

-- | Нарисовать фишку.
drawMark :: Mark -> Picture
drawMark X = drawX
drawMark O = drawO

-- | Нарисовать «крестик».
drawX :: Picture
drawX = pictures
  [ polygon [(-0.4,  0.3), (-0.3,  0.4), ( 0.4, -0.3), ( 0.3, -0.4)]
  , polygon [(-0.4, -0.3), (-0.3, -0.4), ( 0.4,  0.3), ( 0.3,  0.4)] ]

-- | Нарисовать «нолик».
drawO :: Picture
drawO = thickCircle 0.3 0.1

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) _ _ mouse) = placeMark (mouseToCell mouse)
handleGame _ = id

-- | Поставить фишку и сменить игрока (если возможно).
placeMark :: (Int, Int) -> Game -> Game
placeMark (i, j) game =
  case gameWinner game of
    Just _ -> game    -- если есть победитель, то поставить фишку нельзя
    Nothing -> case modifyAt j (modifyAt i place) (gameBoard game) of
      Nothing -> game -- если поставить фишку нельзя, ничего не изменится
      Just newBoard -> game
        { gameBoard  = newBoard
        , gamePlayer = switchPlayer (gamePlayer game)
        , gameWinner = winner newBoard
        }
  where
    place Nothing = Just (Just (gamePlayer game))
    place _       = Nothing -- если клетка занята, поставить фишку нельзя 

-- | Сменить текущего игрока.
switchPlayer :: Mark -> Mark
switchPlayer X = O
switchPlayer O = X

-- | Применить преобразование к элементу списка
-- с заданным индексом. Если преобразование не удалось — вернуть 'Nothing'.
-- Иначе вернуть преобразованный список.
modifyAt :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
modifyAt _ _ []     = Nothing
modifyAt 0 f (x:xs) = case f x of
  Nothing -> Nothing
  Just y  -> Just (y : xs)
modifyAt i f (x:xs) = case modifyAt (i - 1) f xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)

-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> (Int, Int)
mouseToCell (x, y) = (i, j)
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize

-- | Определить победителя на игровом поле, если такой есть.
winner :: Board -> Maybe Mark
winner board = getFirstWinner (map lineWinner allLines)
  where
    allLines = rows ++ cols ++ diagonals
    rows = board
    cols = transpose board
    diagonals = lefts board ++ rights board

    lefts b = leftTops b ++ leftBottoms b
    rights = lefts . reverse

    leftTops    = transpose . zipWith drop [0..]
    leftBottoms = leftTops . transpose

    getFirstWinner :: [Maybe a] -> Maybe a
    getFirstWinner = foldr first Nothing
      where
        first Nothing y = y
        first x       _ = x

    lineWinner :: Eq a => [Maybe a] -> Maybe a
    lineWinner = getWinnerSegment . segments

    getWinnerSegment :: [(Maybe a, Int)] -> Maybe a
    getWinnerSegment = foldr compareSegments Nothing
      where
        compareSegments (Just x, n) _
          | n >= winnerStreak = Just x
        compareSegments _   y = y

    segments :: Eq a => [a] -> [(a, Int)]
    segments [] = []
    segments (x:xs) = segment : rest
      where
        segment = (x, 1 + length (takeWhile (== x) xs))
        rest    = segments (dropWhile (== x) xs)

-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> Game
updateGame _ = id

-- | Ширина игрового поля в клетках.
boardWidth :: Int
boardWidth  = 5

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 5

-- | Сколько фишек подряд необходимо для выигрыша.
winnerStreak :: Int
winnerStreak = 5

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 100

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight

module TicTacToe where

import Graphics.Gloss.Interface.Pure.Game

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

-- | Клетка игрового поля.
type Cell = Maybe Mark

-- | Игровое поле.
type Board = [[Cell]]

-- | Состояние игры.
data Game = Game
  { gameBoard  :: Board       -- ^ Игровое поле.
  , gamePlayer :: Maybe Mark  -- ^ Чей ход? Если все клетки заняты или есть победитель, то 'Nothing'.
  }

-- | Начальное состояние игры.
-- Игровое поле — пусто.
-- Первый игрок ходит за крестики.
initGame :: Game
initGame = Game
  { gameBoard  = replicate boardHeight (replicate boardWidth Nothing)
  , gamePlayer = Just X
  }

-- | Отобразить игровое поле.
drawGame :: Game -> Picture
drawGame game = translate (-w) (-h) (scale c c (pictures
  [ drawGrid
  , drawBoard (gameBoard game)
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
drawBoard :: Board -> Picture
drawBoard board = pictures (map pictures drawCells)
  where
    drawCells = map drawRow (zip [0..] board)
    drawRow (j, row) = map drawCellAt (zip [0..] row)
      where
        drawCellAt (i, cell) = translate (0.5 + i) (0.5 + j) (drawCell cell)

-- | Нарисовать фишку в клетке поля (если она там есть).
drawCell :: Cell -> Picture
drawCell Nothing     = blank
drawCell (Just mark) = drawMark mark

-- | Нарисовать фишку.
drawMark :: Mark -> Picture
drawMark X = color white unitX
  where
    unitX = pictures
      [ polygon [(-0.4,  0.3), (-0.3,  0.4), ( 0.4, -0.3), ( 0.3, -0.4)]
      , polygon [(-0.4, -0.3), (-0.3, -0.4), ( 0.4,  0.3), ( 0.3,  0.4)] ]
drawMark O = color white unitO
  where
    unitO = thickCircle 0.3 0.1

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) _ _ mouse) = placeMark (mouseToCell mouse)
handleGame _ = id

-- | Поставить фишку и сменить игрока (если возможно).
placeMark :: (Int, Int) -> Game -> Game
placeMark (i, j) game = case modifyAt j (modifyAt i g) (gameBoard game) of
  Just newBoard -> game
    { gameBoard  = newBoard
    , gamePlayer = newPlayer (gamePlayer game)
    }
  _ -> game   -- если поставить фишку нельзя — ничего не изменяется
  where
    g Nothing = Just (gamePlayer game)
    g _       = Nothing

    newPlayer Nothing  = Nothing
    newPlayer (Just m) = Just (switchPlayer m)

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

-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> Game
updateGame _ = id

-- | Ширина игрового поля в клетках.
boardWidth :: Int
boardWidth  = 10

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 5

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 70

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight

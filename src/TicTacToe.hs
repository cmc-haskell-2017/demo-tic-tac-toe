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
drawGame _ = pictures
  [ drawGrid ]

-- | Сетка игрового поля.
drawGrid :: Picture
drawGrid = color white (translate (-w) (-h) (scale c c (pictures (hs ++ vs))))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [1..m - 1]
    vs = map (\i -> line [(i, 0), (i, m)]) [1..n - 1]

    c = fromIntegral cellSize

    n = fromIntegral boardWidth
    m = fromIntegral boardHeight

    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame _ = id

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

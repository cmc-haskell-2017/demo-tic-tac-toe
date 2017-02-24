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

-- | Состояние игры.
data Game = Game

-- | Начальное состояние игры.
initGame :: Game
initGame = Game

-- | Отобразить игровое поле.
drawGame :: Game -> Picture
drawGame _ = blank

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

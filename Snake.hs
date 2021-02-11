module Snake where

move :: Food -> Direction -> Snake -> (Boolean, Snake)

checkGameOver :: Snake -> Boolean

generateNewFood :: Snake -> Random -> (Food, StdGen)

changeDirection :: GameState -> Direction -> GameState
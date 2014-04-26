import Keyboard


--MODELS AND INPUTS
type Input = {space:Bool, dx:Int, dy:Int, dt:Time}

type Player = {x:Float, y:Float, vx:Float, vy:Float, angle:Float}

data GameState = Play | NewStage | NewRound | GameOver | Menu

type Game = {state:GameState, player:Player}

defaultGame : Game
defaultGame = {state = Play, player = defaultPlayer}

defaultPlayer : Player
defaultPlayer = {x = 0, y = 0, vx = 0, vy = 0, angle = 0}


delta : Signal Time
delta = inSeconds <~ fps 60

input : Signal Input
input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ delta)

--UPDATE SECTION
stepGame : Input -> Game -> Game
stepGame i g = defaultGame


gameState : Signal Game
gameState = foldp stepGame defaultGame input
--DRAWING SECTION

drawGame : Game -> Element
drawGame input = asText input


--MAIN GAME LOOP
main : Signal Element
main = lift drawGame gameState


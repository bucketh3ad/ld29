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
stepPlayer : Input -> Player -> Player
stepPlayer i p = {p | angle <- p.angle - ( toFloat i.dx * i.dt * 2) }

stepGame : Input -> Game -> Game
stepGame ({space,dx,dy,dt} as i) ({state,player} as g) =
  {g | player <- stepPlayer i player}


gameState : Signal Game
gameState = foldp stepGame defaultGame input

--DRAWING SECTION

background : Form
background = group
  [ rect 800 600 |> filled black
  , rect 50 600 |> filled white
    |> move (-375,0)
  , rect 50 600 |> filled white
    |> move (375, 0)
  , rect 800 50 |> filled white
    |> move (0,-275)
  , rect 800 50 |> filled white
    |> move (0, 275)
  ]

drawPlayer : Form
drawPlayer = polygon [(0,15),(-10,-10),(10,-10)] |> outlined (solid white)

drawGame : Game -> Element
drawGame ({state,player} as game) =
  collage 800 600
    [ background
    , drawPlayer
      |> move (player.x, player.y)
      |> rotate player.angle
    , toForm (asText "A test" |> color white)
      |> move (0, 100)
    , toForm (asText player |> color white)
      |> move (0, -100)
    ]

--MAIN GAME LOOP
main : Signal Element
main = lift drawGame gameState


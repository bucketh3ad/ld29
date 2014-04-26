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

collideRect : Player -> Player
collideRect ({x,y,vx,vy,angle} as p) =
  let collidingH = abs x == 335
      collidingV = abs y == 235
  in {p | vx <- if collidingH then -vx else vx
        , vy <- if collidingV then -vy else vy }

applyThrust : Bool -> Float -> Player -> Player
applyThrust active dt ({x,y,vx,vy,angle} as p) =
  let 
      vxA = if active then sin (degrees angle) else 0
      vyA = if active then cos (degrees angle) else 0
      vx' = clamp -200 200 (vx - vxA)
      vy' = clamp -200 200 (vy + vyA)
  in {p | vx <- vx'
        , vy <- vy'
        , x <- movePlayer dt x vx -335 335
        , y <- movePlayer dt y vy -235 235 }
        
movePlayer : Float -> Float -> Float -> Float -> Float -> Float
movePlayer dt x vx xmin xmax = clamp xmin xmax (x + vx * dt)

stepPlayer : Input -> Player -> Player
stepPlayer ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle} as p) =
  let p' = collideRect <| applyThrust (dy == 1) dt p
  in {p' | angle <- angle - (toFloat dx * dt * 50)}
 

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
      |> rotate (degrees player.angle)
    , toForm (asText "A test" |> color white)
      |> move (0, 100)
    , toForm (asText player |> color white)
      |> move (0, -100)
    ]

--MAIN GAME LOOP
main : Signal Element
main = lift drawGame gameState


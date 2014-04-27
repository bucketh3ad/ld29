import Keyboard


--MODELS AND INPUTS
type Input = {space:Bool, dx:Int, dy:Int, dt:Time}

type GameObject a = {a | x:Float, y:Float, vx:Float, vy:Float, angle:Float, rev:Bool}

type Player = GameObject {}

data EnemyType = Small | Medium | Large

type Enemy =  GameObject {}

data GameState = Play | NewStage | NewRound | GameOver | Menu

data Collision = LeftRight | TopBottom

type Surface = Player -> Player

type Game = {state:GameState, player:Player, surface:Surface, enemy:Enemy}

defaultGame : Game
defaultGame = { state = Play,
                player = defaultPlayer,
                surface = klein,
                enemy = defaultEnemy }

defaultPlayer : Player
defaultPlayer = {x = 0, y = 0, vx = 0, vy = 0, angle = 0, rev = False}

defaultEnemy : Enemy
defaultEnemy = {x = 0, y = 0, vx = 50, vy = 50, angle = 0, rev = False}


delta : Signal Time
delta = inSeconds <~ fps 60

input : Signal Input
input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ delta)

--Constants
thrustFactor : Float
thrustFactor = 2


--UPDATE SECTION

--Helper functions
doV : [Collision] -> Bool
doV cols = any (\n -> n == TopBottom) cols

doH : [Collision] -> Bool
doH cols = any (\n -> n == LeftRight) cols
        
reflectAngle : Bool -> Float -> Float
reflectAngle yaxis a =
  if yaxis then -a
  else 180 - a


--Surface definitions
rectangle : Player -> Player
rectangle = collideRect [LeftRight,TopBottom]

cylinder : Player -> Player
cylinder = collideCyl [LeftRight] . collideRect [TopBottom]

mobius : Player -> Player
mobius = collideMobius [LeftRight] . collideRect [TopBottom]

torus : Player -> Player
torus = collideCyl [LeftRight,TopBottom]

klein : Player -> Player
klein = collideMobius [LeftRight] . collideCyl [TopBottom]

chaosphere : Player -> Player
chaosphere = collideMobius [LeftRight,TopBottom]


--Collision functions
collideRect : [Collision] -> Player -> Player
collideRect cols ({x,y,vx,vy,angle,rev} as p) =
  let collidingH = abs x >= 335
      collidingV = abs y >= 235
  in {p | vx <- if collidingH && doH cols then -vx else vx
        , vy <- if collidingV && doV cols then -vy else vy }
        
collideCyl : [Collision] -> Player -> Player
collideCyl cols ({x,y,vx,vy,angle,rev} as p) =
  let collidingH = abs x >= 365
      collidingV = abs y >= 265
  in {p | x <- if collidingH && doH cols then -x else x
        , y <- if collidingV && doV cols then -y else y }
        
collideMobius : [Collision] -> Player -> Player
collideMobius cols ({x,y,vx,vy,angle,rev} as p) =
  let collidingH = abs x >= 365
      collidingV = abs y >= 265
      doV' = collidingV && doV cols
      doH' = collidingH && doH cols
      doMove = doH' || doV'
  in {p | x <- if doMove then -x else x
        , y <- if doMove then -y else y
        , vx <- if doV' then -vx else vx
        , vy <- if doH' then -vy else vy
        , angle <- if doMove then reflectAngle doV' angle else angle
        , rev <- if doMove then not rev else rev}


--Movement functions
applyThrust : Bool -> Float -> Player -> Player
applyThrust active dt ({x,y,vx,vy,angle,rev} as p) =
  let 
      vxA = if active then thrustFactor*(sin (degrees angle)) else 0
      vyA = if active then thrustFactor*(cos (degrees angle)) else 0
      vx' = clamp -200 200 (vx - vxA)
      vy' = clamp -200 200 (vy + vyA)
  in {p | vx <- vx'
        , vy <- vy'
        , x <- movePlayer dt x vx -365 365
        , y <- movePlayer dt y vy -265 265 }

enemyMovement : Float -> Enemy -> Enemy
enemyMovement dt e = 
  { e| x <- movePlayer dt e.x e.vx -365 365
     , y <- movePlayer dt e.y e.vy -265 265 }

movePlayer : Float -> Float -> Float -> Float -> Float -> Float
movePlayer dt x vx xmin xmax = clamp xmin xmax (x + vx * dt)


--Update functions
stepEnemy : Surface -> Input -> Enemy -> Enemy
stepEnemy surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev} as e) =
  let angle' = if rev then angle + (dt * 50) else angle - (dt * 50)
      e' = surface <| enemyMovement dt e
  in  {e' | angle <- angle'}

stepPlayer : Surface -> Input -> Player -> Player
stepPlayer surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev} as p) =
  let p' = surface <| applyThrust (dy == 1) dt p
      dx' = if rev then -dx else dx
  in {p' | angle <- p'.angle - (toFloat dx' * dt * 100)}
 
stepGame : Input -> Game -> Game
stepGame ({space,dx,dy,dt} as i) ({state,player,surface,enemy} as g) =
  let stuck = abs player.x == 365 && abs player.y == 265 -- TRAPPED IN THE CHAOSPHERE
      p' = if stuck then {player | x <- 0, y <-0 } else player
  in {g | player <- stepPlayer surface i p'
        , enemy <- stepEnemy surface i enemy}

gameState : Signal Game
gameState = foldp stepGame defaultGame input

--DRAWING SECTION

background : Form
background = rect 800 600 |> filled black
  
foreground : Form
foreground =  group
  [ rect 50 600 |> filled white
    |> move (-375,0)
  , rect 50 600 |> filled white
    |> move (375, 0)
  , rect 800 50 |> filled white
    |> move (0,-275)
  , rect 800 50 |> filled white
    |> move (0, 275)
  ]

drawPlayer : Bool -> Form
drawPlayer rev = 
  let x' = if rev then 4 else -4
  in group
  [ circle 4 |> filled darkBlue |> move (x',-6)
  , circle 4 |> filled red |> move (-x',-6)
  , polygon [(0,15),(-10,-10),(10,-10)] |> outlined (solid white)
  ]

drawEnemy : Form
drawEnemy = group
  [ ngon 12 40 |> outlined (solid white)
  , ngon 6 25 |> outlined (solid white)
  , ngon 3 10 |> outlined (solid white)
  ]

drawGame : Game -> Element
drawGame ({state,player,surface,enemy} as game) =
  collage 800 600
    [ background
    , drawPlayer player.rev
      |> move (player.x, player.y)
      |> rotate (degrees player.angle)
    , drawEnemy 
      |> move (enemy.x, enemy.y)
      |> rotate (degrees enemy.angle)
    , toForm (show player |> toText |> Text.color white |> centered )
      |> move (0, -100)
    , toForm (show enemy |> toText |> Text.color white |> centered)
      |> move (0 , 100)
    , foreground
    ]

--MAIN GAME LOOP
main : Signal Element
main = lift drawGame gameState


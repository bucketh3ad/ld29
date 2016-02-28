module Ld29 where

import Keyboard
import Time
import Graphics.Collage exposing (..)
import Graphics.Element exposing (show,Element)
import Color exposing (..)
import Text

--MODELS AND INPUTS
type alias Input = {space:Bool, dx:Int, dy:Int, dt:Time.Time}

type alias GameObject a = {a | x:Float, y:Float, vx:Float, vy:Float, angle:Float, rev:Bool}

type alias Player = GameObject {}

type EnemyType = Small | Medium | Large

type alias Enemy =  GameObject {size:EnemyType}

type alias Bullet = GameObject {age:Float,active:Bool}

type GameState = Play | Win | Lose | Begin | Victory

type Collision = LeftRight | TopBottom

type alias Surface = Player -> Player

type SurfaceType = Rectangle | Cylinder | Torus | Mobius | Klein | Chaosphere

type alias Game = {state:GameState, player:Player, surface:SurfaceType, enemies:List Enemy, bullet:Bullet}

defaultGame : Game
defaultGame = { state = Begin,
                player = defaultPlayer,
                surface = Rectangle,
                enemies = defaultEnemies,
                bullet = defaultBullet}

defaultPlayer : Player
defaultPlayer = {x = 0, y = 0, vx = 0, vy = 0, angle = 0, rev = False}

defaultEnemy1 : Enemy
defaultEnemy1 = {x = 50, y = 50, vx = 50, vy = 50, angle = 0, rev = False, size = Large}

defaultEnemy2 : Enemy
defaultEnemy2 = {defaultEnemy1 | y  = -50, vy = -50}

defaultEnemy3 : Enemy
defaultEnemy3 = {defaultEnemy2 | x = -50, vx = -50}

defaultEnemy4 : Enemy
defaultEnemy4 = {defaultEnemy3 | y = 50, vy = 50}

defaultEnemies : List Enemy
defaultEnemies = [defaultEnemy1, defaultEnemy2, defaultEnemy3, defaultEnemy4]

testEnemies : List Enemy
testEnemies = [defaultEnemy1]


defaultBullet : Bullet
defaultBullet = {x = 0, y = 0, vx = 0, vy = 0, angle = 0, rev = False, age = 0,active = False}


delta : Signal Time.Time
delta = Signal.map Time.inSeconds <| Time.fps 60

input : Signal Input
input = 
  let s = Keyboard.space
      x = Signal.map .x Keyboard.arrows
      y = Signal.map .y Keyboard.arrows
  in Signal.map4 Input s x y delta 

--Constants
thrustFactor : Float
thrustFactor = 2

outerXMax : Float
outerXMax = 380

innerXMax : Float
innerXMax = 335

outerYMax : Float
outerYMax = 280

innerYMax : Float
innerYMax = 235




--UPDATE SECTION

--Helper functions
doV : List Collision -> Bool
doV cols = List.any (\n -> n == TopBottom) cols

doH : List Collision -> Bool
doH cols = List.any (\n -> n == LeftRight) cols
        
reflectAngle : Bool -> Float -> Float
reflectAngle yaxis a =
  if yaxis then -a
  else 180 - a

findDistance : Float -> Float -> Float -> Float -> Float
findDistance ax ay bx by = 
  let x' = ax - bx
      y' = ay - by
  in sqrt (x'^2 + y'^2)
  
enemySize : EnemyType -> Float
enemySize t =
  case t of
    Small -> 18
    Medium -> 30
    Large -> 45
      
nextSurface : SurfaceType -> SurfaceType
nextSurface s =
  case s of
    Rectangle -> Cylinder
    Cylinder -> Mobius
    Mobius -> Torus
    Torus -> Klein
    Klein -> Chaosphere
    Chaosphere -> Rectangle
     
getSurface : SurfaceType -> Surface
getSurface s =
  case s of
    Rectangle -> rectangle
    Cylinder -> cylinder
    Mobius -> mobius
    Torus -> torus
    Klein -> klein
    Chaosphere -> chaosphere
     

--Surface definitions
rectangle : Player -> Player
rectangle = collideRect [LeftRight,TopBottom]

cylinder : Player -> Player
cylinder = collideCyl [LeftRight] << collideRect [TopBottom]

mobius : Player -> Player
mobius = collideMobius [LeftRight] << collideRect [TopBottom]

torus : Player -> Player
torus = collideCyl [LeftRight,TopBottom]

klein : Player -> Player
klein = collideMobius [LeftRight] << collideCyl [TopBottom]

chaosphere : Player -> Player
chaosphere = collideMobius [LeftRight,TopBottom]


--Border collision functions
collideRect : List Collision -> Player -> Player
collideRect cols ({x,y,vx,vy,angle,rev} as p) =
  let collidingH = abs x >= innerXMax
      collidingV = abs y >= innerYMax
      doH' = collidingH && doH cols
      doV' = collidingV && doV cols
  in {p | vx = if doH' then -vx else vx
        , vy = if doV' then -vy else vy
        , x = if doH' then clamp -innerXMax innerXMax x else x
        , y = if doV' then clamp -innerYMax innerYMax y else y }
        
collideCyl : List Collision -> Player -> Player
collideCyl cols ({x,y,vx,vy,angle,rev} as p) =
  let collidingH = abs x >= outerXMax
      collidingV = abs y >= outerYMax
  in {p | x = if collidingH && doH cols then -x else x
        , y = if collidingV && doV cols then -y else y }
        
collideMobius : List Collision -> Player -> Player
collideMobius cols ({x,y,vx,vy,angle,rev} as p) =
  let collidingH = abs x >= outerXMax
      collidingV = abs y >= outerYMax
      doV' = collidingV && doV cols
      doH' = collidingH && doH cols
      doMove = doH' || doV'
  in {p | x = if doMove then -x else x
        , y = if doMove then -y else y
        , vx = if doV' then -vx else vx
        , vy = if doH' then -vy else vy
        , angle = if doMove then reflectAngle doV' angle else angle
        , rev = if doMove then not rev else rev}

--Object collision functions
checkCollision : GameObject a -> Enemy -> Bool
checkCollision obj e = findDistance e.x e.y obj.x obj.y <= enemySize e.size

mapPlayerCollisions : Player -> List Enemy -> Bool
mapPlayerCollisions p es =
  let bools = List.map (checkCollision p) es
  in List.any (\ n -> n) bools

mapBulletCollisions : Bullet -> List Enemy -> (List Enemy,Bool)
mapBulletCollisions b es = 
  let bools = List.map (checkCollision b) es
      b' = List.any (\ n -> n) bools
      es' = List.concat <| List.map handleBulletCollision <| List.map2 (,) es bools
  in  (es',b')
  
handleBulletCollision : (Enemy,Bool) -> List Enemy
handleBulletCollision (e,b) =
  if b && e.size == Small then
    []
  else if b then
    splitEnemy e
  else
    [e]
     
splitEnemy : Enemy -> List Enemy
splitEnemy e = 
  let newsize = if e.size == Large then Medium else Small
      dx = if e.vx > 0 then 15 else -15
      dy = if e.vy > 0 then 15 else -15
      e1 = {e | vx = e.vx + dx
              , vy = e.vy - dy
              , size = newsize }
      e2 = {e | vx = e.vx - dx
              , vy = e.vy + dy
              , size = newsize }
  in [e1,e2]

--Movement functions
applyThrust : Bool -> Float -> Player -> Player
applyThrust active dt ({x,y,vx,vy,angle,rev} as p) =
  let 
      vxA = if active 
            then thrustFactor*(sin (degrees angle))
            else 0
      vyA = if active
            then thrustFactor*(cos (degrees angle))
            else 0
      vx' = clamp -200 200 (vx - vxA)
      vy' = clamp -200 200 (vy + vyA)
  in {p | vx = vx'
        , vy = vy'
        , x = movePlayer dt x vx -outerXMax outerXMax
        , y = movePlayer dt y vy -outerYMax outerYMax }

enemyMovement : Float -> Enemy -> Enemy
enemyMovement dt e = 
  { e| x = movePlayer dt e.x e.vx -outerXMax outerXMax
     , y = movePlayer dt e.y e.vy -outerYMax outerYMax }

movePlayer : Float -> Float -> Float -> Float -> Float -> Float
movePlayer dt x vx xmin xmax = clamp xmin xmax (x + vx * dt)


--Update functions
createBullet : Player -> Bullet
createBullet ({x,y,vx,vy,angle,rev} as p) = 
  let vx' = -(sin (degrees angle) * 200)
      vy' = cos (degrees angle) * 200
  in {x = p.x, y = p.y, vx = vx', vy = vy', angle = p.angle, rev = p.rev, age = 0, active = True}

stepBullet : Surface -> Input -> Bullet -> Bullet
stepBullet surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev,age,active} as b) =
  let active' = active && age <= 2
      age' = if active' then age + dt else 0
      b' = { b | x = movePlayer dt x vx -outerXMax outerXMax
               , y = movePlayer dt y vy -outerYMax outerYMax }
      b'' = surface b' 
  in { b'' | active = active'
           , age = age'}

stepEnemy : Surface -> Input -> Enemy -> Enemy
stepEnemy surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev,size} as e) =
  let angle' = if rev then angle + (dt * 50) else angle - (dt * 50)
      e' = enemyMovement dt e
      e'' = surface e' 
  in  { e'' | angle = angle'}

stepPlayer : Surface -> Input -> Player -> Player
stepPlayer surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev} as p) =
  let p' = surface <| applyThrust (dy == 1) dt p
      dx' = if rev then -dx else dx
  in {p' | angle = p'.angle - (toFloat dx' * dt * 100)}
 
stepGame : Input -> Game -> Game
stepGame ({space,dx,dy,dt} as i) ({state,player,surface,enemies,bullet} as g) =
  let stuck = abs player.x == outerXMax && abs player.y == outerYMax
      p' = if stuck then {player | x = 0, y =0 } else player
      b' = if not bullet.active && space && state == Play
             then createBullet player
             else bullet
      bulletCollision = if bullet.active then mapBulletCollisions bullet enemies else (enemies,False)
      b'' = if not (snd bulletCollision) then b'
            else { b' | active = False }
      e' = fst bulletCollision
      playerCollision = mapPlayerCollisions p' e'
      state' = if List.length e' == 0 then Win
               else if playerCollision then Lose
               else state
      p'' = if state' == Play then stepPlayer (getSurface surface) i p'
            else if state' /= Play && space then defaultPlayer
            else p'
      e'' = if state' == Play then List.map (stepEnemy (getSurface surface) i) e'
            else if state' /= Play && space then defaultEnemies
            else e'
      b''' = if state' == Play then stepBullet (getSurface surface) i b'' else { b'' | active = False }
      nextSurf = nextSurface surface
      surface' = if state' == Win && space then nextSurf else surface
      state'' = if state' /= Play && space then Play
                else if state' == Win && surface' == Chaosphere then Victory
                else state'
  in {g | player = p''
        , enemies = e''  
        , bullet = b'''
        , surface = surface'
        , state = state'' }

gameState : Signal Game
gameState = Signal.foldp stepGame defaultGame input

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

drawBullet : Form
drawBullet = circle 5 |> filled white
  
drawEnemy : Enemy -> Form
drawEnemy e =
  let e' = case e.size of
                Large -> ngon 12 35 |> outlined (solid white)
                Medium -> ngon 6 25 |> outlined (solid white)
                Small -> ngon 3 10 |> outlined (solid white)
  in e' |> move (e.x,e.y) |> rotate (degrees e.angle)
  
drawEnemies : List Enemy -> Form
drawEnemies = group << List.map drawEnemy
  
prettyStats : List Float -> Form
prettyStats = formatText << toString << List.map truncate 

formatText : String -> Form
formatText = toForm << Graphics.Element.centered << (Text.color white) << Text.fromString

drawVictory : Form
drawVictory = "Congratulations! You have conquered every surface!"
 |> formatText
 
drawInstruction : Form
drawInstruction = "Press SPACE to start. Control with the arrow keys"
 |> formatText

drawGame : Game -> Element
drawGame ({state,player,surface,enemies,bullet} as game) =
  let bullet' = if not bullet.active
                then toForm (Graphics.Element.spacer 1 1)
                else drawBullet |> move (bullet.x, bullet.y)
      v' = if state == Victory
           then drawVictory |> move (0,100)
           else toForm (Graphics.Element.spacer 1 1)
      i' = if state == Begin
           then drawInstruction |> move (0,100)
           else toForm (Graphics.Element.spacer 1 1)
  in collage 800 600
      [ background
      , drawPlayer player.rev
        |> move (player.x, player.y)
        |> rotate (degrees player.angle)
      , drawEnemies enemies
      , bullet'
      , v'
      , i'
      --, prettyStats [player.x,player.y,player.vx,player.vy]
      --  |> move (0, -100)
      , toString state |> formatText
        |> move (0,200)
      , toString surface |> formatText
        |> move (0,-200)
      , foreground
      ]

--MAIN GAME LOOP
main : Signal Element
main = Signal.map drawGame gameState


import Keyboard


--MODELS AND INPUTS
type Input = {space:Bool, dx:Int, dy:Int, dt:Time}

type GameObject a = {a | x:Float, y:Float, vx:Float, vy:Float, angle:Float, rev:Bool}

type Player = GameObject {}

data EnemyType = Small | Medium | Large

type Enemy =  GameObject {size:EnemyType}

type Bullet = GameObject {age:Float,active:Bool}

data GameState = Play | Win | Lose

data Collision = LeftRight | TopBottom

type Surface = Player -> Player

type Game = {state:GameState, player:Player, surface:Surface, enemies:[Enemy], bullet:Bullet}

defaultGame : Game
defaultGame = { state = Play,
                player = defaultPlayer,
                surface = klein,
                enemies = [defaultEnemy,mediumEnemy,smallEnemy,{smallEnemy | vx <- 50, vy <- -50}],
                bullet = defaultBullet}

defaultPlayer : Player
defaultPlayer = {x = 0, y = 0, vx = 0, vy = 0, angle = 0, rev = False}

defaultEnemy : Enemy
defaultEnemy = {x = 0, y = 0, vx = 50, vy = 50, angle = 0, rev = False, size = Large}

mediumEnemy : Enemy
mediumEnemy = {x = 0, y = 0, vx = -50, vy = -50, angle = 0, rev = False, size = Medium}

smallEnemy : Enemy
smallEnemy = {x = 0, y = 0, vx = -50, vy = 50, angle = 0, rev = False, size = Small}

defaultBullet : Bullet
defaultBullet = {x = 0, y = 0, vx = 0, vy = 0, angle = 0, rev = False, age = 0,active = False}


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
doV : [Collision] -> Bool
doV cols = any (\n -> n == TopBottom) cols

doH : [Collision] -> Bool
doH cols = any (\n -> n == LeftRight) cols
        
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
  if  |t == Small -> 18
      |t == Medium -> 30
      |t == Large -> 45

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


--Border collision functions
collideRect : [Collision] -> Player -> Player
collideRect cols ({x,y,vx,vy,angle,rev} as p) =
  let collidingH = abs x >= innerXMax
      collidingV = abs y >= innerYMax
  in {p | vx <- if collidingH && doH cols then -vx else vx
        , vy <- if collidingV && doV cols then -vy else vy }
        
collideCyl : [Collision] -> Player -> Player
collideCyl cols ({x,y,vx,vy,angle,rev} as p) =
  let collidingH = abs x >= outerXMax
      collidingV = abs y >= outerYMax
  in {p | x <- if collidingH && doH cols then -x else x
        , y <- if collidingV && doV cols then -y else y }
        
collideMobius : [Collision] -> Player -> Player
collideMobius cols ({x,y,vx,vy,angle,rev} as p) =
  let collidingH = abs x >= outerXMax
      collidingV = abs y >= outerYMax
      doV' = collidingV && doV cols
      doH' = collidingH && doH cols
      doMove = doH' || doV'
  in {p | x <- if doMove then -x else x
        , y <- if doMove then -y else y
        , vx <- if doV' then -vx else vx
        , vy <- if doH' then -vy else vy
        , angle <- if doMove then reflectAngle doV' angle else angle
        , rev <- if doMove then not rev else rev}

--Object collision functions
checkCollision : GameObject a -> Enemy -> Bool
checkCollision obj e = findDistance e.x e.y obj.x obj.y <= enemySize e.size

mapPlayerCollisions : Player -> [Enemy] -> Bool
mapPlayerCollisions p es =
  let bools = map (checkCollision p) es
  in any (\ n -> n) bools

mapBulletCollisions : Bullet -> [Enemy] -> ([Enemy],Bool)
mapBulletCollisions b es = 
  let bools = map (checkCollision b) es
      b' = any (\ n -> n) bools
      es' = concat <| map handleBulletCollision (zip es bools)
  in  (es',b')
  
handleBulletCollision : (Enemy,Bool) -> [Enemy]
handleBulletCollision (e,b) =
  if | b && e.size == Small -> []
     | b -> splitEnemy e
     | otherwise -> [e]
     
splitEnemy : Enemy -> [Enemy]
splitEnemy e = 
  let newsize = if e.size == Large then Medium else Small
      dx = if e.vx > 0 then 15 else -15
      dy = if e.vy > 0 then 15 else -15
      e1 = {e | vx <- e.vx + dx
              , vy <- e.vy - dy
              , size <- newsize }
      e2 = {e | vx <- e.vx - dx
              , vy <- e.vy + dy
              , size <- newsize }
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
  in {p | vx <- vx'
        , vy <- vy'
        , x <- movePlayer dt x vx -outerXMax outerXMax
        , y <- movePlayer dt y vy -outerYMax outerYMax }

enemyMovement : Float -> Enemy -> Enemy
enemyMovement dt e = 
  { e| x <- movePlayer dt e.x e.vx -outerXMax outerXMax
     , y <- movePlayer dt e.y e.vy -outerYMax outerYMax }

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
  let active' = active && age <= 4
      age' = if active' then age + dt else 0
      b' = { b | x <- movePlayer dt x vx -outerXMax outerXMax
               , y <- movePlayer dt y vy -outerYMax outerYMax }
      b'' = {b' - age}
      b''' = surface <| {b'' - active}
      b'''' = {b''' | age = age'}
  in { b'''' | active = active' }

stepEnemy : Surface -> Input -> Enemy -> Enemy
stepEnemy surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev,size} as e) =
  let angle' = if rev then angle + (dt * 50) else angle - (dt * 50)
      e' = enemyMovement dt e
      e'' = surface <| { e' - size }
      e''' = { e'' | size = e.size }
  in  { e''' | angle <- angle'}

stepPlayer : Surface -> Input -> Player -> Player
stepPlayer surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev} as p) =
  let p' = surface <| applyThrust (dy == 1) dt p
      dx' = if rev then -dx else dx
  in {p' | angle <- p'.angle - (toFloat dx' * dt * 100)}
 
stepGame : Input -> Game -> Game
stepGame ({space,dx,dy,dt} as i) ({state,player,surface,enemies,bullet} as g) =
  let stuck = abs player.x == outerXMax && abs player.y == outerYMax
      p' = if stuck then {player | x <- 0, y <-0 } else player
      b' = if not bullet.active && space
             then createBullet player
             else stepBullet surface i bullet
      bulletCollision = if bullet.active then mapBulletCollisions bullet enemies else (enemies,False)
      b'' = if not (snd bulletCollision) then b'
            else { b' | active <- False }
      e' = fst bulletCollision
      playerCollision = mapPlayerCollisions p' e'
      state' = if | length e' == 0 -> Win
                  | playerCollision -> Lose
                  | otherwise -> Play
  in {g | player <- stepPlayer surface i p'
        , enemies <- map (stepEnemy surface i) e'  
        , bullet <- b''
        , state <- state' }

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

drawBullet : Form
drawBullet = circle 5 |> filled white
  
drawEnemy : Enemy -> Form
drawEnemy e =
  let e' = if | e.size == Large -> ngon 12 35 |> outlined (solid white)
              | e.size == Medium -> ngon 6 25 |> outlined (solid white)
              | e.size == Small -> ngon 3 10 |> outlined (solid white)
  in e' |> move (e.x,e.y) |> rotate (degrees e.angle)
  
drawEnemies : [Enemy] -> Form
drawEnemies = group . map drawEnemy
  
prettyPrint : [Float] -> Form
prettyPrint = toForm . centered . (Text.color white) . toText . show . map (\n -> truncate n)

drawGame : Game -> Element
drawGame ({state,player,surface,enemies,bullet} as game) =
  let bullet' = if not bullet.active
                then toForm (spacer 1 1)
                else drawBullet |> move (bullet.x, bullet.y)
  in collage 800 600
      [ background
      , drawPlayer player.rev
        |> move (player.x, player.y)
        |> rotate (degrees player.angle)
      , drawEnemies enemies
      , bullet'
      , prettyPrint [player.x,player.y,player.vx,player.vy]
        |> move (0, -100)
      , toForm (show state |> toText |> Text.color white |> centered)
        |> move (0 , 100)
      , foreground
      ]

--MAIN GAME LOOP
main : Signal Element
main = lift drawGame gameState


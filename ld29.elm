import Keyboard


--MODELS AND INPUTS
type Input = {space:Bool, dx:Int, dy:Int, dt:Time}

type GameObject a = {a | x:Float, y:Float, vx:Float, vy:Float, angle:Float, rev:Bool}

type Player = GameObject {}

data EnemyType = Small | Medium | Large

type Enemy =  GameObject {size:EnemyType}

type Bullet = GameObject {age:Float,active:Bool}

data GameState = Play | Win | Lose | Begin | Victory

data Collision = LeftRight | TopBottom

type Surface a = GameObject a -> GameObject a

data SurfaceType = Rectangle | Cylinder | Torus | Mobius | Klein | Chaosphere

type Game = {state:GameState, player:Player, surface:SurfaceType, enemies:[Enemy], bullet:Bullet}

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
defaultEnemy2 = {defaultEnemy1 | y  <- -50, vy <- -50}

defaultEnemy3 : Enemy
defaultEnemy3 = {defaultEnemy2 | x <- -50, vx <- -50}

defaultEnemy4 : Enemy
defaultEnemy4 = {defaultEnemy3 | y <- 50, vy <- 50}

defaultEnemies : [Enemy]
defaultEnemies = [defaultEnemy1, defaultEnemy2, defaultEnemy3, defaultEnemy4]

testEnemies : [Enemy]
testEnemies = [defaultEnemy1]


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
      
nextSurface : SurfaceType -> SurfaceType
nextSurface s =
  if | s == Rectangle -> Cylinder
     | s == Cylinder -> Mobius
     | s == Mobius -> Torus
     | s == Torus -> Klein
     | s == Klein -> Chaosphere
     | s == Chaosphere -> Rectangle
     
getSurface : SurfaceType -> Surface a
getSurface s =
  if | s == Rectangle -> rectangle
     | s == Cylinder -> cylinder
     | s == Mobius -> mobius
     | s == Torus -> torus
     | s == Klein -> klein
     | s == Chaosphere -> chaosphere
     

--Surface definitions
rectangle : Surface a
rectangle = collideRect [LeftRight,TopBottom]

cylinder : Surface a
cylinder = collideCyl [LeftRight] . collideRect [TopBottom]

mobius : Surface a
mobius = collideMobius [LeftRight] . collideRect [TopBottom]

torus : Surface a
torus = collideCyl [LeftRight,TopBottom]

klein : Surface a
klein = collideMobius [LeftRight] . collideCyl [TopBottom]

chaosphere : Surface a
chaosphere = collideMobius [LeftRight,TopBottom]


--Border collision functions
collideRect : [Collision] -> Surface a
collideRect cols obj =
  let collidingH = abs obj.x >= innerXMax
      collidingV = abs obj.y >= innerYMax
      doH' = collidingH && doH cols
      doV' = collidingV && doV cols
  in {obj | vx <- if doH' then -obj.vx else obj.vx
        , vy <- if doV' then -obj.vy else obj.vy
        , x <- if doH' then clamp -innerXMax innerXMax obj.x else obj.x
        , y <- if doV' then clamp -innerYMax innerYMax obj.y else obj.y }
        
collideCyl : [Collision] -> Surface a
collideCyl cols obj =
  let collidingH = abs obj.x >= outerXMax
      collidingV = abs obj.y >= outerYMax
  in {obj | x <- if collidingH && doH cols then -obj.x else obj.x
        , y <- if collidingV && doV cols then -obj.y else obj.y }
        
collideMobius : [Collision] -> Surface a
collideMobius cols obj =
  let collidingH = abs obj.x >= outerXMax
      collidingV = abs obj.y >= outerYMax
      doV' = collidingV && doV cols
      doH' = collidingH && doH cols
      doMove = doH' || doV'
  in {obj | x <- if doMove then -obj.x else obj.x
        , y <- if doMove then -obj.y else obj.y
        , vx <- if doV' then -obj.vx else obj.vx
        , vy <- if doH' then -obj.vy else obj.vy
        , angle <- if doMove then reflectAngle doV' obj.angle else obj.angle
        , rev <- if doMove then not obj.rev else obj.rev}

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

stepBullet : Surface {} -> Input -> Bullet -> Bullet
stepBullet surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev,age,active} as b) =
  let active' = active && age <= 2
      age' = if active' then age + dt else 0
      b' = { b | x <- movePlayer dt x vx -outerXMax outerXMax
               , y <- movePlayer dt y vy -outerYMax outerYMax }
      b'' = {b' - age}
      b''' = surface <| {b'' - active}
      b'''' = {b''' | age = age'}
  in { b'''' | active = active' }

stepEnemy : Surface {} -> Input -> Enemy -> Enemy
stepEnemy surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev,size} as e) =
  let angle' = if rev then angle + (dt * 50) else angle - (dt * 50)
      e' = enemyMovement dt e
      e'' = surface <| { e' - size }
      e''' = { e'' | size = e.size }
  in  { e''' | angle <- angle'}

stepPlayer : Surface {} -> Input -> Player -> Player
stepPlayer surface ({space,dx,dy,dt} as i) ({x,y,vx,vy,angle,rev} as p) =
  let p' = surface <| applyThrust (dy == 1) dt p
      dx' = if rev then -dx else dx
  in {p' | angle <- p'.angle - (toFloat dx' * dt * 100)}
 
stepGame : Input -> Game -> Game
stepGame ({space,dx,dy,dt} as i) ({state,player,surface,enemies,bullet} as g) =
  let stuck = abs player.x == outerXMax && abs player.y == outerYMax
      p' = if stuck then {player | x <- 0, y <-0 } else player
      b' = if not bullet.active && space && state == Play
             then createBullet player
             else bullet
      bulletCollision = if bullet.active then mapBulletCollisions bullet enemies else (enemies,False)
      b'' = if not (snd bulletCollision) then b'
            else { b' | active <- False }
      e' = fst bulletCollision
      playerCollision = mapPlayerCollisions p' e'
      state' = if | length e' == 0 -> Win
                  | playerCollision -> Lose
                  | otherwise -> state
      p'' = if | state' == Play -> stepPlayer (getSurface surface) i p'
               | state' /= Play && space -> defaultPlayer
               | otherwise -> p'
      e'' = if | state' == Play -> map (stepEnemy (getSurface surface) i) e'
               | state' /= Play && space -> defaultEnemies
               | otherwise -> e'
      b''' = if state' == Play then stepBullet (getSurface surface) i b'' else { b'' | active <- False }
      nextSurf = nextSurface surface
      surface' = if state' == Win && space then nextSurf else surface
      state'' = if | state' /= Play && space -> Play
                   | state' == Win && surface' == Chaosphere -> Victory
                   | otherwise -> state'
  in {g | player <- p''
        , enemies <- e''  
        , bullet <- b'''
        , surface <- surface'
        , state <- state'' }

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
  
prettyStats : [Float] -> Form
prettyStats = formatText . show . map (\n -> truncate n)

formatText : String -> Form
formatText = toForm . centered . (Text.color white) . toText

drawVictory : Form
drawVictory = "Congratulations! You have conquered every surface!"
 |> formatText
 
drawInstruction : Form
drawInstruction = "Press SPACE to start. Control with the arrow keys"
 |> formatText

drawGame : Game -> Element
drawGame ({state,player,surface,enemies,bullet} as game) =
  let bullet' = if not bullet.active
                then toForm (spacer 1 1)
                else drawBullet |> move (bullet.x, bullet.y)
      v' = if state == Victory
           then drawVictory |> move (0,100)
           else toForm (spacer 1 1)
      i' = if state == Begin
           then drawInstruction |> move (0,100)
           else toForm (spacer 1 1)
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
      , show state |> formatText
        |> move (0,200)
      , show surface |> formatText
        |> move (0,-200)
      , foreground
      ]

--MAIN GAME LOOP
main : Signal Element
main = lift drawGame gameState


Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   _J = _N.JavaScript.make(_elm),
   $moduleName = "Main";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var Keyboard = Elm.Keyboard.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var formatText = function ($) {
      return Graphics.Collage.toForm(Text.centered(Text.color(Color.white)(Text.toText($))));
   };
   var drawVictory = formatText("Congratulations! You have conquered every surface!");
   var drawInstruction = formatText("Press SPACE to start. Control with the arrow keys");
   var prettyStats = function ($) {
      return formatText(String.show(List.map(function (n) {
         return Basics.truncate(n);
      })($)));
   };
   var drawBullet = Graphics.Collage.filled(Color.white)(Graphics.Collage.circle(5));
   var drawPlayer = function (rev) {
      return function () {
         var x$ = rev ? 4 : -4;
         return Graphics.Collage.group(_J.toList([Graphics.Collage.move({ctor: "_Tuple2"
                                                                        ,_0: x$
                                                                        ,_1: -6})(Graphics.Collage.filled(Color.darkBlue)(Graphics.Collage.circle(4)))
                                                 ,Graphics.Collage.move({ctor: "_Tuple2"
                                                                        ,_0: 0 - x$
                                                                        ,_1: -6})(Graphics.Collage.filled(Color.red)(Graphics.Collage.circle(4)))
                                                 ,Graphics.Collage.outlined(Graphics.Collage.solid(Color.white))(Graphics.Collage.polygon(_J.toList([{ctor: "_Tuple2"
                                                                                                                                                     ,_0: 0
                                                                                                                                                     ,_1: 15}
                                                                                                                                                    ,{ctor: "_Tuple2"
                                                                                                                                                     ,_0: -10
                                                                                                                                                     ,_1: -10}
                                                                                                                                                    ,{ctor: "_Tuple2"
                                                                                                                                                     ,_0: 10
                                                                                                                                                     ,_1: -10}])))]));
      }();
   };
   var foreground = Graphics.Collage.group(_J.toList([Graphics.Collage.move({ctor: "_Tuple2"
                                                                            ,_0: -375
                                                                            ,_1: 0})(Graphics.Collage.filled(Color.white)(A2(Graphics.Collage.rect,
                                                     50,
                                                     600)))
                                                     ,Graphics.Collage.move({ctor: "_Tuple2"
                                                                            ,_0: 375
                                                                            ,_1: 0})(Graphics.Collage.filled(Color.white)(A2(Graphics.Collage.rect,
                                                     50,
                                                     600)))
                                                     ,Graphics.Collage.move({ctor: "_Tuple2"
                                                                            ,_0: 0
                                                                            ,_1: -275})(Graphics.Collage.filled(Color.white)(A2(Graphics.Collage.rect,
                                                     800,
                                                     50)))
                                                     ,Graphics.Collage.move({ctor: "_Tuple2"
                                                                            ,_0: 0
                                                                            ,_1: 275})(Graphics.Collage.filled(Color.white)(A2(Graphics.Collage.rect,
                                                     800,
                                                     50)))]));
   var background = Graphics.Collage.filled(Color.black)(A2(Graphics.Collage.rect,
   800,
   600));
   var createBullet = function (_v0) {
      return function () {
         return function () {
            var vy$ = Basics.cos(Basics.degrees(_v0.angle)) * 200;
            var vx$ = 0 - Basics.sin(Basics.degrees(_v0.angle)) * 200;
            return {_: {}
                   ,active: true
                   ,age: 0
                   ,angle: _v0.angle
                   ,rev: _v0.rev
                   ,vx: vx$
                   ,vy: vy$
                   ,x: _v0.x
                   ,y: _v0.y};
         }();
      }();
   };
   var movePlayer = F5(function (dt,
   x,
   vx,
   xmin,
   xmax) {
      return A3(Basics.clamp,
      xmin,
      xmax,
      x + vx * dt);
   });
   var findDistance = F4(function (ax,
   ay,
   bx,
   by) {
      return function () {
         var y$ = ay - by;
         var x$ = ax - bx;
         return Basics.sqrt(Math.pow(x$,
         2) + Math.pow(y$,2));
      }();
   });
   var reflectAngle = F2(function (yaxis,
   a) {
      return yaxis ? 0 - a : 180 - a;
   });
   var innerYMax = 235;
   var outerYMax = 280;
   var innerXMax = 335;
   var outerXMax = 380;
   var enemyMovement = F2(function (dt,
   e) {
      return _U.replace([["x"
                         ,A5(movePlayer,
                         dt,
                         e.x,
                         e.vx,
                         0 - outerXMax,
                         outerXMax)]
                        ,["y"
                         ,A5(movePlayer,
                         dt,
                         e.y,
                         e.vy,
                         0 - outerYMax,
                         outerYMax)]],
      e);
   });
   var stepEnemy = F3(function (surface,
   _v2,
   _v3) {
      return function () {
         return function () {
            return function () {
               var e$ = A2(enemyMovement,
               _v2.dt,
               _v3);
               var e$$ = surface(_U.remove("size",
               e$));
               var e$$$ = _U.insert("size",
               _v3.size,
               e$$);
               var angle$ = _v3.rev ? _v3.angle + _v2.dt * 50 : _v3.angle - _v2.dt * 50;
               return _U.replace([["angle"
                                  ,angle$]],
               e$$$);
            }();
         }();
      }();
   });
   var stepBullet = F3(function (surface,
   _v6,
   _v7) {
      return function () {
         return function () {
            return function () {
               var b$ = _U.replace([["x"
                                    ,A5(movePlayer,
                                    _v6.dt,
                                    _v7.x,
                                    _v7.vx,
                                    0 - outerXMax,
                                    outerXMax)]
                                   ,["y"
                                    ,A5(movePlayer,
                                    _v6.dt,
                                    _v7.y,
                                    _v7.vy,
                                    0 - outerYMax,
                                    outerYMax)]],
               _v7);
               var b$$ = _U.remove("age",b$);
               var b$$$ = surface(_U.remove("active",
               b$$));
               var active$ = _v7.active && _U.cmp(_v7.age,
               2) < 1;
               var age$ = active$ ? _v7.age + _v6.dt : 0;
               var b$$$$ = _U.insert("age",
               age$,
               b$$$);
               return _U.insert("active",
               active$,
               b$$$$);
            }();
         }();
      }();
   });
   var thrustFactor = 2;
   var applyThrust = F3(function (active,
   dt,
   _v10) {
      return function () {
         return function () {
            var vyA = active ? thrustFactor * Basics.cos(Basics.degrees(_v10.angle)) : 0;
            var vy$ = A3(Basics.clamp,
            -200,
            200,
            _v10.vy + vyA);
            var vxA = active ? thrustFactor * Basics.sin(Basics.degrees(_v10.angle)) : 0;
            var vx$ = A3(Basics.clamp,
            -200,
            200,
            _v10.vx - vxA);
            return _U.replace([["vx",vx$]
                              ,["vy",vy$]
                              ,["x"
                               ,A5(movePlayer,
                               dt,
                               _v10.x,
                               _v10.vx,
                               0 - outerXMax,
                               outerXMax)]
                              ,["y"
                               ,A5(movePlayer,
                               dt,
                               _v10.y,
                               _v10.vy,
                               0 - outerYMax,
                               outerYMax)]],
            _v10);
         }();
      }();
   });
   var stepPlayer = F3(function (surface,
   _v12,
   _v13) {
      return function () {
         return function () {
            return function () {
               var dx$ = _v13.rev ? 0 - _v12.dx : _v12.dx;
               var p$ = surface(A3(applyThrust,
               _U.eq(_v12.dy,1),
               _v12.dt,
               _v13));
               return _U.replace([["angle"
                                  ,p$.angle - Basics.toFloat(dx$) * _v12.dt * 100]],
               p$);
            }();
         }();
      }();
   });
   var delta = A2(Signal._op["<~"],
   Time.inSeconds,
   Time.fps(60));
   var defaultBullet = {_: {}
                       ,active: false
                       ,age: 0
                       ,angle: 0
                       ,rev: false
                       ,vx: 0
                       ,vy: 0
                       ,x: 0
                       ,y: 0};
   var defaultPlayer = {_: {}
                       ,angle: 0
                       ,rev: false
                       ,vx: 0
                       ,vy: 0
                       ,x: 0
                       ,y: 0};
   var Game = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,bullet: e
             ,enemies: d
             ,player: b
             ,state: a
             ,surface: c};
   });
   var Chaosphere = {ctor: "Chaosphere"};
   var Klein = {ctor: "Klein"};
   var Mobius = {ctor: "Mobius"};
   var Torus = {ctor: "Torus"};
   var Cylinder = {ctor: "Cylinder"};
   var Rectangle = {ctor: "Rectangle"};
   var nextSurface = function (s) {
      return _U.eq(s,
      Rectangle) ? Cylinder : _U.eq(s,
      Cylinder) ? Mobius : _U.eq(s,
      Mobius) ? Torus : _U.eq(s,
      Torus) ? Klein : _U.eq(s,
      Klein) ? Chaosphere : _U.eq(s,
      Chaosphere) ? Rectangle : _E.If($moduleName,
      "between lines 116 and 121");
   };
   var TopBottom = {ctor: "TopBottom"};
   var doV = function (cols) {
      return A2(List.any,
      function (n) {
         return _U.eq(n,TopBottom);
      },
      cols);
   };
   var LeftRight = {ctor: "LeftRight"};
   var doH = function (cols) {
      return A2(List.any,
      function (n) {
         return _U.eq(n,LeftRight);
      },
      cols);
   };
   var collideRect = F2(function (cols,
   _v16) {
      return function () {
         return function () {
            var collidingV = _U.cmp(Basics.abs(_v16.y),
            innerYMax) > -1;
            var doV$ = collidingV && doV(cols);
            var collidingH = _U.cmp(Basics.abs(_v16.x),
            innerXMax) > -1;
            var doH$ = collidingH && doH(cols);
            return _U.replace([["vx"
                               ,doH$ ? 0 - _v16.vx : _v16.vx]
                              ,["vy"
                               ,doV$ ? 0 - _v16.vy : _v16.vy]
                              ,["x"
                               ,doH$ ? A3(Basics.clamp,
                               0 - innerXMax,
                               innerXMax,
                               _v16.x) : _v16.x]
                              ,["y"
                               ,doV$ ? A3(Basics.clamp,
                               0 - innerYMax,
                               innerYMax,
                               _v16.y) : _v16.y]],
            _v16);
         }();
      }();
   });
   var collideCyl = F2(function (cols,
   _v18) {
      return function () {
         return function () {
            var collidingV = _U.cmp(Basics.abs(_v18.y),
            outerYMax) > -1;
            var collidingH = _U.cmp(Basics.abs(_v18.x),
            outerXMax) > -1;
            return _U.replace([["x"
                               ,collidingH && doH(cols) ? 0 - _v18.x : _v18.x]
                              ,["y"
                               ,collidingV && doV(cols) ? 0 - _v18.y : _v18.y]],
            _v18);
         }();
      }();
   });
   var collideMobius = F2(function (cols,
   _v20) {
      return function () {
         return function () {
            var collidingV = _U.cmp(Basics.abs(_v20.y),
            outerYMax) > -1;
            var doV$ = collidingV && doV(cols);
            var collidingH = _U.cmp(Basics.abs(_v20.x),
            outerXMax) > -1;
            var doH$ = collidingH && doH(cols);
            var doMove = doH$ || doV$;
            return _U.replace([["x"
                               ,doMove ? 0 - _v20.x : _v20.x]
                              ,["y"
                               ,doMove ? 0 - _v20.y : _v20.y]
                              ,["vx"
                               ,doV$ ? 0 - _v20.vx : _v20.vx]
                              ,["vy"
                               ,doH$ ? 0 - _v20.vy : _v20.vy]
                              ,["angle"
                               ,doMove ? A2(reflectAngle,
                               doV$,
                               _v20.angle) : _v20.angle]
                              ,["rev"
                               ,doMove ? Basics.not(_v20.rev) : _v20.rev]],
            _v20);
         }();
      }();
   });
   var rectangle = collideRect(_J.toList([LeftRight
                                         ,TopBottom]));
   var cylinder = function ($) {
      return collideCyl(_J.toList([LeftRight]))(collideRect(_J.toList([TopBottom]))($));
   };
   var mobius = function ($) {
      return collideMobius(_J.toList([LeftRight]))(collideRect(_J.toList([TopBottom]))($));
   };
   var torus = collideCyl(_J.toList([LeftRight
                                    ,TopBottom]));
   var klein = function ($) {
      return collideMobius(_J.toList([LeftRight]))(collideCyl(_J.toList([TopBottom]))($));
   };
   var chaosphere = collideMobius(_J.toList([LeftRight
                                            ,TopBottom]));
   var getSurface = function (s) {
      return _U.eq(s,
      Rectangle) ? rectangle : _U.eq(s,
      Cylinder) ? cylinder : _U.eq(s,
      Mobius) ? mobius : _U.eq(s,
      Torus) ? torus : _U.eq(s,
      Klein) ? klein : _U.eq(s,
      Chaosphere) ? chaosphere : _E.If($moduleName,
      "between lines 125 and 130");
   };
   var Victory = {ctor: "Victory"};
   var Begin = {ctor: "Begin"};
   var Lose = {ctor: "Lose"};
   var Win = {ctor: "Win"};
   var Play = {ctor: "Play"};
   var Large = {ctor: "Large"};
   var defaultEnemy1 = {_: {}
                       ,angle: 0
                       ,rev: false
                       ,size: Large
                       ,vx: 50
                       ,vy: 50
                       ,x: 50
                       ,y: 50};
   var defaultEnemy2 = _U.replace([["y"
                                   ,-50]
                                  ,["vy",-50]],
   defaultEnemy1);
   var defaultEnemy3 = _U.replace([["x"
                                   ,-50]
                                  ,["vx",-50]],
   defaultEnemy2);
   var defaultEnemy4 = _U.replace([["y"
                                   ,50]
                                  ,["vy",50]],
   defaultEnemy3);
   var defaultEnemies = _J.toList([defaultEnemy1
                                  ,defaultEnemy2
                                  ,defaultEnemy3
                                  ,defaultEnemy4]);
   var defaultGame = {_: {}
                     ,bullet: defaultBullet
                     ,enemies: defaultEnemies
                     ,player: defaultPlayer
                     ,state: Begin
                     ,surface: Rectangle};
   var testEnemies = _J.toList([defaultEnemy1]);
   var Medium = {ctor: "Medium"};
   var Small = {ctor: "Small"};
   var enemySize = function (t) {
      return _U.eq(t,
      Small) ? 18 : _U.eq(t,
      Medium) ? 30 : _U.eq(t,
      Large) ? 45 : _E.If($moduleName,
      "between lines 110 and 112");
   };
   var checkCollision = F2(function (obj,
   e) {
      return _U.cmp(A4(findDistance,
      e.x,
      e.y,
      obj.x,
      obj.y),
      enemySize(e.size)) < 1;
   });
   var mapPlayerCollisions = F2(function (p,
   es) {
      return function () {
         var bools = A2(List.map,
         checkCollision(p),
         es);
         return A2(List.any,
         function (n) {
            return n;
         },
         bools);
      }();
   });
   var splitEnemy = function (e) {
      return function () {
         var dy = _U.cmp(e.vy,
         0) > 0 ? 15 : -15;
         var dx = _U.cmp(e.vx,
         0) > 0 ? 15 : -15;
         var newsize = _U.eq(e.size,
         Large) ? Medium : Small;
         var e1 = _U.replace([["vx"
                              ,e.vx + dx]
                             ,["vy",e.vy - dy]
                             ,["size",newsize]],
         e);
         var e2 = _U.replace([["vx"
                              ,e.vx - dx]
                             ,["vy",e.vy + dy]
                             ,["size",newsize]],
         e);
         return _J.toList([e1,e2]);
      }();
   };
   var handleBulletCollision = function (_v22) {
      return function () {
         switch (_v22.ctor)
         {case "_Tuple2":
            return _v22._1 && _U.eq(_v22._0.size,
              Small) ? _J.toList([]) : _v22._1 ? splitEnemy(_v22._0) : _J.toList([_v22._0]);}
         _E.Case($moduleName,
         "between lines 204 and 206");
      }();
   };
   var mapBulletCollisions = F2(function (b,
   es) {
      return function () {
         var bools = A2(List.map,
         checkCollision(b),
         es);
         var b$ = A2(List.any,
         function (n) {
            return n;
         },
         bools);
         var es$ = List.concat(A2(List.map,
         handleBulletCollision,
         A2(List.zip,es,bools)));
         return {ctor: "_Tuple2"
                ,_0: es$
                ,_1: b$};
      }();
   });
   var stepGame = F2(function (_v26,
   _v27) {
      return function () {
         return function () {
            return function () {
               var nextSurf = nextSurface(_v27.surface);
               var bulletCollision = _v27.bullet.active ? A2(mapBulletCollisions,
               _v27.bullet,
               _v27.enemies) : {ctor: "_Tuple2"
                               ,_0: _v27.enemies
                               ,_1: false};
               var e$ = Basics.fst(bulletCollision);
               var b$ = Basics.not(_v27.bullet.active) && (_v26.space && _U.eq(_v27.state,
               Play)) ? createBullet(_v27.player) : _v27.bullet;
               var b$$ = Basics.not(Basics.snd(bulletCollision)) ? b$ : _U.replace([["active"
                                                                                    ,false]],
               b$);
               var stuck = _U.eq(Basics.abs(_v27.player.x),
               outerXMax) && _U.eq(Basics.abs(_v27.player.y),
               outerYMax);
               var p$ = stuck ? _U.replace([["x"
                                            ,0]
                                           ,["y",0]],
               _v27.player) : _v27.player;
               var playerCollision = A2(mapPlayerCollisions,
               p$,
               e$);
               var state$ = _U.eq(List.length(e$),
               0) ? Win : playerCollision ? Lose : _v27.state;
               var e$$ = _U.eq(state$,
               Play) ? A2(List.map,
               A2(stepEnemy,
               getSurface(_v27.surface),
               _v26),
               e$) : !_U.eq(state$,
               Play) && _v26.space ? defaultEnemies : e$;
               var b$$$ = _U.eq(state$,
               Play) ? A3(stepBullet,
               getSurface(_v27.surface),
               _v26,
               b$$) : _U.replace([["active"
                                  ,false]],
               b$$);
               var surface$ = _U.eq(state$,
               Win) && _v26.space ? nextSurf : _v27.surface;
               var state$$ = !_U.eq(state$,
               Play) && _v26.space ? Play : _U.eq(state$,
               Win) && _U.eq(surface$,
               Chaosphere) ? Victory : state$;
               var p$$ = _U.eq(state$,
               Play) ? A3(stepPlayer,
               getSurface(_v27.surface),
               _v26,
               p$) : !_U.eq(state$,
               Play) && _v26.space ? defaultPlayer : p$;
               return _U.replace([["player"
                                  ,p$$]
                                 ,["enemies",e$$]
                                 ,["bullet",b$$$]
                                 ,["surface",surface$]
                                 ,["state",state$$]],
               _v27);
            }();
         }();
      }();
   });
   var drawEnemy = function (e) {
      return function () {
         var e$ = _U.eq(e.size,
         Large) ? Graphics.Collage.outlined(Graphics.Collage.solid(Color.white))(A2(Graphics.Collage.ngon,
         12,
         35)) : _U.eq(e.size,
         Medium) ? Graphics.Collage.outlined(Graphics.Collage.solid(Color.white))(A2(Graphics.Collage.ngon,
         6,
         25)) : _U.eq(e.size,
         Small) ? Graphics.Collage.outlined(Graphics.Collage.solid(Color.white))(A2(Graphics.Collage.ngon,
         3,
         10)) : _E.If($moduleName,
         "between lines 346 and 348");
         return Graphics.Collage.rotate(Basics.degrees(e.angle))(Graphics.Collage.move({ctor: "_Tuple2"
                                                                                       ,_0: e.x
                                                                                       ,_1: e.y})(e$));
      }();
   };
   var drawEnemies = function ($) {
      return Graphics.Collage.group(List.map(drawEnemy)($));
   };
   var drawGame = function (_v30) {
      return function () {
         return function () {
            var i$ = _U.eq(_v30.state,
            Begin) ? Graphics.Collage.move({ctor: "_Tuple2"
                                           ,_0: 0
                                           ,_1: 100})(drawInstruction) : Graphics.Collage.toForm(A2(Graphics.Element.spacer,
            1,
            1));
            var v$ = _U.eq(_v30.state,
            Victory) ? Graphics.Collage.move({ctor: "_Tuple2"
                                             ,_0: 0
                                             ,_1: 100})(drawVictory) : Graphics.Collage.toForm(A2(Graphics.Element.spacer,
            1,
            1));
            var bullet$ = Basics.not(_v30.bullet.active) ? Graphics.Collage.toForm(A2(Graphics.Element.spacer,
            1,
            1)) : Graphics.Collage.move({ctor: "_Tuple2"
                                        ,_0: _v30.bullet.x
                                        ,_1: _v30.bullet.y})(drawBullet);
            return A3(Graphics.Collage.collage,
            800,
            600,
            _J.toList([background
                      ,Graphics.Collage.rotate(Basics.degrees(_v30.player.angle))(Graphics.Collage.move({ctor: "_Tuple2"
                                                                                                        ,_0: _v30.player.x
                                                                                                        ,_1: _v30.player.y})(drawPlayer(_v30.player.rev)))
                      ,drawEnemies(_v30.enemies)
                      ,bullet$
                      ,v$
                      ,i$
                      ,Graphics.Collage.move({ctor: "_Tuple2"
                                             ,_0: 0
                                             ,_1: 200})(formatText(String.show(_v30.state)))
                      ,Graphics.Collage.move({ctor: "_Tuple2"
                                             ,_0: 0
                                             ,_1: -200})(formatText(String.show(_v30.surface)))
                      ,foreground]));
         }();
      }();
   };
   var GameObject = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return _U.insert("rev",
      f,
      _U.insert("angle",
      e,
      _U.insert("vy",
      d,
      _U.insert("vx",
      c,
      _U.insert("y",
      b,
      _U.insert("x",a,g))))));
   });
   var Input = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,dt: d
             ,dx: b
             ,dy: c
             ,space: a};
   });
   var input = A2(Signal.sampleOn,
   delta,
   A2(Signal._op["~"],
   A2(Signal._op["~"],
   A2(Signal._op["~"],
   A2(Signal._op["<~"],
   Input,
   Keyboard.space),
   A2(Signal.lift,
   function (_) {
      return _.x;
   },
   Keyboard.arrows)),
   A2(Signal.lift,
   function (_) {
      return _.y;
   },
   Keyboard.arrows)),
   delta));
   var gameState = A3(Signal.foldp,
   stepGame,
   defaultGame,
   input);
   var main = A2(Signal.lift,
   drawGame,
   gameState);
   _elm.Main.values = {_op: _op
                      ,defaultGame: defaultGame
                      ,defaultPlayer: defaultPlayer
                      ,defaultEnemy1: defaultEnemy1
                      ,defaultEnemy2: defaultEnemy2
                      ,defaultEnemy3: defaultEnemy3
                      ,defaultEnemy4: defaultEnemy4
                      ,defaultEnemies: defaultEnemies
                      ,testEnemies: testEnemies
                      ,defaultBullet: defaultBullet
                      ,delta: delta
                      ,input: input
                      ,thrustFactor: thrustFactor
                      ,outerXMax: outerXMax
                      ,innerXMax: innerXMax
                      ,outerYMax: outerYMax
                      ,innerYMax: innerYMax
                      ,doV: doV
                      ,doH: doH
                      ,reflectAngle: reflectAngle
                      ,findDistance: findDistance
                      ,enemySize: enemySize
                      ,nextSurface: nextSurface
                      ,getSurface: getSurface
                      ,rectangle: rectangle
                      ,cylinder: cylinder
                      ,mobius: mobius
                      ,torus: torus
                      ,klein: klein
                      ,chaosphere: chaosphere
                      ,collideRect: collideRect
                      ,collideCyl: collideCyl
                      ,collideMobius: collideMobius
                      ,checkCollision: checkCollision
                      ,mapPlayerCollisions: mapPlayerCollisions
                      ,mapBulletCollisions: mapBulletCollisions
                      ,handleBulletCollision: handleBulletCollision
                      ,splitEnemy: splitEnemy
                      ,applyThrust: applyThrust
                      ,enemyMovement: enemyMovement
                      ,movePlayer: movePlayer
                      ,createBullet: createBullet
                      ,stepBullet: stepBullet
                      ,stepEnemy: stepEnemy
                      ,stepPlayer: stepPlayer
                      ,stepGame: stepGame
                      ,gameState: gameState
                      ,background: background
                      ,foreground: foreground
                      ,drawPlayer: drawPlayer
                      ,drawBullet: drawBullet
                      ,drawEnemy: drawEnemy
                      ,drawEnemies: drawEnemies
                      ,prettyStats: prettyStats
                      ,formatText: formatText
                      ,drawVictory: drawVictory
                      ,drawInstruction: drawInstruction
                      ,drawGame: drawGame
                      ,main: main
                      ,Small: Small
                      ,Medium: Medium
                      ,Large: Large
                      ,Play: Play
                      ,Win: Win
                      ,Lose: Lose
                      ,Begin: Begin
                      ,Victory: Victory
                      ,LeftRight: LeftRight
                      ,TopBottom: TopBottom
                      ,Rectangle: Rectangle
                      ,Cylinder: Cylinder
                      ,Torus: Torus
                      ,Mobius: Mobius
                      ,Klein: Klein
                      ,Chaosphere: Chaosphere
                      ,Input: Input
                      ,GameObject: GameObject
                      ,Game: Game};
   return _elm.Main.values;
};
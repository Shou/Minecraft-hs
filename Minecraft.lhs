---
title: "Generating castles for Minecraft™ using Haskell"
published: 2019-07-25T08:00:00Z
tags: Minecraft, haskell, DSL, graphics, code generation
description: "Building up a little language with the goal of generating a variety of castles for the Minecraft™ game."
---

Introduction
------------

My kids and I have enjoyed building various structures in the game Minecraft, but it can get rather monotonous placing blocks one-at-a-time. I took a very brief look at the custom "mods" available, but wanted something more compositional, hence the idea for a Haskell DSL was born. The aim is to build a skeleton structure, thereby eliminating the most repetitive parts of manual construction, for example walls, floors, roofs etc. We can then enjoy furnishing the structures by hand in the actual game.

I haven't yet distilled down the optimal set of primitives and combinators for all buildings/structures, but what I have so far seems usable and sufficient for generating castles such as these:

![English Castle](../img/minecraft/english_castle1.png "English Castle")

I use some continuous floating-point maths for computing circles and spirals, but it seems to work fine even when rasterized down to a small number of Minecraft blocks.

Finally, note that I have only ever tested this on the original Java version of Minecraft.

Preliminaries
-------------

Before we model the above in Haskell, let's enable some extensions and import some modules:

>{-# LANGUAGE GeneralizedNewtypeDeriving #-}
>{-# LANGUAGE RankNTypes #-}
>{-# LANGUAGE RecordWildCards #-}
>{-# LANGUAGE ScopedTypeVariables #-}
>{-# LANGUAGE TemplateHaskell #-}
>{-# LANGUAGE ViewPatterns #-}
>{-# LANGUAGE OverloadedLists #-}
>{-# LANGUAGE TypeApplications #-}
>{-# LANGUAGE BlockArguments #-}

> module Minecraft where

> import           Prelude hiding (repeat, replicate, floor)
> import qualified Prelude as Prelude
> import           Control.Arrow
> import           Control.Lens (Lens', view, over, set, makeLenses)
> import           Control.Monad
> import qualified Control.Monad.Trans.State as State
> import qualified Control.Monad.IO.Class as IO
> import qualified Control.Monad.Loops as Loops
> import Data.Coerce
> import Data.Fixed (mod')
> import qualified Debug.Trace as Trace
> import           System.FilePath
> import           System.IO
> import           System.Random
> import           Text.Printf
> import Data.Functor ((<&>))
> import qualified Data.Map as Map
> import Data.Function ((&)) -- only scrubs align their code
> import qualified Data.List as List

We will also use lenses to represent the three dimensions. This will allow us to write code that can read and modify an abstracted dimension using only one function parameter, rather than passing getters and setters around. The only lens library functions we will use are as follows:

~~~{.haskell}
view :: Lens' a b -> a -> b
over :: Lens' a b -> (b -> b) -> a -> a
set  :: Lens' a b ->       b  -> a -> a
~~~

Data types
----------

The basic atom in Minecraft is the block. Each block has coordinates, a kind (e.g. air, cobblestone, water) and some optional state (which we won't make use of here).

> type Kind  = String
> type State = String

> data Block = Block
>     { _blockCoord :: Coord
>     , _blockKind  :: Kind
>     , _blockState :: Maybe State
>     }
>     deriving Show

> data Coord = Coord { _x :: Int, _y :: Int, _z :: Int }
>     deriving (Ord, Eq)

> instance Show Coord where
>     show (Coord x y z) = show (x, y, z)

> makeLenses ''Coord
> makeLenses ''Block

Minecraft structures are represented as an ordered list of blocks, wrapped in a newtype, in order to hide the underlying representation. We will be working with lists of Blocks soon and so the newtype helps us distinguish between the layers.

> newtype Blocks = Blocks { unBlocks :: [Block] }
>     deriving (Semigroup, Monoid, Show)

> overN :: forall a b . Coercible a b => (b -> b) -> a -> a
> overN f = coerce @a @b >>> f >>> coerce @b @a

> mkBlocks :: [(Coord, Maybe State)] -> Blocks
> mkBlocks = Blocks . map (\(c, ms) -> Block c cobblestone ms)

> -- | A block of nothing (air) at the origin (0,0,0)
> zero :: Blocks
> zero = Blocks [Block (Coord 0 0 0) air Nothing]

> mapBlocks :: (Block -> Block) -> Blocks -> Blocks
> mapBlocks f = Blocks . map f . unBlocks

> (#&) :: (Block -> Block) -> Blocks -> Blocks
> (#&) = mapBlocks

> mapKind :: (Kind -> Kind) -> Blocks -> Blocks
> mapKind f = mapBlocks $ over blockKind f

We set the kind of block using an infix `#` operator:

> -- | Set the kind of all blocks
> infixr 8 #
> (#) :: Blocks -> Kind -> Blocks
> (#) blocks k = mapKind (const k) blocks

We derive semigroup and monoid using the underlying list instances. The semantics of the Blocks monoid is that of a non-commutative monoid, the right-hand-side overrides the left. For example:

~~~{.haskell}
zero <> (zero # cobblestone) -- results in a cobblestone block at (0,0,0)
(zero # cobblestone) <> zero -- results in nothing (an air block) at (0,0,0)
~~~

Abstracting over dimensions using lenses
----------------------------------------

We will abstract over dimensions using lenses:

> type Dimension = Lens' Coord Int

We have the lenses `x`, `y` and `z` automatically generated for us using Template Haskell. Note that Minecraft uses the convention where `x` and `z` are in the horizontal plane and `y` is the height. This convention continues to confuse me and was the cause of most of the bugs in my structure generation. I follow it here, only to be consistent with Minecraft.

The use of lenses means that we can use a single dimension parameter for functions that read coordinates, update them or both.

> -- | Move blocks by 'i' in dimension 'd'.
> move :: Dimension -> Int -> Blocks -> Blocks
> move d i = mapBlocks $ over (blockCoord . d) (+i)

> -- | Translate blocks by the supplied 'x, y, z' offset.
> translate :: Int -> Int -> Int -> Blocks -> Blocks
> translate x' y' z' = move x x' . move y y' . move z z'

> -- | Get the coordinate bounds for blocks along a particular dimension 'd'.
> bounds :: Dimension -> Blocks -> (Int, Int)
> bounds d blocks = (minimum ps, maximum ps)
>   where
>     ps = map (view $ blockCoord . d) $ unBlocks blocks

This centering function is particularly useful and is simplified by using a single dimension lens parameter:

> -- | Centre blocks on the origin of the supplied dimension.
> centre :: Dimension -> Blocks -> Blocks
> centre d blocks = move d (- (w `div` 2) - mn) blocks
>   where
>     w        = mx - mn
>     (mn, mx) = bounds d blocks

> centre_xz :: Blocks -> Blocks
> centre_xz = centre x . centre z


Building our first structure
----------------------------

To build structures we will need *combinators* that provide us with repetition. The simplest one that sprang to my mind was the following:

> -- | Repeat structure 'n' times with function 'f' applied iteratively.
> repeat :: (Blocks -> Blocks) -> Int -> Blocks -> Blocks
> repeat f n = mconcat . take n . iterate f

I then used `repeat` to define a combinator that will replicate a structure using a particular spacing (probably because I had castle *crenellations* in mind!).

> -- | replicate structure 'n' times with a spacing 'i' in dimension 'd'.
> replicate :: Dimension -> Int -> Int -> Blocks -> Blocks
> replicate d i = repeat (move d i)

A common use for `replicate` would be to define a simple line of blocks, which we will default to cobblestone:

> -- | Create a line of cobblestone blocks with length 'n' along dimension 'd'.
> line :: Dimension -> Int -> Blocks
> line d n = replicate d 1 n zero # cobblestone

Similarly we can define walls, floors and squares of cobblestone:

> wall :: Dimension -> Int -> Int -> Blocks
> wall d w h
>     = replicate y 1 h
>     . replicate d 1 w
>     $ zero # cobblestone

> floor :: Int -> Int -> Blocks
> floor wx wz
>     = replicate x 1 wx
>     . replicate z 1 wz
>     $ zero # cobblestone

> cube :: Int -> Int -> Int -> Blocks
> cube w h t
>   = replicate x 1 t 
>   $ wall z w h

> square :: Int -> Blocks
> square w =
>     l x <> move z (w-1) (l x) <>
>     l z <> move x (w-1) (l z)
>   where
>     l :: Dimension -> Blocks
>     l d = line d w

> rect :: Int -> Int -> Blocks
> rect wx wz = mconcat
>   [ line x wx
>   , move z (wz-1) $ line x wx
>   , line z wz
>   , move x (wx-1) $ line z wz
>   ]

> wideRect :: Int -> Int -> Int -> Blocks
> wideRect t wx wz = mconcat
>   [ translate i 0 i $ rect (wx-2*i) (wz-2*i)
>   | i <- [0..t-1]
>   ]

> squareWall :: Int -> Int -> Blocks
> squareWall w h = repeat (move y 1) h (square w)

> -- | A square of thickness 't' and width 'w'.
> wideSquare :: Int -> Int -> Blocks
> wideSquare t w = mconcat
>     [ translate i 0 i $ square (w-2*i)
>     | i <- [0..t-1]
>     ]

A square of crenellations is similar to a square, but we place the blocks with a spacing of two. It works best with odd widths, e.g. 5, 7, 9 etc.

> squareCrenellations :: Int -> Blocks
> squareCrenellations w =
>     l x <> move z (w-1) (l x) <>
>     l z <> move x (w-1) (l z)
>   where
>     l :: Dimension -> Blocks
>     l d = replicate d 2 (w `div` 2 + w `rem` 2) blk
>     blk = zero # cobblestone

We now have enough to define a square turret complete with windows and crenellations!

> squareTurret :: Int -> Int -> Blocks
> squareTurret w h = mconcat
>     [ squareWall w h
>     , translate (-1) h (-1) $ mconcat
>         [ floor w' w'
>         , squareWall w' 2
>         , move y 2 (squareCrenellations w')
>         ]
>     , translate (w `div` 2) 1    0  windows
>     , translate (w `div` 2) 1 (w-1) windows
>     ]
>   where
>     w'      = w + 2
>     windows = replicate y 3 (h `div` 3) zero

To get this structure into Minecraft, we need to generate an ".mcfunction" text file of commands that can be executed by the game. The only command we will use is "setblock" which takes three coordinates and a block kind. But before Minecraft will load such a file, we need to create a "datapack" inside a particular level. The level I used was called "Castles" and for my Linux machine, Minecraft saved game state was stored in `~/.minecraft`. I created the following directories:

~~~{.bash}
[tim@x1c:~/.minecraft/saves/Castle/datapacks]$ find
.
./haskell
./haskell/data
./haskell/data/haskell
./haskell/data/haskell/functions
./haskell/pack.mcmeta
~~~

The content of `pack.mcmeta` was as follows:

~~~
{
 "pack": {
 "pack_format": 3,
 "description": "Tim's data pack"
 }
}
~~~

Before we generate the mcfunction file, we first prune the block list to get rid of any overridden blocks (alternatively we could write a new monoid instance to do this as it goes along).

> -- | Removes unnecessary setblock instructions from the list.
> prune :: Blocks -> Blocks
> prune = Blocks . Map.elems . Map.fromList . map (_blockCoord &&& id) . unBlocks

Finally here is the "render" function for generating the commands:

> render :: FilePath -> String -> String -> Coord -> Blocks -> IO ()
> render minecraftDir levelName functionName Coord{..} (prune -> blocks) = do
>     writeFile mcmetaFile packMcMeta
>     withFile filePath WriteMode $ \hnd ->
>         let isRelative = _x == 0 && _y == 0 && _z == 0
>         in forM_ (unBlocks $ translate _x _y _z blocks) $ \(Block Coord{..} kind mstate) ->
>           let shower a = if isRelative then "~" <> show a else show a
>           in hPutStrLn hnd $ printf "setblock %s %s %s %s[%s]"
>               (shower _x) (shower _y) (shower _z) kind (foldMap id mstate)
>   where
>     filePath = foldr @[] (</>) (functionName ++ ".mcfunction")
>                    [ minecraftDir, "saves", levelName, "datapacks"
>                    , "haskell", "data", "haskell",  "functions" ]
>     mcmetaFile = foldr1 @[] (</>)
>       [ minecraftDir, "saves", levelName, "datapacks"
>       , "haskell", "pack.mcmeta"
>       ]
>     packMcMeta = "{\"pack\": {\"pack_format\": 1, \"description\": \"Haskell functions\" } }"

> renderToFlatpak = render "/home/benedict/.var/app/com.mojang.Minecraft/.minecraft/"
> renderRelative name = renderToFlatpak "Haskell" name (Coord 0 0 0)

To render our square turret, first load Minecraft, then enter the level you have the datapack installed into and then press F3 to find your current coordinates. You can then give `render` these coordinates at the haskell prompt:

~~~{.haskell}
λ> let t = square_turret 9 15
λ> render "~/.minecraft" "Castles" "square_turret" (Coord (-763) 4 1305) t
~~~

From within Minecraft, press `t` to bring up the prompt and enter the following (tab completion should work).

~~~
/reload
/function haskell:square_turret
~~~

If Minecraft cannot see the mcfunction file, check that it can see the `[file/haskell]` datapack by entering `/datapack list`. If all is well, you should see something like this:

![Square Turret](../img/minecraft/square_turret.png "Square Turret")

Note that to remove the turret from the game, we can re-render it using `# air` to set all blocks to empty (air).


Circles, Spirals and Cones
--------------------------

A square turret obviously works well in Minecraft, but I wondered how effective it would be to attempt to rasterize a circular turret. I tried the simplest implementation I could think of:

> circle :: Int -> Int -> Blocks
> circle r steps = translate r 0 r $
>     mkBlocks [ (Coord x 0 z, Nothing)
>              | s <- [1..steps]
>              , let phi = (2*pi*fromIntegral s) / fromIntegral steps :: Double
>                    z   = round $ fromIntegral r * cos phi
>                    x   = round $ fromIntegral r * sin phi
>              ]

> circleWall :: Int -> Int -> Int -> Blocks
> circleWall r h steps =
>     repeat (move y 1) h (circle r steps)

Note that the number of steps can be varied, with low values being useful for circular placement of windows and crenellations. A solid circle is even easier:

> circleFloor :: Int -> Blocks
> circleFloor r = translate r 0 r $
>     mkBlocks [ (Coord x 0 z, Nothing)
>              | x <- [-r..r]
>              , z <- [-r..r]
>              , let d = sqrt (fromIntegral $ x*x + z*z) :: Double
>              , d <= fromIntegral r
>              ]

> circleFloor' :: Int -> Blocks
> circleFloor' radius = mconcat
>   [ circle r (r * 2 * 6) & translate (radius - r) 0 (radius - r)
>   | r <- [0 .. radius]
>   ] <> (floor 2 2 & translate radius 0 radius)

> cylinder :: Int -> Int -> Blocks
> cylinder r h = repeat (move y 1) h (circleFloor r)

It would be nice to also create a staircase inside the circular turret, which we can generate using a spiral:

> spiral :: Int -> Int -> Int -> Int -> Int -> Blocks
> spiral r start h revs steps = translate r 0 r $
>     mkBlocks [ (Coord x y z, Nothing)
>              | s   <- [1..steps]
>              , let phi = (2*pi*fromIntegral (revs*s)) / fromIntegral steps :: Double
>                    z   = round $ fromIntegral r * cos phi
>                    x   = round $ fromIntegral r * sin phi
>                    y   = round $ fromIntegral start + fromIntegral (h*s) / (fromIntegral steps :: Double)
>              ]

r : Radius
t : "Line width", the width of the staircase
h : Height
revs : Spiral revolutions
steps : Quantity of blocks in total, gets hole-y with smaller numbers

Example: wideSpiral 30 30 50 3 1000

> wideSpiral :: Int -> Int -> Int -> Int -> Int -> Int -> Blocks
> wideSpiral r t start h revs steps = mconcat
>     [ translate i 0 i $ spiral (r-i) start h revs steps
>     | i <- [0..t-1]
>     ]

> wideSlabSpiral :: Int -> Int -> Int -> Int -> Int -> Blocks
> wideSlabSpiral r t h revs steps = mconcat
>   [ translate i 0 i $ slabSpiral (r-i) h revs steps
>   | i <- [0..t-1]
>   ]
>   where
>   nubber bs =
>     let tops = filter ((== Just "type=top") <<< view blockState) bs
>         bottoms = filter ((== Just "type=bottom") <<< view blockState) bs
>     in List.unionBy (\a b -> view blockCoord a == view blockCoord b) bottoms tops
>   slabSpiral :: Int -> Int -> Int -> Int -> Blocks
>   slabSpiral r h revs steps = translate r 0 r $
>     mkBlocks [ (Coord x y z, state)
>              | s   <- [1..steps]
>              , let phi = (2*pi*fromIntegral (revs*s)) / fromIntegral steps :: Double
>                    z = round $ fromIntegral r * cos phi
>                    x = round $ fromIntegral r * sin phi
>                    y' = fromIntegral (h*s) / (fromIntegral steps :: Double)
>                    y = Prelude.floor y'
>                    state = Just $ mappend "type=" if mod' y' 1.0 >= 0.5 then "top" else "bottom"
>              ] # slabify smooth_stone

> spirograph (fromIntegral -> bigR) (fromIntegral -> r) (fromIntegral -> a) steps =
>   mkBlocks [ (Coord x 0 z, Nothing)
>            | t <- [1..steps]
>            , let z = round $ (bigR - r) * cos (r/bigR * t) + a * cos ((1-r/bigR) * t)
>                  x = round $ (bigR - r) * sin (r/bigR * t) - a * sin ((1-r/bigR) * t)
>            ]

> spirograph3d (fromIntegral -> bigR) (fromIntegral -> r) (fromIntegral -> a) t (fromIntegral -> h) revs steps =
>   mkBlocks [ (Coord x' y z', Nothing)
>            | s <- fromIntegral <$> [1 .. steps]
>            , w <- fromIntegral <$> [-t .. t]
>            , let y = round $ (h * s) / dSteps
>                  z = (bigR - r) * cos (r/bigR * s) + a * cos ((1-r/bigR) * s)
>                  x = (bigR - r) * sin (r/bigR * s) - a * sin ((1-r/bigR) * s)
>                  phi' = 2 * pi * (fromIntegral y/h)
>                  x' = round $ w + (x * cos phi' + z * sin phi')
>                  z' = round $ w + (z * cos phi' - x * sin phi')
>            ]
>   where
>     dSteps :: Double
>     dSteps = fromIntegral steps

> weed r steps =
>   mkBlocks [ (Coord x 0 z, Nothing)
>            | s <- fromIntegral <$> [1 .. steps]
>            , let t = 2 * pi / fromIntegral steps * s - pi
>                  z = round $ (1 + 0.9 * cos (8 * t)) * (1 + 0.1 * cos (24 * t))
>                  x = round $ r * (0.9 + 0.05 * cos (200 * t)) * (1 + sin t)
>            ]

My circular turret with a spiral staircase, crenellations, windows and a top floor with exit hole was thus:

Example: circularTurret 3 15 20

> circularTurret :: Int -> Int -> Int -> Blocks
> circularTurret r h steps = mconcat
>     [ cylinder r h    # air -- clear space
>     , translate 1 0 1 $ wideSpiral (r-1) 3 0 h 3 (3*steps) -- spiral staircase
>     , circleWall r h steps
>     , translate (-1) h (-1) (circleFloor r' <> -- upper floor
>                              circleWall r' 2 (2 * steps) <> -- upper wall
>                              move y 2 (circle r' (steps `div` 2))) -- crenellations
>     , translate 2 h 2 $ floor 3 3 # air -- exit for staircase
>     , move y 1 $ repeat (move y (h `div` 3)) 3 (circle r 4) # air -- windows
>     ]
>   where r' = r + 1

In Minecraft, the circular turret has rasterized reasonably well and the spiral staircase inside works well:


![Circular Turret](../img/minecraft/circular_turret.png "Circular Turret")


![Spiral Staircase](../img/minecraft/spiral_staircase.png "Spiral Staircase")


My kids prefer the Germanic style of castle, most often seen in Disney movies, so I had a go at adding a conic top to the circular turret:

> cone :: Int -> Int -> Int -> Blocks
> cone r h steps = mconcat
>     [ translate (r - r') y (r - r') $ circle r' steps
>     | y <- [0..h]
>     , let r' = round $ fromIntegral (r*(h-y)) / (fromIntegral h :: Double)
>     ]

> circularTurret_germanic :: Int -> Int -> Int -> Blocks
> circularTurret_germanic r h steps =
>     circularTurret r h steps <>
>     translate (-1) h (-1)
>          (move y 1 (circle r' 4) # air <> -- top windows
>           move y 2 (cone r' 8 (2 * steps) # bricks)) -- cone roof
>   where r' = r + 1

![Germanic Circular Turret](../img/minecraft/germanic_circular_turret.png "Germanic Circular Turret")


Grid Layouts
------------

There are a whole host of layout combinators that we could imagine being useful, however for castles, a grid layout combinator should probably suffice. My implementation is:

> grid :: Int -> [[Blocks]] -> Blocks
> grid spacing = f z . map (f x . map centre_xz)
>   where
>     f :: Dimension -> [Blocks] -> Blocks
>     f d = foldr (\a b -> a <> move d spacing b) mempty

Note that each grid item is centered horizontally on the origin before it is moved into position.


The Castle Keep
---------------

The castle keep is a large fortified building in the centre of the grounds. We'll build ours from four turrets, four walls, three floors and an archway entrance.

> castleKeep :: Blocks -> Int -> Int -> Blocks
> castleKeep t w h = mconcat
>     [ floors
>     , squareWall w h
>     , move y h (squareCrenellations w)
>     , grid (w-1) [ [ t,  t]
>                  , [ t,  t]
>                  ]
>     -- make a larger archway from default stone that juts out,
>     -- before overlaying the smaller empty space one.
>     , translate (w `div` 2) 0 w $ centre x $ archway 3 2
>     , translate (w `div` 2) 0 w $ centre x $ archway 2 3 # air
>     ]
>   where
>     floors
>         = translate 1 (-1) 1
>         . replicate y 6 3
>         $ floor w' w' # oak_planks
>     w' = w - 2

The archway was tricky to do. I started by defining some simple right-angle rotations, to allow me to rotate my circle floors (used for the circular turrets).

> rotate_x, rotate_y :: Blocks -> Blocks
> rotate_x = mapBlocks $ over blockCoord $ \(Coord x y z) -> Coord x z y
> rotate_y = mapBlocks $ over blockCoord $ \(Coord x y z) -> Coord y x z

X,Z rotation to a degree angle.

> angleCoord :: Int -> Coord -> Coord
> angleCoord deg (Coord x y z) =
>   let theta = fromIntegral deg * (pi / 180)
>       x' = round $ cos theta * fromIntegral x + sin theta * fromIntegral x
>       z' = round $ negate (sin theta) * fromIntegral z + cos theta * fromIntegral z
>   in Coord x' y z'

> angle :: Int -> Blocks -> Blocks
> angle deg = mapBlocks $ over blockCoord (angleCoord deg)

An archway can then be made by rotating the circular floor and replicating it to achieve thickness:

> -- | Make a solid archway of radius 'r' and thickness 't'.
> archway :: Int -> Int -> Blocks
> archway r t
>     = replicate z 1 t
>     $ rotate_x (circleFloor r <> floor (2*r + 1) r)

Notice that the `castleKeep` definition is parameterised by a turret structure.

![Castle Keep](../img/minecraft/castle_keep.png "Castle Keep")


A Full Castle
-------------

The most important component of a English castle is the outer castle wall, so I made the one below quite elaborate. The wall has an archway entrance, an inner and outer skin, an overhang, crenellations and a wall-top fence. Feel free to add arrow slit windows and perhaps inner platforms, ladders and doors!

> castleWall :: Int -> Int -> Blocks
> castleWall w h = mconcat
>     [ squareWall w h                             -- outer wall
>     , translate 3    0  3 (squareWall (w-6) h)   -- inner wall
>     , translate 1 (h-1) 1 (wideSquare 2 (w-2))   -- roof
>     , translate (-1) (h-1) (-1)
>         (squareWall (w+2) 2 <> move y 2 (squareCrenellations (w+2))) -- overhangs
>     , translate 3 h 3 (square (w-6) # oak_fence)  -- wall top fencing
>     -- since the wall is hollow we make a larger archway section
>     -- out of default stone before we overlay the smaller empty space one
>     , translate (w `div` 2) 0 (w-3) $ centre x $ archway 5 2
>     , translate (w `div` 2) 0 (w-4) $ centre x $ archway 4 4 # air
>     ]

To create the final full castle, we start with the castle wall and then surround the keep by outer turrets and a gatehouse (two turrets close together at the entrance).

> englishCastle :: Blocks
> englishCastle = mconcat
>     [ castleWall w h
>     , grid (w `div` 2)
>         [ [ t,  t,  t]
>         , [ t,  k,  t]
>         , [ t,  g,  t]
>         ]
>     ]
>   where
>     t  = circularTurret 4 15 20
>     k  = castleKeep (circularTurret 3 15 20) kw kh
>     w  = 100 -- castle
>     h  = 10  -- wall height
>     kw = 24  -- keep width
>     kh = 15  -- keep height
>     -- gatehouse entrance has two turrets together
>     g  = move x (-12) t <> move x 12 t


![English Castle](../img/minecraft/english_castle2.png "English Castle")


A Mossy Castle
--------------

Once we have generated a structure, we can modify it, perhaps by applying a random process. One idea is to make the castle mossy (see below), but one could also imagine many other effects such as creating a castle ruin!

> randomise :: Double -> (Kind, Kind) -> Blocks -> IO Blocks
> randomise p (from, to) (Blocks bs) =
>     Blocks <$> mapM f bs
>   where
>     f b | view blockKind b == from = do
>               r <- randomIO
>               return $ if r < p
>                        then set blockKind to b
>                        else b
>         | otherwise = return b

> mossy :: Blocks -> IO Blocks
> mossy = randomise 0.2 (cobblestone, mossy_cobblestone)

![Mossy English Castle](../img/minecraft/mossy_english_castle1.png "Mossy English Castle")

![Mossy English Castle](../img/minecraft/mossy_english_castle2.png "Mossy English Castle")


More Castles
------------

We can easily generate lots of variations on the castle theme, by changing the function parameters.
For example, a Germanic castle, using our earlier Germanic turret:

![Germanic Castle](../img/minecraft/germanic_castle.png "Germanic Castle")

A desert castle would be made of sandstone rather then cobblestone, thus we need a way to substitute a particular block kind for another:

> -- | Substitute one block for another, intended to be used infix, e.g.
> -- @ castle `subst` (cobblestone, sandstone) @
> subst :: (Kind, Kind) -> Blocks -> Blocks
> subst (from, to) blocks = mapKind f blocks
>   where
>     f k | k == from = to
>         | otherwise = k

![Desert Castle](../img/minecraft/desert_castle.png "Desert Castle")

> stairs :: Int -> Int -> String -> Blocks
> stairs start end direction = take (end - start) (iterate succ start) & foldMap ascend
>   where
>   ascend a = mapBlocks (set blockState (Just $ "facing=" <> direction)) (line x 1)
>     & subst (cobblestone, quartz_stairs)
>     & translate a a 0

> plazaSkyscraper :: Int -> Int -> Int -> Int -> Blocks
> plazaSkyscraper plazaRadius floors circum height = mconcat
>   [ replicate y 1 10 (wideSquare plazaRadius (circum + plazaRadius * 2)) # air
>   , wideSquare plazaRadius (circum + plazaRadius * 2) # smooth_stone
>   , skyscraper floors circum height & translate plazaRadius 0 plazaRadius
>   ]

> skyscraper :: Int -> Int -> Int -> Blocks
> skyscraper floors circum height =
>   mkFloors floors circum height
>     <> mkGround circum 4 4
>     <> mkRoof floors circum height
>
>   where
>     mkGround :: Int -> Int -> Int -> Blocks
>     mkGround r w h = foldMap @[] id $ foldMap @[] id $

Entrance openings

>       [ [ wall z w h & translate 0 1 (subtract 2 $ r `div` 2)
>         , wall z w h & translate r 1 (subtract 2 $ r `div` 2)
>         ] <&> (# air)

Slabs for the entrances

>       , [ line x w & translate (r `div` 2) 0 (-1)
>         , line z w & translate (-1) 0 (r `div` 2)
>         , line x w & translate (r `div` 2) 0 (succ r)
>         , line z w & translate (succ r) 0 (r `div` 2)
>         ] <&> subst (cobblestone, slabify smooth_stone)

Add the ground flooring.

>       , [ floor r r
>         ] <&> subst (cobblestone, smooth_stone)
>       ]
>
>     mkRoof n r h = foldMap @[] id $
>       [ replicate y 1 3 (square (succ r) # smooth_stone)
>       , floor (r - 1) (r - 1) # grass_block & translate 1 1 1
>       , floor (h + 2) 3 # smooth_stone & translate 3 1 1
>       , floor h 2 # air & translate 4 1 1
>       , replicate z 1 2 (stairs 0 1 "east" # quartz_stairs) & translate (h + 3) 1 1
>       , zero # "acacia_sapling"
>           & translate (r `div` 2) 2 (r `div` 2)
>           & mapBlocks (set blockState $ Just "stage=1")
>       ] <&> move y (n * h)
>
>     mkFloor :: Int -> Int -> Blocks
>     mkFloor r h = foldMap @[] id $ foldMap @[] id
>       [ [ wall x r h
>         , wall z r h
>         , wall x (succ r) h & move z r
>         , wall z (succ r) h & move x r

Carpet.

>         , floor (pred r) (pred r)
>             & translate 1 1 1
>             & subst (cobblestone, light_gray_carpet)

No carpet on stairs.

>         , floor (h-1) 2 & translate 4 1 1 & subst (cobblestone, air)

Ceiling.

>         , floor r r & translate 1 h 1
>         ] <&> subst (cobblestone, smooth_stone)

Windows!

>       , [ wall x (r - 1) (h - 2) & move x 1 & move y 2
>         , wall z (r - 1) (h - 2) & move z 1 & move y 2
>         , wall x (r - 1) (h - 2) & move x 1 & move y 2 & move z r
>         , wall z (r - 1) (h - 2) & move z 1 & move y 2 & move x r
>         ] <&> subst (cobblestone, glass_pane)

We make some ceiling lights out of glowstones, as repeating hollow squares.

>       , takeWhile (>0) (iterate (subtract 10) (r - 4)) <&> \radius -> foldMap @[] id
>           [ square radius
>               & translate (subtract radius r `div` 2) h (subtract radius r `div` 2)
>               & subst (cobblestone, glowstone)

We cover the ceiling lights next.

>           , wideSquare 1 radius
>               & translate (subtract radius r `div` 2) (h - 1) (subtract radius r `div` 2)
>               & subst (cobblestone, white_stained_glass)
>           ]

Stairs! And space for the stairs.

>       , [ [0 .. 2] & foldMap @[] \n ->
>             floor (h-1) 2 & translate 4 (h-n) 1 & subst (cobblestone, air)
>         , wall x (h - 2) h & translate 4 0 3 & subst (cobblestone, smooth_stone)
>         , stairs 1 (succ h) "east" & translate 2 0 1
>         , stairs 1 (succ h) "east" & translate 2 0 2
>         ]
>       ]
>
>     mkFloors n r h =
>       let ys = scanl (+) 0 (List.replicate (pred n) h)
>       in foldMap @[] (\y' -> mkFloor r h & move y y') ys

> plazaRoundSkyscraper :: Int -> Int -> Int -> Int -> Blocks
> plazaRoundSkyscraper plazaRadius radius floors height = mconcat
>   [ replicate y 1 10 (floor (2 * (plazaRadius + radius)) (2 * (plazaRadius + radius))) # air
>   , floor (2 * (plazaRadius + radius)) (2 * (plazaRadius + radius)) # smooth_stone
>   , roundSkyscraper radius floors height
>       & translate plazaRadius 0 plazaRadius
>   ]

> roundSkyscraper :: Int -> Int -> Int -> Blocks
> roundSkyscraper radius floors height = mconcat

Cool black glass exterior.

>   [ circleWall radius (floors * height) (radius * 2 * 4) # "black_stained_glass"

Every floor's flooring

>   , [0, height .. floors * height] & foldMap @[] \n -> circleFloor' radius # smooth_stone & move y n

Roof?

>   , circleFloor' radius # smooth_stone & move y (floors * height + 1)

Carpet!

>   , [0, height .. floors * height - height] & foldMap @[] \n -> circleFloor' (radius - 1) # light_gray_carpet & translate 1 (succ n) 1

Spiral staircase.

>   , wideSlabSpiral (radius - 1) 3 (floors * height) (floors `div` 2) (radius * 4 * 3 * floors)
>       & translate 1 1 1
>       & overN @Blocks @[Block] \blocks -> flip (foldMap @[]) blocks \block ->
>           let space = take 3 (iterate succ 1) <&> \n ->
>                 over blockCoord (over y (+n)) block
>                   & over blockKind (const air)
>                   & over blockState (const Nothing)
>           in space <> [block]

Ceiling lights.

>   , [height, height * 2 .. floors * height] & foldMap @[] \y' ->
>       takeWhile (>0) (iterate (subtract 5) (radius - 5)) & foldMap @[] \r -> mconcat
>         [ circle r (r * 2 * 4) # glowstone & move y y'
>         , circle r (r * 2 * 4) # white_stained_glass & move y (pred y')
>         ] & translate (radius - r) 0 (radius - r)

Entrances

>   , wall z (radius `div` 4 + 1) (height - 3) # air & translate 0 1 (radius - 1)
>   , replicate y 1 2 (circle radius (radius * 2 * 6) # smooth_stone) & move y (floors * height + 1)
>   ]

> plazaPool plazaR width length depth = mconcat
>   [ cube (length + plazaR * 2) 10 (width + plazaR * 2) # air
>   , wideRect plazaR (width + plazaR * 2) (length + plazaR * 2) # smooth_stone
>   , rect (succ width) (succ length) # slabify smooth_stone & translate (plazaR - 1) 1 (plazaR - 1)
>   , pool width length depth & translate (plazaR - 1) 0 (plazaR - 1)
>   ]

> pool width length depth = mconcat
>   [ cube (length + 2) (depth + 1) (width + 2) # smooth_stone & move y (-depth)
>   , cube length depth width # "water" & translate 1 (-(depth - 1)) 1
>   ]

> street :: Int -> Int -> Blocks
> street length lanes | lanes == 1 = floor 3 length & subst (cobblestone, gray_concrete)
> street length lanes = mconcat
>   [ sidewalk
>   , foldl @[] mkLanes mempty [1 .. lanes]
>   , floor 1 length & translate ((lanes * 4 + 8) `div` 2) 0 0 & subst (cobblestone, white_concrete)
>   , sidewalk & translate (lanes * 4 + 4) 0 0
>   , zero & translate 1 0 1 & subst (air, iron_trapdoor) & mapBlocks (set blockState (Just "half=top"))
>   , cube 3 3 3 & translate 0 (-3) 0
>   , wall x 3 3 & translate 0 (-6) 2
>   , line y 6 # ladder & translate 1 (-6) 1
>   , lamps
>   , lamps & translate (lanes * 4 + 4) 0 0
>   ]
>   where
>   mkLanes :: Blocks -> Int -> Blocks
>   mkLanes acc n = mconcat
>     [ acc
>     , lane & translate (n * 4 + 1) 0 0
>     , striped & translate (n * 4 + 4) 0 0
>     , replicate y 1 10
>         $ floor 4 length # air & translate (n * 4 + 1) 1 0
>     ]
>
>   lane = floor 3 length & subst (cobblestone, gray_concrete)
>   striped = takeWhile (<= length) [0, 3 .. ] & foldMap \n ->
>     floor 1 (min 3 (length - n))
>       & translate 0 0 n
>       & subst ( cobblestone
>               , if isEven n || (length - n < 3)
>                 then gray_concrete
>                 else white_concrete
>               )
>   lamps = takeWhile (<= length) [4, 12 .. ] & foldMap \n ->
>     mconcat
>       [ line y 4 # iron_bars & translate 0 1 0
>       , zero # redstone_lamp & translate 0 5 0
>       , zero # daylight_detector & translate 0 6 0 & mapBlocks (set blockState (Just "inverted=true"))
>       ] & translate 2 0 n
>   sidewalk = mconcat
>     [ cube length 10 5 # air
>     , floor 5 length # smooth_stone
>     ]
>   isEven n = n `mod` 2 == 0

> generateCity :: Int -> IO Blocks
> generateCity n = snd <$> flip State.runStateT mempty (generateCity' n)
>   where
>   generateCity' :: Int -> State.StateT Blocks IO ()
>   generateCity' 0 = pure ()
>   generateCity' n = do
>     d <- IO.liftIO $ (90*) <$> randomRIO @Int (0, 3)
>     roadLength <- IO.liftIO $ randomRIO @Int (50, 500)
>     roadLanes <- IO.liftIO $ (2*) <$> randomRIO @Int (2, 4)

Add a street which we build around.

>     State.modify $ mappend $ angle d $ street roadLength roadLanes
>     let Coord roadX _ roadZ =
>           angleCoord d $ Coord (4 * roadLanes - 1 + 10) 0 roadLength
>     structureSizes <- IO.liftIO $ Loops.iterateUntilM
>       (sum >>> (> roadLength - 75))
>       (\xs -> (:xs) <$> randomRIO (25, 75))
>       []
>     IO.liftIO $ print @[Int] structureSizes
>     void $ flip (`foldM` 0) structureSizes \acc size -> do
>       structureZero <- IO.liftIO $ join $ ($ size) <$> randomStructure
>       let structure = structureZero & translate (-size) 0 acc & angle d
>       State.modify $ mappend structure
>       pure (acc + size)
>   randomStructure :: IO (Int -> IO Blocks)
>   randomStructure = (structures !!) <$> randomRIO (0, length structures - 1)
>   structures :: [Int -> IO Blocks]
>   structures =
>     [ \size -> do
>         plazaR <- randomRIO $ plazaRange size
>         plazaSkyscraper plazaR
>           <$> randomRIO (5, 15)
>           <*> pure (size - plazaR * 2)
>           <*> randomRIO (5, 9)
>
>     , \size -> do
>         plazaR <- randomRIO $ plazaRange size
>         plazaRoundSkyscraper plazaR
>           <$> pure ((size - plazaR * 2) `div` 2)
>           <*> randomRIO (5, 15)
>           <*> randomRIO (5, 9)
>           <&> translate 1 0 0
>
>     , \size -> do
>         plazaR <- randomRIO $ plazaRange size
>         width <- randomRIO (5, 10)
>         plazaPool plazaR width
>           <$> pure ((size - plazaR * 2) `div` 2)
>           <*> randomRIO (1, 2)
>           <&> translate (size - (plazaR * 2 + width)) 0 0
>     ]
>   plazaRange s = (5, s `div` 5)


Have fun!

---

Note: the literal Haskell for this entire post can be found [here](https://raw.githubusercontent.com/willtim/timphilipwilliams.com/master/posts/2019-07-25-minecraft.lhs).

* * * * * * * *

Appendix
--------

<h4>A selection of Minecraft block kinds</h4>

> air = "air"
> anvil = "anvil"
> bed = "red_bed"
> bookshelf = "bookshelf"
> bricks = "bricks"
> chair = "oak_stairs"
> cobblestone = "cobblestone"
> crafting_table = "crafting_table"
> door_mat = "red_carpet"
> enchanting_table = "enchanting_table"
> furnace = "furnace"
> glass_pane = "glass_pane"
> ladder = "ladder"
> mossy_cobblestone = "mossy_cobblestone"
> oak_door = "oak_door"
> oak_fence = "oak_fence"
> oak_planks = "oak_planks"
> sandstone = "sandstone"
> spruce_stairs = "spruce_stairs"
> spruce_wood = "spruce_wood"
> table = "oak_planks"
> torch = "torch"
> quartz_block = "quartz_block"
> smooth_quartz = "smooth_quartz"
> smooth_stone = "smooth_stone"
> slabify = (<> "_slab")
> quartz_stairs = "quartz_stairs"
> glowstone = "glowstone"
> white_stained_glass = "white_stained_glass"
> light_gray_carpet = "light_gray_carpet"
> grass_block = "grass_block"
> gray_concrete = "gray_concrete"
> white_concrete = "white_concrete"
> iron_trapdoor = "iron_trapdoor"
> iron_bars = "iron_bars"
> redstone_lamp = "redstone_lamp"
> daylight_detector = "daylight_detector"

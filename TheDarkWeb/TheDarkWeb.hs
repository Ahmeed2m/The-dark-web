--main module loads the other modules then uses the functions inside them
module Main where

--importing the needed libraries
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Paths_FunGEn (getDataFileName)

--defining the gameState with a parameter (level)
data GameState = Level Int

--IOGame is a predefined function in the game engine (IOGame t s u v a)
--t is the type of the game special attributes
--s is the type of the object special attributes
--u is the type of the game levels
--v is the type of the map tile special attribute
--a is the type returned by each action of the game
--            IOGame t   s    u      v  a
type Move a = IOGame () () GameState () a

--w,h are the window dimensions casted to GLdouble (openGL uses GLdouble)
width = 800
height = 600
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

type RGB = Maybe [(Int,Int,Int)]

removeObjectBG :: RGB
removeObjectBG = Just [(39,127,58)]
--defining the HEX values of the background (the color to be removed)

main :: IO ()
main = do
  --getting the needed images pathes in bmp extention
  backGround <- getDataFileName "ships/binaryBG.bmp"
  hackerDown <- getDataFileName "ships/hacker.bmp"
  hackerUp <- getDataFileName "ships/hacker3.bmp"
  hackerLeft <- getDataFileName "ships/hacker2.bmp"
  hackerRight <- getDataFileName "ships/hacker4.bmp"
  attacker <- getDataFileName "ships/seeDee.bmp"
  tor <- getDataFileName "ships/tor.bmp"
  skull <- getDataFileName "ships/skull.bmp"
  flashMemory <- getDataFileName "ships/flashMemory.bmp"

  let 
    --winConfig: initial postision, window dimensions, window title
    winConfig = ((100,80),(width,height),"The dark web");
    --bmplist: List of tuples (image path, HEX values "line->31" )
    bmplist = [(backGround,Nothing),(hackerUp,removeObjectBG),(attacker,removeObjectBG),(tor,removeObjectBG), (hackerDown, removeObjectBG), (hackerLeft, removeObjectBG), (hackerRight, removeObjectBG),(skull, removeObjectBG),(flashMemory, removeObjectBG)];
        --textureMap: (BG index in bmplist) (BG width) (BG height) (window width) (windowheight)
        gameMap = textureMap 0 w h w h;
        --defining object group: objectGroup (object group name) (List of functions that returns GameObject type after creation)
        hacker   = objectGroup "hackerGroup" [createhacker];
        seeDee = objectGroup "seeDeeGroup" [createseeDee,createseeDee2,createseeDee3,createseeDee4,createseeDee5,createseeDee6,createflashMem,createflashMem2,createflashMem3,createTor];    
    --Input: Assigning characters with its state (Press/StillDown) to a function
    input = [(Char 'q', Press, \_ _ -> funExit), (Char 'r', Press, restart), (SpecialKey KeyRight, StillDown, turnRight), (SpecialKey KeyLeft,  StillDown, turnLeft), (SpecialKey KeyUp, StillDown, turnUp), (SpecialKey KeyDown, StillDown, turnDown)]
  --funInit: (winConfig "line->48") (gameMap "line->52") (List of object group) (initial level) (intial score) (Keymapping) (gameCycle) (refresh rate) (bmplist)
  --Since in this case there are no score, the fifth parameter is left as ()
  funInit winConfig gameMap [hacker, seeDee] (Level 1) () input gameCycle (Timer 40) bmplist

--Finding objects using its name and object group
--set object inital picture using bmplist index and object name
--set object inital position
--set object sleeping status (Visible/Hidden)
restart :: Modifiers -> Position -> Move ()
restart _ _ = do hacker <- findObject "hacker" "hackerGroup"
                 setObjectCurrentPicture 1 hacker
                 setObjectPosition (w/2,50) hacker
                 tor <- findObject "tor" "seeDeeGroup"
                 setObjectPosition (w/2, h-30) tor
                 setObjectCurrentPicture 3 tor
                 seeDee2 <- findObject "seeDee2" "seeDeeGroup"
                 seeDee3 <- findObject "seeDee3" "seeDeeGroup"
                 seeDee4 <- findObject "seeDee4" "seeDeeGroup"
                 seeDee5 <- findObject "seeDee5" "seeDeeGroup"
                 seeDee6 <- findObject "seeDee6" "seeDeeGroup"
                 flashMem <- findObject "flashMem" "seeDeeGroup"
                 flashMem2 <- findObject "flashMem2" "seeDeeGroup"
                 flashMem3 <- findObject "flashMem3" "seeDeeGroup"
                 setObjectAsleep True seeDee2
                 setObjectAsleep True seeDee3
                 setObjectAsleep True seeDee4
                 setObjectAsleep True seeDee5
                 setObjectAsleep True seeDee6
                 setObjectAsleep True flashMem
                 setObjectAsleep True flashMem2
                 setObjectAsleep True flashMem3
                 setGameState (Level 1)

--getting the object position
--changing object position with a defined offset
--preventing the position from exceeding the map bounds
turnRight :: Modifiers -> Position -> Move ()
turnRight _ _ = do
  obj <- findObject "hacker" "hackerGroup"
  setObjectCurrentPicture 6 obj
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + 20 + sX / 2 <= w)
     then (setObjectPosition ((pX + 20),pY) obj)
     else (setObjectPosition (w - (sX/2), pY) obj)

turnLeft :: Modifiers -> Position -> Move ()
turnLeft _ _ = do
  obj <- findObject "hacker" "hackerGroup"
  setObjectCurrentPicture 5 obj
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - 20 - sX/2 >= 0)
     then (setObjectPosition ((pX - 20),pY) obj)
     else (setObjectPosition (sX/2, pY) obj)

turnUp :: Modifiers -> Position -> Move ()
turnUp _ _ = do
  obj <- findObject "hacker" "hackerGroup"
  setObjectCurrentPicture 1 obj
  (pX,pY) <- getObjectPosition obj
  (_,sY)  <- getObjectSize obj
  if (pY +20 + sY/2 <= h)
     then (setObjectPosition (pX, (pY + 20)) obj)
     else (setObjectPosition (pX, h - (sY/2)) obj)

turnDown :: Modifiers -> Position -> Move ()
turnDown _ _ = do
  obj <- findObject "hacker" "hackerGroup"
  setObjectCurrentPicture 4 obj
  (pX,pY) <- getObjectPosition obj
  (_,sY)  <- getObjectSize obj
  if (pY - 20 - sY / 2 >= 0)
     then (setObjectPosition (pX, (pY - 20)) obj)
     else (setObjectPosition (pX, sY/2) obj)

--defining new object: object name, picture, sleeping status, initial position, initial speed, special attribute
--the hacker is controlled by input so its speed is (0,0)
--special attribute does not make any sense in our game so its parameter left empty
createhacker :: GameObject ()
createhacker =
  let hackerPic   = Tex (80,80) 1;
  in object "hacker" hackerPic False (w/2,50) (0,0) ()

createseeDee :: GameObject ()
createseeDee =
  let seeDeePic = Tex (35,35) 2
  in object "seeDee1" seeDeePic True (w/2,h/2) (-30,0) ();
      
createseeDee2 :: GameObject ()
createseeDee2 =
  let seeDeePic = Tex (35,35) 2
 in object "seeDee2" seeDeePic True (w/2, 3*h/4) (-50, 0) ();

createseeDee3 :: GameObject ()
createseeDee3 = 
  let seeDeePic = Tex (35,35) 2
 in object "seeDee3" seeDeePic True (w/4, h/2) (0, -60) ();

createseeDee4 :: GameObject ()
createseeDee4 = 
  let seeDeePic = Tex (35,35) 2
 in object "seeDee4" seeDeePic True (3*w/4, h/2) (0, -70) ();

createseeDee5 :: GameObject ()
createseeDee5 = 
   let seeDeePic = Tex (35,35) 2
  in object "seeDee5" seeDeePic True (2*w/4, h/2) (0, -90) ();

createseeDee6 :: GameObject ()
createseeDee6 = 
    let seeDeePic = Tex (35,35) 2
   in object "seeDee6" seeDeePic True (w-400,h-100) (-10, -10) ();
  
createflashMem :: GameObject ()
createflashMem = 
   let flashPic = Tex (30,30) 8
  in object "flashMem" flashPic True (w+50,h-450) (-10,0) ();
 
createflashMem2 :: GameObject ()
createflashMem2 = 
   let flashPic = Tex (30,30) 8
  in object "flashMem2" flashPic True (-30,h-500) (10,0) ();
 
createflashMem3 :: GameObject ()
createflashMem3 = 
   let flashPic = Tex (30,30) 8
  in object "flashMem3" flashPic True (0,h-400) (10,0) ();
 
createTor :: GameObject ()
createTor =
  let torPic = Tex (70,70) 3
  in object "tor" torPic True (w/2, h-30) (0,0) ();

--changing the game state according to the level
--prints text on the screen in some cases with its font configuration
--In level 0 (losing state) the target object is replaced with a skull object
gameCycle :: Move ()
gameCycle = do 
  (Level level) <- getGameState
  case level of
    0 -> do
      printOnScreen (show("You have been Hacked")) TimesRoman24 ((w/2)-90,h-100) 1.0 1.0 1.0
      printOnScreen (show("Press 'q' to quit")) TimesRoman24 ((w/2)-100,h-300) 1.0 1.0 1.0
      printOnScreen (show("Press 'r' to play again")) TimesRoman24 ((w/2)-100,h-350) 1.0 1.0 1.0
      seeDee1 <- findObject "seeDee1" "seeDeeGroup"
      seeDee2 <- findObject "seeDee2" "seeDeeGroup"
      seeDee3 <- findObject "seeDee3" "seeDeeGroup"
      seeDee4 <- findObject "seeDee4" "seeDeeGroup"
      seeDee5 <- findObject "seeDee5" "seeDeeGroup"
      seeDee6 <- findObject "seeDee6" "seeDeeGroup"
      flashMem <- findObject "flashMem" "seeDeeGroup"
      flashMem2 <- findObject "flashMem2" "seeDeeGroup"
      flashMem3 <- findObject "flashMem3" "seeDeeGroup"
      tor <- findObject "tor" "seeDeeGroup"
      hacker <- findObject "hacker" "hackerGroup"
      setObjectCurrentPicture 7 tor 
      setObjectAsleep True seeDee1
      setObjectAsleep True seeDee2
      setObjectAsleep True seeDee3
      setObjectAsleep True seeDee4
      setObjectAsleep True seeDee5
      setObjectAsleep True seeDee6
      setObjectAsleep True flashMem
      setObjectAsleep True flashMem2
      setObjectAsleep True flashMem3
      setObjectAsleep True hacker
    1 -> firstLevel
    2 -> secondLevel
    3 -> thirdLevel
    4 -> fourthLevel
    5 -> fifthLevel
    6 -> sixthLevel
    7 -> do
      printOnScreen (show("The dark web have been hacked")) TimesRoman24 ((w/2)-160,h-70) 1.0 1.0 1.0
      printOnScreen (show("You are now a true hacker")) TimesRoman24 ((w/2)-130,h-100) 1.0 1.0 1.0
      printOnScreen (show("Press 'q' to quit")) TimesRoman24 ((w/2)-100,h-300) 1.0 1.0 1.0
      printOnScreen (show("Press 'r' to play again")) TimesRoman24 ((w/2)-100,h-350) 1.0 1.0 1.0
      seeDee1 <- findObject "seeDee1" "seeDeeGroup"
      seeDee2 <- findObject "seeDee2" "seeDeeGroup"
      seeDee3 <- findObject "seeDee3" "seeDeeGroup"
      seeDee4 <- findObject "seeDee4" "seeDeeGroup"
      seeDee5 <- findObject "seeDee5" "seeDeeGroup"
      seeDee6 <- findObject "seeDee6" "seeDeeGroup"
      flashMem <- findObject "flashMem" "seeDeeGroup"
      flashMem2 <- findObject "flashMem2" "seeDeeGroup"
      flashMem3 <- findObject "flashMem3" "seeDeeGroup"
      hacker <- findObject "hacker" "hackerGroup"
      setObjectAsleep True seeDee1
      setObjectAsleep True seeDee2
      setObjectAsleep True seeDee3
      setObjectAsleep True seeDee4
      setObjectAsleep True seeDee5
      setObjectAsleep True seeDee6
      setObjectAsleep True flashMem
      setObjectAsleep True flashMem2
      setObjectAsleep True flashMem3
      setObjectAsleep True hacker

--setting the sleeping status of the needed objects as false according the the level 
--reversing the object moving direction when it collides with maps borders
--setting the game state level to 0 when the character collides with any obstacle(CD/Flash memory)
--setting the game state level to the next logical level when the character collides with the target (tor)
firstLevel :: Move()
firstLevel = do
  seeDee1 <- findObject "seeDee1" "seeDeeGroup"
  setObjectAsleep False seeDee1
  hacker <- findObject "hacker" "hackerGroup"
  setObjectAsleep False hacker
  tor <- findObject "tor" "seeDeeGroup"
  setObjectAsleep False tor
  seeDee1Left <- objectLeftMapCollision seeDee1
  when (seeDee1Left) (reverseXSpeed seeDee1)
  seeDee1Right <- objectRightMapCollision seeDee1
  when (seeDee1Right) (reverseXSpeed seeDee1)
  lossCol1 <- objectsCollision hacker seeDee1
  when (lossCol1) (setGameState (Level 0))
  winCol <- objectsCollision hacker tor
  when (winCol) (setGameState (Level 2) )
  when (winCol) (setObjectCurrentPicture 4 hacker)

secondLevel :: Move()
secondLevel = do
  hacker <- findObject "hacker" "hackerGroup"
  setObjectAsleep False hacker
  seeDee1 <- findObject "seeDee1" "seeDeeGroup"
  seeDee2 <- findObject "seeDee2" "seeDeeGroup"
  setObjectAsleep False seeDee2
  tor <- findObject "tor" "seeDeeGroup"
  setObjectAsleep False tor
  setObjectPosition (w-40, 50) tor
  seeDee1Left <- objectLeftMapCollision seeDee1
  seeDee2Left <- objectLeftMapCollision seeDee2
  when (seeDee1Left) (reverseXSpeed seeDee1)
  when (seeDee2Left) (reverseXSpeed seeDee2)
  seeDee1Right <- objectRightMapCollision seeDee1
  seeDee2Right <- objectRightMapCollision seeDee2
  when (seeDee1Right) (reverseXSpeed seeDee1)
  when (seeDee2Right) (reverseXSpeed seeDee2)
  lossCol1 <- objectsCollision hacker seeDee1
  when (lossCol1) (setGameState (Level 0))
  lossCol2 <- objectsCollision hacker seeDee2
  when (lossCol2) (setGameState (Level 0))
  winCol <- objectsCollision hacker tor
  when (winCol) (setGameState (Level 3))
  when (winCol) (setObjectCurrentPicture 5 hacker)

thirdLevel :: Move()
thirdLevel = do
  hacker <- findObject "hacker" "hackerGroup"
  setObjectAsleep False hacker
  seeDee1 <- findObject "seeDee1" "seeDeeGroup"
  setObjectAsleep False seeDee1
  seeDee2 <- findObject "seeDee2" "seeDeeGroup"
  setObjectAsleep False seeDee2
  seeDee3 <- findObject "seeDee3" "seeDeeGroup"
  setObjectAsleep False seeDee3
  seeDee4 <- findObject "seeDee4" "seeDeeGroup"
  setObjectAsleep False seeDee4
  tor <- findObject "tor" "seeDeeGroup"
  setObjectAsleep False tor
  setObjectPosition (50, h-40) tor
  seeDee1Left <- objectLeftMapCollision seeDee1
  seeDee2Left <- objectLeftMapCollision seeDee2
  when (seeDee1Left) (reverseXSpeed seeDee1)
  when (seeDee2Left) (reverseXSpeed seeDee2)
  seeDee1Right <- objectRightMapCollision seeDee1
  seeDee2Right <- objectRightMapCollision seeDee2
  when (seeDee1Right) (reverseXSpeed seeDee1)
  when (seeDee2Right) (reverseXSpeed seeDee2)
  seeDee3Down <- objectBottomMapCollision seeDee3
  seeDee4Down <- objectBottomMapCollision seeDee4
  when (seeDee3Down) (reverseYSpeed seeDee3)
  when (seeDee4Down) (reverseYSpeed seeDee4)
  seeDee3Up <- objectTopMapCollision seeDee3
  seeDee4Up <- objectTopMapCollision seeDee4
  when (seeDee3Up) (reverseYSpeed seeDee3)
  when (seeDee4Up) (reverseYSpeed seeDee4)
  lossCol1 <- objectsCollision hacker seeDee1
  when (lossCol1) (setGameState (Level 0))
  lossCol2 <- objectsCollision hacker seeDee2
  when (lossCol2) (setGameState (Level 0))
  lossCol3 <- objectsCollision hacker seeDee3
  when (lossCol3) (setGameState (Level 0))
  lossCol4 <- objectsCollision hacker seeDee4
  when (lossCol4) (setGameState (Level 0))
  winCol <- objectsCollision hacker tor
  when (winCol) (setGameState (Level 4))
  when (winCol) (setObjectCurrentPicture 5 hacker)

fourthLevel :: Move()
fourthLevel = do
  hacker <- findObject "hacker" "hackerGroup"
  setObjectAsleep False hacker
  seeDee1 <- findObject "seeDee1" "seeDeeGroup"
  setObjectAsleep False seeDee1
  seeDee2 <- findObject "seeDee2" "seeDeeGroup"
  setObjectAsleep False seeDee2
  seeDee3 <- findObject "seeDee3" "seeDeeGroup"
  setObjectAsleep False seeDee3
  seeDee4 <- findObject "seeDee4" "seeDeeGroup"
  setObjectAsleep False seeDee4
  seeDee5 <- findObject "seeDee5" "seeDeeGroup"
  setObjectAsleep False seeDee5
  tor <- findObject "tor" "seeDeeGroup"
  setObjectAsleep False tor
  setObjectPosition (w/2,100) tor
  seeDee1Left <- objectLeftMapCollision seeDee1
  seeDee2Left <- objectLeftMapCollision seeDee2
  when (seeDee1Left) (reverseXSpeed seeDee1)
  when (seeDee2Left) (reverseXSpeed seeDee2)
  seeDee1Right <- objectRightMapCollision seeDee1
  seeDee2Right <- objectRightMapCollision seeDee2
  when (seeDee1Right) (reverseXSpeed seeDee1)
  when (seeDee2Right) (reverseXSpeed seeDee2)
  seeDee3Down <- objectBottomMapCollision seeDee3
  seeDee4Down <- objectBottomMapCollision seeDee4
  when (seeDee3Down) (reverseYSpeed seeDee3)
  when (seeDee4Down) (reverseYSpeed seeDee4)
  seeDee3Up <- objectTopMapCollision seeDee3
  seeDee4Up <- objectTopMapCollision seeDee4
  when (seeDee3Up) (reverseYSpeed seeDee3)
  when (seeDee4Up) (reverseYSpeed seeDee4)
  seeDee5Down <- objectBottomMapCollision seeDee5
  when (seeDee5Down) (reverseYSpeed seeDee5)
  seeDee5Up <- objectTopMapCollision seeDee5
  when (seeDee5Up) (reverseYSpeed seeDee5)
  lossCol1 <- objectsCollision hacker seeDee1
  when (lossCol1) (setGameState (Level 0))
  lossCol2 <- objectsCollision hacker seeDee2
  when (lossCol2) (setGameState (Level 0))
  lossCol3 <- objectsCollision hacker seeDee3
  when (lossCol3) (setGameState (Level 0))
  lossCol4 <- objectsCollision hacker seeDee4
  when (lossCol4) (setGameState (Level 0))
  lossCol5 <- objectsCollision hacker seeDee5
  when (lossCol5) (setGameState (Level 0))
  winCol <- objectsCollision hacker tor
  when (winCol) (setGameState (Level 6))
  when (winCol) (setObjectCurrentPicture 5 hacker)

fifthLevel :: Move()
fifthLevel = do
  hacker <- findObject "hacker" "hackerGroup"
  setObjectAsleep False hacker
  seeDee1 <- findObject "seeDee1" "seeDeeGroup"
  setObjectAsleep False seeDee1
  seeDee2 <- findObject "seeDee2" "seeDeeGroup"
  setObjectAsleep False seeDee2
  seeDee3 <- findObject "seeDee3" "seeDeeGroup"
  setObjectAsleep False seeDee3
  seeDee4 <- findObject "seeDee4" "seeDeeGroup"
  setObjectAsleep False seeDee4
  seeDee5 <- findObject "seeDee5" "seeDeeGroup"
  setObjectAsleep False seeDee5
  seeDee6 <- findObject "seeDee6" "seeDeeGroup"
  setObjectAsleep False seeDee6
  tor <- findObject "tor" "seeDeeGroup"
  setObjectAsleep False tor
  setObjectPosition (100,100) tor
  seeDee1Left <- objectLeftMapCollision seeDee1
  seeDee2Left <- objectLeftMapCollision seeDee2
  when (seeDee1Left) (reverseXSpeed seeDee1)
  when (seeDee2Left) (reverseXSpeed seeDee2)
  seeDee1Right <- objectRightMapCollision seeDee1
  seeDee2Right <- objectRightMapCollision seeDee2
  when (seeDee1Right) (reverseXSpeed seeDee1)
  when (seeDee2Right) (reverseXSpeed seeDee2)
  seeDee3Down <- objectBottomMapCollision seeDee3
  seeDee4Down <- objectBottomMapCollision seeDee4
  when (seeDee3Down) (reverseYSpeed seeDee3)
  when (seeDee4Down) (reverseYSpeed seeDee4)
  seeDee3Up <- objectTopMapCollision seeDee3
  seeDee4Up <- objectTopMapCollision seeDee4
  when (seeDee3Up) (reverseYSpeed seeDee3)
  when (seeDee4Up) (reverseYSpeed seeDee4)
  seeDee5Down <- objectBottomMapCollision seeDee5
  when (seeDee5Down) (reverseYSpeed seeDee5)
  seeDee5Up <- objectTopMapCollision seeDee5
  when (seeDee5Up) (reverseYSpeed seeDee5)
  seeDee6Left <- objectLeftMapCollision seeDee6
  seeDee6Down <- objectBottomMapCollision seeDee6
  seeDee6Up <- objectTopMapCollision seeDee6
  seeDee6Right <- objectRightMapCollision seeDee6
  when (seeDee6Left) (reverseXSpeed seeDee6)
  when (seeDee6Down) (reverseYSpeed seeDee6)
  when (seeDee6Right) (reverseXSpeed seeDee6)
  when (seeDee6Up) (reverseYSpeed seeDee6)
  lossCol1 <- objectsCollision hacker seeDee1
  when (lossCol1) (setGameState (Level 0))
  lossCol2 <- objectsCollision hacker seeDee2
  when (lossCol2) (setGameState (Level 0))
  lossCol3 <- objectsCollision hacker seeDee3
  when (lossCol3) (setGameState (Level 0))
  lossCol4 <- objectsCollision hacker seeDee4
  when (lossCol4) (setGameState (Level 0))
  lossCol5 <- objectsCollision hacker seeDee5
  when (lossCol5) (setGameState (Level 0))
  lossCol6 <- objectsCollision hacker seeDee6
  when (lossCol6) (setGameState (Level 0))
  winCol <- objectsCollision hacker tor
  when (winCol) (setGameState (Level 1))

sixthLevel :: Move()
sixthLevel = do
  hacker <- findObject "hacker" "hackerGroup"
  setObjectAsleep False hacker
  seeDee1 <- findObject "seeDee1" "seeDeeGroup"
  setObjectAsleep False seeDee1
  seeDee2 <- findObject "seeDee2" "seeDeeGroup"
  setObjectAsleep False seeDee2
  seeDee3 <- findObject "seeDee3" "seeDeeGroup"
  setObjectAsleep False seeDee3
  seeDee4 <- findObject "seeDee4" "seeDeeGroup"
  setObjectAsleep False seeDee4
  seeDee5 <- findObject "seeDee5" "seeDeeGroup"
  setObjectAsleep False seeDee5
  seeDee6 <- findObject "seeDee6" "seeDeeGroup"
  setObjectAsleep False seeDee6
  flashMem <- findObject "flashMem" "seeDeeGroup"
  setObjectAsleep False flashMem
  flashMem2 <- findObject "flashMem2" "seeDeeGroup"
  setObjectAsleep False flashMem2
  flashMem3 <- findObject "flashMem3" "seeDeeGroup"
  setObjectAsleep False flashMem3
  tor <- findObject "tor" "seeDeeGroup"
  setObjectAsleep False tor
  setObjectPosition (w-40,h-40) tor
  seeDee1Left <- objectLeftMapCollision seeDee1
  seeDee2Left <- objectLeftMapCollision seeDee2
  when (seeDee1Left) (reverseXSpeed seeDee1)
  when (seeDee2Left) (reverseXSpeed seeDee2)
  seeDee1Right <- objectRightMapCollision seeDee1
  seeDee2Right <- objectRightMapCollision seeDee2
  when (seeDee1Right) (reverseXSpeed seeDee1)
  when (seeDee2Right) (reverseXSpeed seeDee2)
  seeDee3Down <- objectBottomMapCollision seeDee3
  seeDee4Down <- objectBottomMapCollision seeDee4
  when (seeDee3Down) (reverseYSpeed seeDee3)
  when (seeDee4Down) (reverseYSpeed seeDee4)
  seeDee3Up <- objectTopMapCollision seeDee3
  seeDee4Up <- objectTopMapCollision seeDee4
  when (seeDee3Up) (reverseYSpeed seeDee3)
  when (seeDee4Up) (reverseYSpeed seeDee4)
  seeDee5Down <- objectBottomMapCollision seeDee5
  when (seeDee5Down) (reverseYSpeed seeDee5)
  seeDee5Up <- objectTopMapCollision seeDee5
  when (seeDee5Up) (reverseYSpeed seeDee5)
  seeDee6Left <- objectLeftMapCollision seeDee6
  seeDee6Down <- objectBottomMapCollision seeDee6
  seeDee6Up <- objectTopMapCollision seeDee6
  seeDee6Right <- objectRightMapCollision seeDee6
  when (seeDee6Left) (reverseXSpeed seeDee6)
  when (seeDee6Down) (reverseYSpeed seeDee6)
  when (seeDee6Right) (reverseXSpeed seeDee6)
  when (seeDee6Up) (reverseYSpeed seeDee6)
  lossCol1 <- objectsCollision hacker seeDee1
  when (lossCol1) (setGameState (Level 0))
  lossCol2 <- objectsCollision hacker seeDee2
  when (lossCol2) (setGameState (Level 0))
  lossCol3 <- objectsCollision hacker seeDee3
  when (lossCol3) (setGameState (Level 0))
  lossCol4 <- objectsCollision hacker seeDee4
  when (lossCol4) (setGameState (Level 0))
  lossCol5 <- objectsCollision hacker seeDee5
  when (lossCol5) (setGameState (Level 0))
  lossCol6 <- objectsCollision hacker seeDee6
  when (lossCol6) (setGameState (Level 0))
  lossCol7 <- objectsCollision hacker flashMem
  when (lossCol7) (setGameState (Level 0))
  lossCol8 <- objectsCollision hacker flashMem2
  when (lossCol8) (setGameState (Level 0))
  lossCol9 <- objectsCollision hacker flashMem3
  when (lossCol9) (setGameState (Level 0))
  winCol <- objectsCollision hacker tor
  when (winCol) (setGameState (Level 7))
  
{--}

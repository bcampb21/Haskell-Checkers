module CheckersBoard (
    Board
  , Game
  , ModifiedBoard
  , BSquare
  , Position
  , initialBoard
  , getPiece
  , deletePiece
  , getModifiedBoard
  , toListFromBoard
  , getPlayerPiece
  , getInitialGame
  , getModifiedGame
  , validateMove
  , validatePlayer
  , checkGameOver
  , getWinner
) where

import Data.List (intercalate)
import Data.Vector (Vector,toList,fromList,(!),(//))
import qualified Data.Vector as V
import Data.Either

-- Player Pieces represention
-- The types of player on the checkers board - Red and Black
data Player = Red | Black deriving (Eq)

-- Type of Pieces on the board
data Piece = Single | King deriving (Eq)

-- Player Piece will be comprised of player and type.
data PlayerPiece = PlayerPiece Player Piece deriving (Eq)

-- Game and game state
data Game = Game GameState [Player] deriving (Eq)
data GameState = GameState Board Player deriving (Show, Eq) -- State, Board, Current Player

-- Prints the player -> R or B
instance Show Player where
  show Red  = "R"
  show Black  = "B"
 
-- Prints the type -> s or k
instance Show Piece where
  show King   = "k"
  show Single = "s"
  
-- Combine Player and Type to form the player piece -> Rs
instance Show PlayerPiece where
  show (PlayerPiece player piece) = (show player) ++ (show piece)


-- Coordinates are 0 based starts from (0,0)
type Position = (Int,Int)


-- Board representation
data Board = Board (Vector(Vector BSquare)) deriving (Eq)

-- Return checkValidMove result or error
type ModifiedBoard  = Either String Board

-- A square of the chess board may contain a piece, or it may not.
type BSquare = Maybe PlayerPiece

-- Display Board
instance Show Board where
  show board = (unlines ([yCoordInd] ++ (prettyBorderLine : rowString)))
    where
      boardList = toListFromBoard board
      rowString = zipWith displayRow ([0..7]) $ boardList
      showSquare Nothing = "  "
      showSquare (Just x) = show x
      prettyBorderLine = "  " ++ (replicate 41 '-' )
      displayRow :: Integer -> [BSquare] -> String
      displayRow i sq       = (intercalate " | " $ (show i) : (map showSquare sq) ) ++ " |" ++ "\n" ++ prettyBorderLine
      yCoordInd         = (intercalate " |  " $ " " : (map (:[]) ['0'..'7']))  ++ " |"
        

instance Show Game where
  show (Game (GameState board player) _) = (show board) ++ (show player)  

-- Initial Board with start values
initialBoard :: Board
initialBoard = Board $ fromList $ map fromList $ concat [
  [ redPieceFirstLine, redEmptyFirstLine, redPieceFirstLine]
  , (replicate 2 emptyLine)
  , [ blackEmptyFirstLine , blackPieceFirstLine, blackEmptyFirstLine]
  ]
  where
    redEmptyFirstLine   = emptyFirstLine Red
    redPieceFirstLine    =  pieceFirstLine Red
    blackPieceFirstLine   = pieceFirstLine Black
    blackEmptyFirstLine    = emptyFirstLine Black
    emptyLine        = replicate 8 Nothing
    pieceFirstLine player = concat [replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing] 
    emptyFirstLine player = concat [replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single]
    
     

-- Query a piece from Board at a Position eg. getPiece initialBoard (0,0) -> will return Just Rs
getPiece :: Board -> Position -> BSquare
getPiece board pos = getBSquareValue board $ pos

-- deletePiece a piece from the Board and get the new Board as a result
deletePiece :: Board -> Position -> Board
deletePiece (Board board) pos = let
  (xPos, yPos) = pos
  in Board $ board // [ (xPos,((board ! xPos) // [(yPos, Nothing)]))]


-- getModifiedBoard - returns the new board if the move is Valid.
getModifiedBoard :: Board -> Position -> Position -> Board
getModifiedBoard board start end = let
  startC = start
  endC   = end
  in
  getNewBoard board startC endC (getBSquareValue board startC) (getBSquareValue board endC)


getNewBoard :: Board -> Position -> Position -> BSquare -> BSquare -> Board
getNewBoard board startC endC (Just p1) Nothing
    |(abs (fst endC - fst startC)) == 2 &&  (getPiece board (((fst startC) + (fst endC)) `div` 2, ((snd startC) + (snd endC)) `div` 2)) == Just (PlayerPiece Red Single) = (modifiedBoard (deletePiece board (((fst startC) + (fst endC)) `div` 2, ((snd startC) + (snd endC)) `div` 2)) startC endC p1)
    |otherwise = (modifiedBoard board startC endC p1)
   

toListFromBoard :: Board -> [[BSquare]]        
toListFromBoard (Board boardList) = toList $ V.map toList boardList                            

-- internal function to check if the move is valie
-- checkValidMove -- TODO: Add more constraints to the possible moves
checkValidMove :: Board -> Position -> Position -> BSquare -> BSquare -> Bool
checkValidMove _ _ _ Nothing _                     = False
checkValidMove board startC endC (Just p1) (Just p2) = False
checkValidMove board startC endC (Just p1) Nothing
	|(fst endC < 0) || (snd endC < 0) || (fst endC > 7) || (snd endC > 7) = False
	|(fst endC - fst startC) == 0 || (snd endC - snd startC) == 0 = False
	|(abs (fst endC - fst startC)) /= (abs (snd endC - snd startC)) = False
	|(abs (fst endC - fst startC)) > 2 = False
	|(abs (fst endC - fst startC)) == 2 &&  (getPiece board (((fst startC) + (fst endC)) `div` 2, ((snd startC) + (snd endC)) `div` 2)) == Just (PlayerPiece Red Single) = True
	|otherwise = True
  
-- internal function to create the modified board if the move is valid                      
modifiedBoard :: Board -> Position -> Position -> PlayerPiece -> Board
modifiedBoard (Board board) (startX,startY) (endX,endY) piece = Board $
  if startX == endX
  then board // [ (startX,( (board ! startX) // [(startY,Nothing),(endY,(Just piece))]))]
  else board // [
    (startX,(board ! startX) // [(startY,Nothing)])
    , (endX,(board ! endX) // [(endY,(Just piece))])
    ]
  
-- get bsquare  value from Board and Position
getBSquareValue :: Board -> Position -> BSquare
getBSquareValue (Board b) (xPosition, yPositionInd) = b ! xPosition ! yPositionInd

-- validate move in Game
validateMove :: Game -> Position -> Position -> Bool
validateMove (Game (GameState board player) _) startPosition endPosition = let
  isValid = checkValidMove board startPosition endPosition (getBSquareValue board startPosition) (getBSquareValue board endPosition)
  in
  if (isValid)
    then True
    else False

-- validate if player chooses the correct piece tp play
validatePlayer :: Game -> Position -> Bool
validatePlayer (Game (GameState board player) _) position = let
  piece = getPlayerPiece board position
  in
  if (piece == PlayerPiece Red Single && player == Red) || (piece == PlayerPiece Black Single && player == Black)
    then True
    else False

-- get modified new game / game state
getModifiedGame :: Game -> Position -> Position -> Game
getModifiedGame (Game (GameState board player) playerList) startPosition endPosition = let
  playerPiece = getPlayerPiece board startPosition
  newBoard = getModifiedBoard board startPosition endPosition
  in
  if player == Red 
      then (Game (GameState newBoard Black) playerList)
      else (Game (GameState newBoard Red) playerList)


-- get player peice from board
getPlayerPiece :: Board -> Position -> PlayerPiece
getPlayerPiece board position = let
  bSquare = getBSquareValue board position
  showSquare Nothing = " "
  showSquare (Just x) = show x
  playerPiece = showSquare bSquare
  in
  if (playerPiece == "Rs")
    then PlayerPiece Red Single
    else PlayerPiece Black Single

-- get initial game
getInitialGame :: Game
getInitialGame = Game initialState [Black, Red] where
  initialState = GameState initialBoard $ Black

-- check game over condition
checkGameOver :: Game -> Bool
checkGameOver (Game (GameState board player) _) = let
  showSquare Nothing = " "
  showSquare (Just x) = show x
  playerPiece00  = getPiece board (0,0);  piece00 = showSquare playerPiece00;  playerPiece01  = getPiece board (0,1);  piece01 = showSquare playerPiece01;  playerPiece02  = getPiece board (0,2);  piece02 = showSquare playerPiece02;  playerPiece03  = getPiece board (0,3);  piece03 = showSquare playerPiece03;  playerPiece04  = getPiece board (0,4);  piece04 = showSquare playerPiece04;  playerPiece05  = getPiece board (0,5);  piece05 = showSquare playerPiece05;  playerPiece06  = getPiece board (0,6);  piece06 = showSquare playerPiece06;  playerPiece07  = getPiece board (0,7);  piece07 = showSquare playerPiece07;
  playerPiece10  = getPiece board (1,0);  piece10 = showSquare playerPiece10;  playerPiece11  = getPiece board (1,1);  piece11 = showSquare playerPiece11;  playerPiece12  = getPiece board (1,2);  piece12 = showSquare playerPiece12;  playerPiece13  = getPiece board (1,3);  piece13 = showSquare playerPiece13;  playerPiece14  = getPiece board (1,4);  piece14 = showSquare playerPiece14;  playerPiece15  = getPiece board (1,5);  piece15 = showSquare playerPiece15;  playerPiece16  = getPiece board (1,6);  piece16 = showSquare playerPiece16;  playerPiece17  = getPiece board (1,7);  piece17 = showSquare playerPiece17;
  playerPiece20  = getPiece board (2,0);  piece20 = showSquare playerPiece20;  playerPiece21  = getPiece board (2,1);  piece21 = showSquare playerPiece21;  playerPiece22  = getPiece board (2,2);  piece22 = showSquare playerPiece22;  playerPiece23  = getPiece board (2,3);  piece23 = showSquare playerPiece23;  playerPiece24  = getPiece board (2,4);  piece24 = showSquare playerPiece24;  playerPiece25  = getPiece board (2,5);  piece25 = showSquare playerPiece25;  playerPiece26  = getPiece board (2,6);  piece26 = showSquare playerPiece26;  playerPiece27  = getPiece board (2,7);  piece27 = showSquare playerPiece27;
  playerPiece30  = getPiece board (3,0);  piece30 = showSquare playerPiece30;  playerPiece31  = getPiece board (3,1);  piece31 = showSquare playerPiece31;  playerPiece32  = getPiece board (3,2);  piece32 = showSquare playerPiece32;  playerPiece33  = getPiece board (3,3);  piece33 = showSquare playerPiece33;  playerPiece34  = getPiece board (3,4);  piece34 = showSquare playerPiece34;  playerPiece35  = getPiece board (3,5);  piece35 = showSquare playerPiece35;  playerPiece36  = getPiece board (3,6);  piece36 = showSquare playerPiece36;  playerPiece37  = getPiece board (3,7);  piece37 = showSquare playerPiece37;
  playerPiece40  = getPiece board (4,0);  piece40 = showSquare playerPiece40;  playerPiece41  = getPiece board (4,1);  piece41 = showSquare playerPiece41;  playerPiece42  = getPiece board (4,2);  piece42 = showSquare playerPiece42;  playerPiece43  = getPiece board (4,3);  piece43 = showSquare playerPiece43;  playerPiece44  = getPiece board (4,4);  piece44 = showSquare playerPiece44;  playerPiece45  = getPiece board (4,5);  piece45 = showSquare playerPiece45;  playerPiece46  = getPiece board (4,6);  piece46 = showSquare playerPiece46;  playerPiece47  = getPiece board (4,7);  piece47 = showSquare playerPiece47;
  playerPiece50  = getPiece board (5,0);  piece50 = showSquare playerPiece50;  playerPiece51  = getPiece board (5,1);  piece51 = showSquare playerPiece51;  playerPiece52  = getPiece board (5,2);  piece52 = showSquare playerPiece52;  playerPiece53  = getPiece board (5,3);  piece53 = showSquare playerPiece53;  playerPiece54  = getPiece board (5,4);  piece54 = showSquare playerPiece54;  playerPiece55  = getPiece board (5,5);  piece55 = showSquare playerPiece55;  playerPiece56  = getPiece board (5,6);  piece56 = showSquare playerPiece56;  playerPiece57  = getPiece board (5,7);  piece57 = showSquare playerPiece57;  
  playerPiece60  = getPiece board (6,0);  piece60 = showSquare playerPiece60;  playerPiece61  = getPiece board (6,1);  piece61 = showSquare playerPiece61;  playerPiece62  = getPiece board (6,2);  piece62 = showSquare playerPiece62;  playerPiece63  = getPiece board (6,3);  piece63 = showSquare playerPiece63;  playerPiece64  = getPiece board (6,4);  piece64 = showSquare playerPiece64;  playerPiece65  = getPiece board (6,5);  piece65 = showSquare playerPiece65;  playerPiece66  = getPiece board (6,6);  piece66 = showSquare playerPiece66;  playerPiece67  = getPiece board (6,7);  piece67 = showSquare playerPiece67;
  playerPiece70  = getPiece board (7,0);  piece70 = showSquare playerPiece70;  playerPiece71  = getPiece board (7,1);  piece71 = showSquare playerPiece71;  playerPiece72  = getPiece board (7,2);  piece72 = showSquare playerPiece72;  playerPiece73  = getPiece board (7,3);  piece73 = showSquare playerPiece73;  playerPiece74  = getPiece board (7,4);  piece74 = showSquare playerPiece74;  playerPiece75  = getPiece board (7,5);  piece75 = showSquare playerPiece75;  playerPiece76  = getPiece board (7,6);  piece76 = showSquare playerPiece76;  playerPiece77  = getPiece board (7,7);  piece77 = showSquare playerPiece77;
  in
  if ((piece00 == "Rs"  || piece00 == " ") && (piece01 == "Rs"  || piece01 == " ") && (piece02 == "Rs"  || piece02 == " ") && (piece03 == "Rs"  || piece03 == " ") && (piece04 == "Rs"  || piece04 == " ") && (piece05 == "Rs"  || piece05 == " ") && (piece06 == "Rs"  || piece06 == " ") && (piece07 == "Rs"  || piece07 == " ") &&
    (piece10 == "Rs"  || piece10 == " ") && (piece11 == "Rs"  || piece11 == " ") && (piece12 == "Rs"  || piece12 == " ") && (piece13 == "Rs"  || piece13 == " ") && (piece14 == "Rs"  || piece14 == " ") && (piece15 == "Rs"  || piece15 == " ") && (piece16 == "Rs"  || piece16 == " ") && (piece17 == "Rs"  || piece17 == " ") &&
    (piece20 == "Rs"  || piece20 == " ") && (piece21 == "Rs"  || piece21 == " ") && (piece22 == "Rs"  || piece22 == " ") && (piece23 == "Rs"  || piece23 == " ") && (piece24 == "Rs"  || piece24 == " ") && (piece25 == "Rs"  || piece25 == " ") && (piece26 == "Rs"  || piece26 == " ") && (piece27 == "Rs"  || piece27 == " ") &&
    (piece30 == "Rs"  || piece30 == " ") && (piece31 == "Rs"  || piece31 == " ") && (piece32 == "Rs"  || piece32 == " ") && (piece33 == "Rs"  || piece33 == " ") && (piece34 == "Rs"  || piece34 == " ") && (piece35 == "Rs"  || piece35 == " ") && (piece36 == "Rs"  || piece36 == " ") && (piece37 == "Rs"  || piece37 == " ") &&
    (piece40 == "Rs"  || piece40 == " ") && (piece41 == "Rs"  || piece41 == " ") && (piece42 == "Rs"  || piece42 == " ") && (piece43 == "Rs"  || piece43 == " ") && (piece44 == "Rs"  || piece44 == " ") && (piece45 == "Rs"  || piece45 == " ") && (piece46 == "Rs"  || piece46 == " ") && (piece47 == "Rs"  || piece47 == " ") &&
    (piece50 == "Rs"  || piece50 == " ") && (piece51 == "Rs"  || piece51 == " ") && (piece52 == "Rs"  || piece52 == " ") && (piece53 == "Rs"  || piece53 == " ") && (piece54 == "Rs"  || piece54 == " ") && (piece55 == "Rs"  || piece55 == " ") && (piece56 == "Rs"  || piece56 == " ") && (piece57 == "Rs"  || piece57 == " ") && 
    (piece60 == "Rs"  || piece60 == " ") && (piece61 == "Rs"  || piece61 == " ") && (piece62 == "Rs"  || piece62 == " ") && (piece63 == "Rs"  || piece63 == " ") && (piece64 == "Rs"  || piece64 == " ") && (piece65 == "Rs"  || piece65 == " ") && (piece66 == "Rs"  || piece66 == " ") && (piece67 == "Rs"  || piece67 == " ") &&
    (piece70 == "Rs"  || piece70 == " ") && (piece71 == "Rs"  || piece71 == " ") && (piece72 == "Rs"  || piece72 == " ") && (piece73 == "Rs"  || piece73 == " ") && (piece74 == "Rs"  || piece74 == " ") && (piece75 == "Rs"  || piece75 == " ") && (piece76 == "Rs"  || piece76 == " ") && (piece77 == "Rs"  || piece77 == " ") ) ||
    ((piece00 == "Bs"  || piece00 == " ") && (piece01 == "Bs"  || piece01 == " ") && (piece02 == "Bs"  || piece02 == " ") && (piece03 == "Bs"  || piece03 == " ") && (piece04 == "Bs"  || piece04 == " ") && (piece05 == "Bs"  || piece05 == " ") && (piece06 == "Bs"  || piece06 == " ") && (piece07 == "Bs"  || piece07 == " ") &&
    (piece10 == "Bs"  || piece10 == " ") && (piece11 == "Bs"  || piece11 == " ") && (piece12 == "Bs"  || piece12 == " ") && (piece13 == "Bs"  || piece13 == " ") && (piece14 == "Bs"  || piece14 == " ") && (piece15 == "Bs"  || piece15 == " ") && (piece16 == "Bs"  || piece16 == " ") && (piece17 == "Bs"  || piece17 == " ") &&
    (piece20 == "Bs"  || piece20 == " ") && (piece21 == "Bs"  || piece21 == " ") && (piece22 == "Bs"  || piece22 == " ") && (piece23 == "Bs"  || piece23 == " ") && (piece24 == "Bs"  || piece24 == " ") && (piece25 == "Bs"  || piece25 == " ") && (piece26 == "Bs"  || piece26 == " ") && (piece27 == "Bs"  || piece27 == " ") &&
    (piece30 == "Bs"  || piece30 == " ") && (piece31 == "Bs"  || piece31 == " ") && (piece32 == "Bs"  || piece32 == " ") && (piece33 == "Bs"  || piece33 == " ") && (piece34 == "Bs"  || piece34 == " ") && (piece35 == "Bs"  || piece35 == " ") && (piece36 == "Bs"  || piece36 == " ") && (piece37 == "Bs"  || piece37 == " ") &&
    (piece40 == "Bs"  || piece40 == " ") && (piece41 == "Bs"  || piece41 == " ") && (piece42 == "Bs"  || piece42 == " ") && (piece43 == "Bs"  || piece43 == " ") && (piece44 == "Bs"  || piece44 == " ") && (piece45 == "Bs"  || piece45 == " ") && (piece46 == "Bs"  || piece46 == " ") && (piece47 == "Bs"  || piece47 == " ") &&
    (piece50 == "Bs"  || piece50 == " ") && (piece51 == "Bs"  || piece51 == " ") && (piece52 == "Bs"  || piece52 == " ") && (piece53 == "Bs"  || piece53 == " ") && (piece54 == "Bs"  || piece54 == " ") && (piece55 == "Bs"  || piece55 == " ") && (piece56 == "Bs"  || piece56 == " ") && (piece57 == "Bs"  || piece57 == " ") && 
    (piece60 == "Bs"  || piece60 == " ") && (piece61 == "Bs"  || piece61 == " ") && (piece62 == "Bs"  || piece62 == " ") && (piece63 == "Bs"  || piece63 == " ") && (piece64 == "Bs"  || piece64 == " ") && (piece65 == "Bs"  || piece65 == " ") && (piece66 == "Bs"  || piece66 == " ") && (piece67 == "Bs"  || piece67 == " ") &&
    (piece70 == "Bs"  || piece70 == " ") && (piece71 == "Bs"  || piece71 == " ") && (piece72 == "Bs"  || piece72 == " ") && (piece73 == "Bs"  || piece73 == " ") && (piece74 == "Bs"  || piece74 == " ") && (piece75 == "Bs"  || piece75 == " ") && (piece76 == "Bs"  || piece76 == " ") && (piece77 == "Bs"  || piece77 == " ")) 
    then True
    else False

-- get winner
getWinner :: Game -> Player
getWinner (Game (GameState board player) _) = player
module Main where
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- A 2 player chess game that can be played with custom sprites according to your tastes. The majority of the rules of chess are enforced by the program and these are listed as follows :|
-- The pieces must follow their valid move sets (e.g. a knight can only move in an L), single movement of pawns after initial two space move, en passant is an included rule and castling |
-- is also included. When castling, the player must select the king with the selction marker and the rook they want to castle with with the pointer marker. This only works if both pieces|
-- are in their initial state.                                                                                                                                                            |
-- Notice: NOT ALL THE RULES ARE INCLUDED. Specifically I am referring to the fact that players must decide for themselves whether they are in check or checkmated but the game in general|
-- is still playable. Also, as an extra note: pawn promotion is included as a rule. However, the promotion automatically promotes to queen which could be fixed by taking a user input    |
-- when the move to the 7th rank is made. Have fun playing Haskell Chess!                                                                                                                 |
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game 
import qualified Data.Map as Map
import Data.Maybe

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Run here. If you would like to, it is possible to change the piece sprites to be any bitmap file you want. So for example, you could replace all the rooks with pictures of Nicholas Cage's face or replace knights with actual horses.|
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- (keep in mind that the original bitmap files are 1000x1000 so try to use bitmap files similar to that size or the sizes of your pieces may be a bit wonky)
main :: IO () 
main =  do
        --import image files
        knightMonad <- loadBMP "assets/knightW.bmp"
        kingMonad <- loadBMP "assets/kingBNoMonads.bmp"
        whitePawn <- loadBMP "assets/pawnW.bmp"
        blackPawn <- loadBMP "assets/pawnB.bmp"
        whiteKnight <- loadBMP "assets/knightWO.bmp"
        blackKnight <- loadBMP "assets/knightB.bmp"
        whiteRook <- loadBMP "assets/rookW.bmp"
        blackRook <- loadBMP "assets/rookB.bmp"
        whiteBishop <- loadBMP "assets/bishopW.bmp"
        blackBishop <- loadBMP "assets/bishopB.bmp"
        whiteKing <- loadBMP "assets/kingW.bmp"
        blackKing <- loadBMP "assets/kingB.bmp"
        whiteQueen <- loadBMP "assets/queenW.bmp"
        blackQueen <- loadBMP "assets/queenB.bmp"
        redPoint <- loadBMP "assets/redPoint.bmp"
        circle <- loadBMP "assets/circle.bmp"
        let images = [knightMonad, kingMonad, whitePawn, blackPawn, whiteKnight, blackKnight, whiteRook, blackRook, whiteBishop, blackBishop, whiteKing, blackKing, whiteQueen, blackQueen, circle, redPoint]
        -- use the play function in the pure game gloss interface to make game creation easier
        play (FullScreen) (greyN 0.77) (60) (initialBoard) (drawWorld images) (takeUserMove) (identity)

identity :: Float -> State -> State
identity float state = state

----------------------------------------------------------------------------------------
-- map handling functions to make removal/insertion of pieces to the board a bit easier|
----------------------------------------------------------------------------------------

insertPiece :: PiecePosition -> Piece -> Board -> Board
insertPiece = Map.insert

checkPosition :: PiecePosition -> Board -> Maybe Piece
checkPosition = Map.lookup

removePiece :: PiecePosition -> Board -> Board
removePiece = Map.delete

-- delete piece at old position, insert same piece on new position and update its state
adjustPosition :: PiecePosition -> PiecePosition -> Board -> Board
adjustPosition position newPosition board = insertPiece newPosition ((updatePieceState position newPosition (fromJust $ checkPosition position board))) $ removePiece position board

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- data type definitions. They sort of act similar to objects in OOP languages and make passing states between functions easier. These contain definitions for pieces, state of the game and the board.|
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- definitions of pieces and the board as a map containing a set of coordinates mapped to each piece
type Board = Map.Map PiecePosition Piece
type PiecePosition = (Int, Int)

-- defining the different states of each piece
data PieceSide  = White       | Black                               deriving (Eq, Show)
data PieceType  = Knight      | Rook | Bishop | Queen | King | Pawn deriving (Eq, Show)
data PieceState = ActiveMoved | ActiveInitial | PawnTwice           deriving (Eq, Show)

-- defining variables for game state
data State  = CurrentState {   board         :: Board
                             , currentTurn   :: PieceSide 
                             , position      :: Position
                             , newPosition   :: NewPosition }

-- definition of pieces e.g. is it a rook or pawn and what side is the piece on
-- piece state defined specifically to check if a pawn has moved yet
data Piece = Piece { pieceSide :: PieceSide
                    ,pieceType :: PieceType
                    ,pieceState:: PieceState } deriving (Eq, Show)

-- position markers to locate which piece we want to move and where we want to move it to
data Position    = Position    { piecePosition    :: PiecePosition } deriving (Eq, Show)
data NewPosition = NewPosition { newPiecePosition :: PiecePosition } deriving (Eq, Show)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Bulk section for checking whether a user input is valid or not. Also, used to update the map state when a valid change is made. When a valid move is made on either side, we must set the currentSide to the opposing side.|
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- definitions if a move made with a piece is valid
-- pawns can only move forward twice on their first move
isValidMove :: Maybe Piece -> PiecePosition -> PiecePosition -> Bool
isValidMove (Just (Piece _ King _))                 (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1
isValidMove (Just (Piece _ Queen _))                (x1, y1) (x2, y2) = (x1 - x2 == 0 || y1 - y2 == 0) || abs (x1 - x2) == abs (y1 - y2)
isValidMove (Just (Piece _ Rook _))                 (x1, y1) (x2, y2) = x1 - x2 == 0 || y1 - y2 == 0
isValidMove (Just (Piece _ Bishop _))               (x1, y1) (x2, y2) = abs (x1 - x2) == abs (y1 - y2)
isValidMove (Just (Piece _ Knight _))               (x1, y1) (x2, y2) = (abs (x1 - x2) == 2 && abs (y1 - y2) == 1) || (abs (y1 - y2) == 2 && abs (x1 - x2) == 1)
isValidMove (Just (Piece White Pawn ActiveInitial)) (x1, y1) (x2, y2) = x1 - x2 == 0 && (y1 - y2 == -2 || y1 - y2 == -1) 
isValidMove (Just (Piece Black Pawn ActiveInitial)) (x1, y1) (x2, y2) = x1 - x2 == 0 && (y1 - y2 == 2 || y1 - y2 == 1) 
isValidMove (Just (Piece White Pawn ActiveMoved))   (x1, y1) (x2, y2) = x1 - x2 == 0 && y1 - y2 == -1
isValidMove (Just (Piece Black Pawn ActiveMoved))   (x1, y1) (x2, y2) = x1 - x2 == 0 && y1 - y2 == 1
isValidMove (Just (Piece White Pawn PawnTwice))     (x1, y1) (x2, y2) = x1 - x2 == 0 && y1 - y2 == -1
isValidMove (Just (Piece Black Pawn PawnTwice))     (x1, y1) (x2, y2) = x1 - x2 == 0 && y1 - y2 == 1

-- defines the capture mechanic
-- special definition for pawns to capture on the diagonal
isValidCapture :: Maybe Piece -> PiecePosition -> PiecePosition -> Bool
isValidCapture (Just (Piece White Pawn _)) (x1, y1) (x2, y2) = abs (x1 - x2) == 1 && y1 - y2 == -1
isValidCapture (Just (Piece Black Pawn _)) (x1, y1) (x2, y2) = abs (x1 - x2) == 1 && y1 - y2 == 1
isValidCapture piece position newPosition = isValidMove piece position newPosition
                  

-- handling events to make sure a player has made a move with a piece (specifically pawns)
updatePieceState :: PiecePosition -> PiecePosition -> Piece  -> Piece
updatePieceState position newPosition piece | pieceType piece == Pawn && (snd newPosition == 7 || snd newPosition == 0)= piece {pieceType  = Queen}
                                            | pieceType piece == Pawn && (abs (snd position - snd newPosition) == 2)   = piece {pieceState = PawnTwice}
                                            | otherwise                                                                = piece {pieceState = ActiveMoved}

-- function for promotion to queen when a pawn reaches 7th rank
promotePiece :: PieceType -> Piece -> Piece
promotePiece new piece = piece { pieceType = new }

-- define initial board state (white starts on bottom)
-- all pieces are defined as active initial
initialBoard :: State
initialBoard = CurrentState (Map.fromList board) White (Position (3, 3)) (NewPosition(4, 4)) where
        iW piece = Piece White piece ActiveInitial
        iB piece = Piece Black piece ActiveInitial
        board = zip (zip [0..7] (replicate 8 7)) [iB Rook, iB Knight, iB Bishop, iB Queen, iB King, iB Bishop, iB Knight, iB Rook] ++
                zip (zip [0..7] (replicate 8 6)) [iB Pawn, iB Pawn,   iB Pawn,   iB Pawn,  iB Pawn, iB Pawn,   iB Pawn,   iB Pawn] ++
                zip (zip [0..7] (replicate 8 1)) [iW Pawn, iW Pawn,   iW Pawn,   iW Pawn,  iW Pawn, iW Pawn,   iW Pawn,   iW Pawn] ++
                zip (zip [0..7] (replicate 8 0)) [iW Rook, iW Knight, iW Bishop, iW Queen, iW King, iW Bishop, iW Knight, iW Rook]

-- updates board state when the even function is called
updateBoard :: (Board, PieceSide) -> Position -> NewPosition -> (Board, PieceSide)
updateBoard (board, side) position newPosition | inputValid                                         = (board, side)
                                               | fst (enPassant (board, side) position newPosition) = updatePassant (board, side) position newPosition
                                               | fst (castlingMove board position newPosition)      = (snd (castlingMove board position newPosition), oppositeSide side)
                                               | validCapture                                       = ((adjustPosition (piecePosition position) (newPiecePosition newPosition) (removePiece (newPiecePosition newPosition) board)), oppositeSide side)
                                               | validMove                                          = ((adjustPosition (piecePosition position) (newPiecePosition newPosition) board), oppositeSide side)
                                               | otherwise                                          = (board, side) where
                                                       -- the user must select a non empty map location and a non empty new map location for a capture to take place
                                                       validCapture = isValidCapture (checkPosition (piecePosition position) board) (piecePosition position) (newPiecePosition newPosition) &&
                                                                      checkPosition (newPiecePosition newPosition) board /= Nothing &&
                                                                      (pieceSide $ fromJust $ checkPosition (newPiecePosition newPosition) board) /= (pieceSide $ fromJust $ checkPosition (piecePosition position) board)
                                                       -- for the edge case where a user attempts to move a piece from an empty square
                                                       validMove    = isValidMove (checkPosition (piecePosition position) board) (piecePosition position) (newPiecePosition newPosition) &&
                                                                      checkPosition (newPiecePosition newPosition) board == Nothing
                                                       -- a move must be done on a piece whose side matches the current players turn taken from state
                                                       -- also checks for any blocking pieces between two points
                                                       inputValid   = maybePieceSideToBool (checkPosition (piecePosition position) board) side  ||
                                                                      (piecePosition position) == (newPiecePosition newPosition)                ||
                                                                      checkForBlockers board position newPosition

-- there are only 4 cases for a valid castle so I hardcoded map adjustments
castlingMove :: Board -> Position -> NewPosition -> (Bool, Board)
castlingMove board position newPosition | checkValidWhiteCastle && -- castling on the white side
                                          newPiecePosition newPosition == (0, 0) = (True, adjustPosition (0, 0) (3, 0) $ adjustPosition (4, 0) (2, 0) board)
                                        | checkValidWhiteCastle &&
                                          newPiecePosition newPosition == (7, 0) = (True, adjustPosition (7, 0) (5, 0) $ adjustPosition (4, 0) (6, 0) board)
                                        | checkValidBlackCastle && -- castling on the black side
                                          newPiecePosition newPosition == (0, 7) = (True, adjustPosition (0, 7) (3, 7) $ adjustPosition (4, 7) (2, 7) board)
                                        | checkValidBlackCastle &&
                                          newPiecePosition newPosition == (7, 7) = (True, adjustPosition (7, 7) (5, 7) $ adjustPosition (4, 7) (6, 7) board)
                                        | otherwise = (False, board) where
                                                checkValidWhiteCastle = checkIfSame (checkPosition (piecePosition position) board)       (Piece White King ActiveInitial) &&
                                                                        checkIfSame (checkPosition (newPiecePosition newPosition) board) (Piece White Rook ActiveInitial)
                                                checkValidBlackCastle = checkIfSame (checkPosition (piecePosition position) board)       (Piece Black King ActiveInitial) &&
                                                                        checkIfSame (checkPosition (newPiecePosition newPosition) board) (Piece Black Rook ActiveInitial)
                                        
-- if either position is empty then we know it's not a valid castle
checkIfSame :: Maybe Piece -> Piece -> Bool
checkIfSame Nothing piece    = False
checkIfSame maybePiece piece = fromJust maybePiece == piece

-- dependent on side we have to move our pawn to its new position and then remove the opposing pawn that is either above or below that space
updatePassant :: (Board, PieceSide) -> Position -> NewPosition -> (Board, PieceSide)
updatePassant (board, side) position newPosition = ((adjustPosition (piecePosition position) (newPiecePosition newPosition) (removePiece (add (newPiecePosition newPosition) (snd (enPassant (board, side) position newPosition)) ) board)), oppositeSide side)

-- if the side performing en passant is white then we check the space below the pawns new position otherwise it's the space above
enPassant :: (Board, PieceSide) -> Position -> NewPosition -> (Bool, (Int, Int))
enPassant (board, side) position newPosition | checkPosition (newPiecePosition newPosition) board == Nothing &&
                                               checkIfSame (checkPosition (piecePosition position) board) (Piece White Pawn ActiveMoved)  &&
                                               validCapturePawn (board, side) position newPosition &&
                                               checkIfSame (checkPosition (add (newPiecePosition newPosition) (0, -1)) board) (Piece Black Pawn PawnTwice) = (True,(0, -1))
                                             | checkPosition (newPiecePosition newPosition) board == Nothing &&
                                               checkIfSame (checkPosition (piecePosition position) board) (Piece Black Pawn ActiveMoved)  &&
                                               validCapturePawn (board, side) position newPosition &&
                                               checkIfSame (checkPosition (add (newPiecePosition newPosition) (0, 1)) board) (Piece White Pawn PawnTwice) = (True,(0, 1))
                                             | otherwise = (False, (0, 0))

-- in order for an en passant to be valid the tile on which the pawn will end up must also be empty
validCapturePawn :: (Board, PieceSide) -> Position -> NewPosition -> Bool
validCapturePawn (board, side) position newPosition = isValidCapture (checkPosition (piecePosition position) board) (piecePosition position) (newPiecePosition newPosition) &&
                                                      ((checkPosition (newPiecePosition newPosition) board) == Nothing                                                      ||
                                                      (pieceSide $ fromJust $ checkPosition (newPiecePosition newPosition) board) /= (pieceSide $ fromJust $ checkPosition (piecePosition position) board))

-- takes two positions on the board and checks if every position between those two positions is empty using Map.lookup
-- for knights this doesn't matter
checkForBlockers :: Board -> Position -> NewPosition -> Bool
checkForBlockers board position newPosition | (pieceType $ fromJust $ checkPosition (piecePosition position) board) == Knight = False
                                            | checkForBlockersCalc board position newPosition = False
                                            | otherwise = True

-- if the distance travelled by a piece is 1 then it must be valid
-- take the list of booleans generated by the tiles between the two positions
-- apply the or operand on every element of each list and if they are all false then we know that the space between two points is empty
checkForBlockersCalc :: Board -> Position -> NewPosition -> Bool
checkForBlockersCalc board position newPosition | maxDistance (piecePosition position) (newPiecePosition newPosition) == 1 = True
                                                | not $ or $ generateBlockList board position newPosition = True
                                                | otherwise = False

-- generates list of coordinates between two positions and looks up each position in the map to check if empty 
generateBlockList :: Board -> Position -> NewPosition -> [Bool]
generateBlockList board (Position (x1, y1)) (NewPosition (x2, y2)) = [(checkPosition positionToCheck board) /= Nothing | positionToCheck <- positionsList ] where
        distance = maxDistance (x1, y1) (x2, y2)
        (hor, vert) = ((x2 - x1) `div` distance, (y2 - y1) `div` distance)
        positionsList = [(x1 + (alpha*hor), y1 + (alpha*vert)) | alpha <- [1 .. (distance - 1)]]

-- take the largest difference between either the x coordinates or the y coordinates
maxDistance :: PiecePosition -> PiecePosition -> Int
maxDistance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

-- if the map location is empty then do not perform the comparison
-- otherwise it will lead to catastrophic failure
maybePieceSideToBool :: Maybe Piece -> PieceSide -> Bool
maybePieceSideToBool Nothing side = True
maybePieceSideToBool (Just piece) side | pieceSide (piece) /= side = True
                                       | otherwise                 = False 

-- gives back opposite side
oppositeSide :: PieceSide -> PieceSide
oppositeSide side   | side == White = Black
                    | otherwise     = White

-- to extract the old position marker from the current state
stateToTuple :: State -> (Int, Int)
stateToTuple state = piecePosition $ position state

-- to extract the new position marker from the current state
stateToNewTuple :: State -> (Int, Int)
stateToNewTuple state = newPiecePosition $ newPosition state

-- limits the space in which the user can select pieces so that it's confined within the map
add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (u, v) | x + u > 7 || x + u < 0 = (x, y)
                  | y + v > 7 || y + v < 0 = (x, y)
                  |otherwise = (x+u, y+v)

-- these events handle the movement of the selection marker which determines what piece a player wants to move
takeUserMove :: Event -> State -> State
takeUserMove (EventKey (SpecialKey KeyUp)      Down _ _) state = CurrentState (board state) (currentTurn state) (Position (add (stateToTuple state) (0, 1)))  (NewPosition $ stateToNewTuple state)
takeUserMove (EventKey (SpecialKey KeyDown)    Down _ _) state = CurrentState (board state) (currentTurn state) (Position (add (stateToTuple state) (0, -1))) (NewPosition $ stateToNewTuple state)
takeUserMove (EventKey (SpecialKey KeyLeft)    Down _ _) state = CurrentState (board state) (currentTurn state) (Position (add (stateToTuple state) (-1, 0))) (NewPosition $ stateToNewTuple state)
takeUserMove (EventKey (SpecialKey KeyRight)   Down _ _) state = CurrentState (board state) (currentTurn state) (Position (add (stateToTuple state) (1, 0)))  (NewPosition $ stateToNewTuple state)

-- events for moving the marker where the user wants to move a piece to
takeUserMove (EventKey (Char 'w') Down _ _) state = CurrentState (board state) (currentTurn state) (Position $ stateToTuple state)  (NewPosition (add (stateToNewTuple state) (0, 1))) 
takeUserMove (EventKey (Char 's') Down _ _) state = CurrentState (board state) (currentTurn state) (Position $ stateToTuple state) (NewPosition (add (stateToNewTuple state) (0, -1)))
takeUserMove (EventKey (Char 'a') Down _ _) state = CurrentState (board state) (currentTurn state) (Position $ stateToTuple state) (NewPosition (add (stateToNewTuple state) (-1, 0)))
takeUserMove (EventKey (Char 'd') Down _ _) state = CurrentState (board state) (currentTurn state) (Position $ stateToTuple state)  (NewPosition (add (stateToNewTuple state) (1, 0)))

-- when the player believes that they have made an appropriate move then we can update the board state
takeUserMove (EventKey (Char 'q') Down _ _) state = CurrentState (fst $ updateBoard (board state, currentTurn state) (position state) (newPosition state)) (snd $ updateBoard (board state, currentTurn state) (position state) (newPosition state)) (Position $ stateToTuple state) (NewPosition $ stateToNewTuple state)
-- any other key then nothing happens
takeUserMove e s = s

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Section for converting the current state of the game to a picture image composed of multiple smaller pictures. Some images are taken from the assets file and added to the picture.    |
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

boardLayoutHorizontal :: Picture
boardLayoutHorizontal =  translate (-370) 420 (scale 0.532 0.5 (Text "A B C D E F G H"))

-- creating a vertical picture containing the numbers 1 .. 8
verticalNumbers :: Picture
verticalNumbers = Pictures [ number (Text (show (fst char))) (snd char) | char <- zip [1..8] [-375, -275..325]  ]

number :: Picture -> Float -> Picture
number char y = translate (-450) y (scale 0.5 0.532 char)

-- defines alternating squares for the chessboard
squares :: Picture
squares = Pictures [ square x y | x<-[-400, -200..200], y<-[-400, -200..200] ] 

shiftedSquares :: Picture
shiftedSquares = Pictures [ square x y | x<-[-300, -100 .. 300], y<-[-300, -100 .. 300] ] 

-- tiles defined as black squares of 100x100 size
square :: Float -> Float -> Picture
square x y = Polygon [(x,y), (x+100,y), (x+100,y+100), (x,y+100) ]

frame :: Float -> Float -> Picture
frame x y = rectangleWire x y

-- title card
meme2 :: Picture -> Picture
meme2 toLargen = translate (-700) 0 (scale 0.5 0.5 toLargen)

-- converts the map of board pieces derived from state to the corresponding image files we pulled from the assets folder
drawPieceFromState :: [Picture] -> Board -> Picture
drawPieceFromState graphics state = Pictures [ translate ((fromIntegral (fst $ fst x) * 100) - 350) ((fromIntegral (snd $ fst x) * 100) - 335) (scale 0.115 0.115 (whichImage graphics (snd x))) | x <- Map.toList state ] where
        whichImage :: [Picture] -> Piece -> Picture
        whichImage graphics piece | pieceType piece == Pawn &&   pieceSide piece == White = (graphics !! 2)
                                  | pieceType piece == Pawn &&   pieceSide piece == Black = (graphics !! 3)
                                  | pieceType piece == Knight && pieceSide piece == White = (graphics !! 4)
                                  | pieceType piece == Knight && pieceSide piece == Black = (graphics !! 5)
                                  | pieceType piece == Rook &&   pieceSide piece == White = (graphics !! 6)
                                  | pieceType piece == Rook &&   pieceSide piece == Black = (graphics !! 7)
                                  | pieceType piece == Bishop && pieceSide piece == White = (graphics !! 8)
                                  | pieceType piece == Bishop && pieceSide piece == Black = (graphics !! 9)
                                  | pieceType piece == King &&   pieceSide piece == White = (graphics !! 10)
                                  | pieceType piece == King &&   pieceSide piece == Black = (graphics !! 11)
                                  | pieceType piece == Queen &&  pieceSide piece == White = (graphics !! 12)
                                  | pieceType piece == Queen &&  pieceSide piece == Black = (graphics !! 13)

backgroundColour :: Color
backgroundColour = greyN 0.77

-- reads the current turn from the state data type and returns the appropriate turn text
turnText :: State -> Picture
turnText state | currentTurn state == Black = translate 500 0 (scale 0.25 0.25 (Text "Turn: Black to move :V"))
                            | otherwise     = translate 500 0 (scale 0.25 0.25 (Text "Turn: White to move >:)"))

-- for marking where the user is currently pointing to the piece they want to move
positionMarker :: Picture -> Position -> Picture
positionMarker image position =  translate (((fromIntegral $ fst $ piecePosition position)*100)-350) (((fromIntegral $ snd $ piecePosition position)*100)-350) (scale 0.125 0.125 image)

-- where said piece is going as a red arrow image
newPositionMarker :: Picture -> NewPosition -> Picture
newPositionMarker image newPosition =  translate (((fromIntegral $ fst $ newPiecePosition newPosition)*100)-350) (((fromIntegral $ snd $ newPiecePosition newPosition)*100)-350) (scale 0.125 0.125 image)

-- coalesces every image into the game window which runs in full screen
drawWorld :: [Picture] -> State -> Picture
drawWorld graphics state = (squares <>
                            shiftedSquares <>
                            frame 800 800 <>
                            boardLayoutHorizontal <>
                            verticalNumbers <>
                            drawPieceFromState graphics (board state) <>
                            meme2 (graphics !! 1) <>
                            positionMarker (graphics !! 14) (position state) <>
                            newPositionMarker (graphics !! 15) (newPosition state) <>
                            turnText state)



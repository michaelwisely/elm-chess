import Graphics.Element (..)
import Graphics.Collage (..)
import Color
import Signal
import Signal (Signal)
import List
import Text
import Time
import Window

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type alias UserInput = {}


userInput : Signal UserInput
userInput =
    Signal.constant {}


type alias Input =
    { timeDelta : Float
    , userInput : UserInput
    }



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}

type Type
    = Pawn
    | Rook
    | Bishop
    | Knight
    | King
    | Queen

type Player
    = Black
    | White

type alias Piece = (Player, Type)

type alias Row = List (Maybe Piece)

type alias Board = List Row

type alias GameState = Board

defaultGame : GameState
defaultGame = [ [Just (Black, Rook), Just (Black, Knight), Just (Black, Bishop), Just (Black, Queen), Just (Black, King), Just (Black, Bishop), Just (Black, Knight), Just (Black, Rook)]
              , [Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn)]
              , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
              , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
              , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
              , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
              , [Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn)]
              , [Just (White, Rook), Just (White, Knight), Just (White, Bishop), Just (White, Queen), Just (White, King), Just (White, Bishop), Just (White, Knight), Just (White, Rook)]]




{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta, userInput} gameState =
    gameState



{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

pieceImage : Int -> (Player, Type) -> Element
pieceImage size (p, t) =
    let
        pieceType = case t of
                      Pawn -> "pawn"
                      Rook -> "rook"
                      Bishop -> "bishop"
                      Knight -> "knight"
                      King -> "king"
                      Queen -> "queen"
        playerColor = case p of
                        White -> "white"
                        Black -> "black"
        src = "images/pieces/" ++ playerColor ++ "-" ++ pieceType ++ ".svg"
    in
      image size size src

toCoord : Float -> Int -> Int -> (Float, Float)
toCoord size rank file =
    let
        x = (toFloat file * size) - size * 3.5
        y = (toFloat rank * size) - size * 3.5
    in
      (x, y)

drawSpace : Float -> Int -> Int -> Maybe Piece -> Form
drawSpace spaceSize rank file p =
    let
        pos = toCoord spaceSize rank file
        imageSize = round spaceSize
        color = if (rank + file) % 2 == 0 then
                    Color.white
                else
                    Color.blue
        space = square spaceSize
              |> filled color
              |> move pos
    in
      case p of
        Just piece -> group [space
                            , pieceImage imageSize piece
                                |> toForm
                                |> move pos]
        Nothing -> space

drawRow : Int -> Int -> Row -> List Form
drawRow pageWidth rank =
    let
        spaceSize = (toFloat pageWidth) / 8.0
    in
      List.indexedMap (drawSpace spaceSize rank)

drawBoard : Int -> Board -> List Form
drawBoard pageSize board = List.indexedMap (drawRow pageSize) board
                     |> List.concat

display : (Int,Int) -> GameState -> Element
display (w,h) gameState =
    let
        pageSize = List.minimum [w, h]
    in
      drawBoard pageSize gameState
          |> collage pageSize pageSize

{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Float
delta =
    Time.fps 30


input : Signal Input
input =
    Signal.sampleOn delta (Signal.map2 Input delta userInput)


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


main : Signal Element
main =
    Signal.map2 display Window.dimensions gameState

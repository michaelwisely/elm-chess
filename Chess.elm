module Chess where

import List
import Graphics.Element (Element, image)

type Kind
    = Pawn
    | Rook
    | Bishop
    | Knight
    | King
    | Queen

type Player
    = Black
    | White

type alias Piece = { player: Player,
                     kind: Kind,
                     rank: Int,
                     file: Int
                   }

type alias RowList = List (Maybe (Player, Kind))
type alias BoardList = List RowList

defaultBoard : BoardList
defaultBoard = [ [Just (Black, Rook), Just (Black, Knight), Just (Black, Bishop), Just (Black, Queen), Just (Black, King), Just (Black, Bishop), Just (Black, Knight), Just (Black, Rook)]
               , [Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn)]
               , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
               , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
               , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
               , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
               , [Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn)]
               , [Just (White, Rook), Just (White, Knight), Just (White, Bishop), Just (White, Queen), Just (White, King), Just (White, Bishop), Just (White, Knight), Just (White, Rook)]]

toPieces : BoardList -> List Piece
toPieces =
    let
        toPiece rank file m =
            case m of
              Just (player, kind) -> [{ player=player, kind=kind, rank=rank, file=file }]
              Nothing -> []

        fromRow rank = List.concat << List.indexedMap (toPiece rank)
    in
      List.concat << List.indexedMap fromRow


pieceImage : Piece -> Int -> Element
pieceImage { player, kind } size =
    let
        name = case kind of
                 Pawn -> "pawn"
                 Rook -> "rook"
                 Bishop -> "bishop"
                 Knight -> "knight"
                 King -> "king"
                 Queen -> "queen"
        color = case player of
                  White -> "white"
                  Black -> "black"
        src = "images/pieces/" ++ color ++ "-" ++ name ++ ".svg"
    in
      image size size src

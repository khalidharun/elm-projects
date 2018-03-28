import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Dict
import Array
import Debug

type alias Model =
  { game : Game
  , board: Board
  }

type alias Game =
  { height: Int
  , width: Int
  , squares: Array.Array Bool
  }

type alias Board =
  { height: Int
  , width: Int
  , squares: Array.Array Square
  }

type alias Square =
  { visibility: Visibility
  , score: Maybe Int
  }

type Visibility = Hidden | Showing

type alias Msg = (Int, Int)

model : Model
model =
  let
    game = emptyGame 5 5
  in
    {
      game = game
    , board = computeBoard game
    }

emptyGame : Int -> Int -> Game
emptyGame height width =
  let
      area = height * width
      squares =
        True
        |> List.repeat area
        |> Array.fromList
  in
      Game height width squares

getIndex : { a | width: Int } -> (Int, Int) -> Int
getIndex board (row, col) = (row * board.width) + col

add (a,b) (c,d) = (a + c, b + d)

isOutOfBounds : { a | height: Int, width: Int } -> (Int, Int) -> Bool
isOutOfBounds game (i, j) =
  if i >= 0 && i < game.height && j >= 0 && j < game.width then
    True
  else
    False

computeSquare : Game -> (Int, Int) -> Square
computeSquare game (row, col) =
  let
      offset =
        [ (-1, -1), (-1, 0), (-1, 1)
        , ( 0, -1),          ( 0, 1)
        , ( 1, -1), ( 1, 0), ( 1, 1)
        ]
      toInt x =
        case x of
          Just True -> 1
          _ -> 0
      label = "Index (" ++ toString(row) ++ ", " ++ toString(col) ++ ")"
      total =
        offset
        |> List.map (add (row, col))
        |> List.filter (isOutOfBounds game)
        |> Debug.log label
        |> List.map (getIndex game)
        |> Debug.log label
        |> List.map (flip Array.get game.squares)
        |> List.map toInt
        |> List.sum
        |> floor
  in
      Square Hidden (Just total)

computeBoard : Game -> Board
computeBoard game =
  let
      count = game.height * game.width
      squares =
        List.range 0 (count - 1)
        |> List.map (\n -> (n // game.width, n % game.width))
        |> Debug.log "Coords"
        |> List.map (computeSquare game)
        |> Debug.log "Scores"
        |> Array.fromList
  in
      Board game.height game.width squares

update : Msg -> Model -> Model
update msg model =
  model

getScore : Maybe Square -> String
getScore maybeSquare =
  case maybeSquare of
    Just square ->
      case square.score of
        Nothing -> "x"
        Just n -> toString(n)
    _ -> "Error"

drawSquare : Board -> Int -> Int -> Html.Html Msg
drawSquare board row col =
  let
    index = getIndex board (row, col)
    renderButton x = button [onClick (row, col)] [text x]
  in
      board.squares
      |> Array.get index
      |> getScore
      |> renderButton

drawBoardRow : Board -> Int -> Html.Html Msg
drawBoardRow board row =
  board.width - 1
  |> List.range 0
  |> List.map (drawSquare board row)
  |> div [ class "board-row" ]

drawBoard : Board -> Html.Html Msg
drawBoard board =
  board.height - 1
  |> List.range 0
  |> List.map (drawBoardRow board)
  |> div [ class "board" ]

drawButtons : Html.Html Msg
drawButtons =
  div [] [ button [] [ text "New Game" ] ]

view model =
  div []
  [ text "Minesweeper"
  , div [] [drawBoard model.board]
  , div [] [drawButtons]
  ]

main =
  Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }

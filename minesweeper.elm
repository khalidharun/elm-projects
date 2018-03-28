import Html exposing (Html, button, div, text, h1)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Json
import Dict
import Array
import Debug

main =
  Html.beginnerProgram { model = model , view = view , update = update }

model : Model
model = resetGame

update : Msg -> Model -> Model
update msg model =
  case msg of
    ResetGame -> resetGame
    Reveal point -> { model | board = revealSquare model.board point }
    Flag point -> { model | board = flagSquare model.board point }

type alias Model = { game : Game , board: Board }
type alias Game  = { height: Int , width: Int , squares: Array.Array Bool }
type alias Board = { height: Int , width: Int , squares: Array.Array Square }
type alias Square = { visibility: Visibility , score: Maybe Int }
type alias Point = (Int, Int)
type Visibility = Hidden | Uncovered | Flagged

type Msg = ResetGame | Reveal Point | Flag Point

resetGame : Model
resetGame =
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

getIndex : { a | width: Int } -> Point -> Int
getIndex board (row, col) =
  (row * board.width) + col

add : Point -> Point -> Point
add (a,b) (c,d) =
  (a + c, b + d)

isOutOfBounds : { a | height: Int, width: Int } -> Point -> Bool
isOutOfBounds game (i, j) =
  if i >= 0 && i < game.height && j >= 0 && j < game.width then
    True
  else
    False

computeSquare : Game -> Point -> Square
computeSquare game point =
  let
      (row, col) = point
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
        |> List.map (add point)
        |> List.filter (isOutOfBounds game)
        |> List.map (getIndex game)
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

getScore : Maybe Square -> String
getScore maybeSquare =
  case maybeSquare of
    Just square ->
      case square.visibility of
        Hidden -> " "
        Flagged -> "F"
        Uncovered ->
          case square.score of
            Nothing -> "E"
            Just n -> toString(n)
    _ -> "Error"


isFlagged : Square -> Bool
isFlagged square =
  case square.visibility of
    Flagged -> True
    _ -> False

computeRemaining : Model -> Int
computeRemaining model =
  let
      numTotal =
        model.game.squares
        |> Array.filter (\n -> True == n)
        |> Array.length

      numFlagged =
        model.board.squares
        |> Array.filter isFlagged
        |> Array.length
  in
    numTotal - numFlagged

onRightClick message =
    onWithOptions
        "contextmenu"
            { stopPropagation = True
               , preventDefault = True
                }
                    (Json.succeed message)

revealSquare : Board -> Point -> Board
revealSquare =
  updateSquare Uncovered

flagSquare : Board -> Point -> Board
flagSquare =
  updateSquare Flagged

updateSquare : Visibility -> Board -> Point -> Board
updateSquare visibility board point =
  let
      (row, col) = point
      idx = getIndex board point
      curr =
        case Array.get idx board.squares of
          Just x -> x
          Nothing -> Square Hidden Nothing
      next = { curr | visibility = visibility }
      squares = Array.set idx next board.squares
  in
      { board | squares = squares }

drawSquare : Board -> Int -> Int -> Html.Html Msg
drawSquare board row col =
  let
      point = (row, col)
      index = getIndex board point
      renderButton x =
        button  [ class "square"
                , onClick (Reveal point)
                , onRightClick (Flag point)
                ]
                [ text x ]
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
  div [ class "buttons" ] [ button [ onClick ResetGame ] [ text "New Game" ] ]

drawCounter : Model -> Html.Html Msg
drawCounter model =
  let
      msg =
        model
        |> computeRemaining
        |> toString
        |> ((++) "Mines Remaining: ")
  in
    div [ class "counter" ] [ text msg ]

view : Model -> Html.Html Msg
view model =
  div []
  [  h1 [] [ text "Minesweeper" ]
  , drawCounter model
  , drawBoard model.board
  , drawButtons
  ]

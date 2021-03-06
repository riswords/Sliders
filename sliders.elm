module Sliders where

import Debug
import Signal
import List exposing (..)
import Keyboard
import Maybe exposing (withDefault)
import Random exposing (Seed, initialSeed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Graphics.Element exposing (show)
import Graphics.Collage exposing (..)
import Text exposing (fromString, Text, Style)
import Color exposing (..)

main : Signal Html
main = Signal.map view (Signal.foldp update init Keyboard.arrows)

type alias Model = {
    grid : List GridRow
    , seed : Seed
    , gameInProgress : Bool
    }

type alias GridRow = List GridSquare
type alias GridSquare = Maybe Block
type alias Block = Int

type alias Keys = { 
    x : Int
    , y : Int
    }

init : Model 
init = spawnSquare {
    grid = initGrid 4
    , seed = initialSeed 0
    , gameInProgress = True
    }

initGrid : Int -> List GridRow
initGrid size = repeat size (repeat size Nothing)


initTestModel : Model
initTestModel = {
    grid = initTestGrid
    , seed = initialSeed 0
    , gameInProgress = True
    }


initTestGrid : List GridRow
initTestGrid = [  [Just 2, Just 4, Just 8, Just 16]
                , [Just 32, Just 64, Just 128, Just 256]
                , [Just 512, Just 1024, Just 2048, Just 4096]
                , [Just 8192, Just 16384, Just 32768, Nothing]
                ]
                
-- UPDATE
type Action = 
    SlideRight
    | SlideLeft
    | SlideUp
    | SlideDown
    | None
    | NewGame


update : Keys -> Model -> Model
update keys model = handleAction (parseAction keys) model

handleAction : Action -> Model -> Model
handleAction action model =
    let canSlide = canSlideAtAll model.grid
    in if not canSlide
      then { model | gameInProgress <- False }
      else
      let
        firstRotation =
            case action of
                SlideRight -> model.grid
                SlideLeft -> rotateCW model.grid |>
                            rotateCW
                SlideUp -> rotateCW model.grid
                SlideDown -> rotateCW model.grid |>
                            rotateCW |>
                            rotateCW
                None -> model.grid
                NewGame -> initGrid (length model.grid)
        slidGridResults = map slideRight firstRotation
        didSlide =
            case action of 
                None -> False
                NewGame -> True
                _ -> any fst slidGridResults
        repaddedGrid = map snd slidGridResults
        secondRotation = 
            case action of
                SlideRight -> repaddedGrid
                SlideLeft -> rotateCW repaddedGrid |>
                            rotateCW
                SlideUp -> rotateCW repaddedGrid |>
                            rotateCW |>
                            rotateCW
                SlideDown -> rotateCW repaddedGrid
                _-> model.grid
    in if didSlide
        then spawnSquare { model | grid <- secondRotation }
        else model

canSlideAtAll : List GridRow -> Bool
canSlideAtAll grid
    =  any ((==) True) (map rowWithOpenSquare grid)
    || any ((==) True) (map rowWithPairs grid)
    || any ((==) True) (map rowWithPairs (rotateCW grid))

rowWithOpenSquare : GridRow -> Bool
rowWithOpenSquare row = any ((==) True) <| map ((==) Nothing) row

rowWithPairs : GridRow -> Bool
rowWithPairs row =
    let foldFun : GridSquare -> (GridSquare, Bool) -> (GridSquare, Bool)
        foldFun cell (leftCell, foundMatch) =
            case (leftCell, foundMatch) of
                (_, True) -> (cell, True)
                (Nothing, False) -> (cell, False)
                (Just lval, False) -> case cell of
                                        Nothing -> (cell, False)
                                        Just val -> (cell, val == lval)
    in snd <| foldl foldFun (Nothing, False) row

spawnSquare : Model -> Model
spawnSquare model =
    let grid = model.grid

        markEmptyLocs : List (List (Maybe (Int, Int)))
        markEmptyLocs = 
            indexedMap (\row gridRow ->
                            indexedMap 
                                (\col cell -> 
                                    case cell of
                                        Nothing -> Just (row, col)
                                        Just _ -> Nothing)
                                gridRow)
                        grid
            
        emptyLocs = concatMap (filter (\i -> case i of 
                                    Nothing -> False
                                    Just _ -> True)) markEmptyLocs
        numEmptySquares = length emptyLocs
        (spawnIndex, seed') = Random.generate (Random.int 0 (numEmptySquares-1)) model.seed
        spawnLoc = withDefault Nothing (get spawnIndex emptyLocs)
        (spawnVal, seed'') = makeSpawnValue seed'
    in case spawnLoc of
        Nothing -> { model | seed <- seed' }
        Just (row, col) ->
            { model | 
                grid <- spawnGridSquare grid row col spawnVal
                , seed <- seed'
            }

spawnGridSquare : List GridRow -> Int -> Int -> Int -> List GridRow
spawnGridSquare grid row col spawnVal =
    let gridRow = withDefault [] (get row grid)
        updatedRow = set col (Just spawnVal) gridRow
    in set row updatedRow grid

makeSpawnValue : Seed -> (Int, Seed)
makeSpawnValue seed = 
    let (coinFlip0, seed0) = Random.generate (Random.int 0 1) seed
        (coinFlip, seed') = if coinFlip0 == 0
                                then (coinFlip0, seed0)
                                else Random.generate (Random.int 0 1) seed0
    in ((coinFlip * 2) + 2, seed')

get : Int -> List a -> Maybe a
get index ls = case ls of
                    [] -> Nothing
                    (x :: xs) -> if index <= 0
                                    then Just x
                                    else get (index - 1) xs

set : Int -> a -> List a -> List a
set index val ls = 
    if index <= 0 || ls == []
        then val :: (withDefault [] (tail ls))
        else  (withDefault val (head ls)) :: (set (index - 1) val (withDefault [] (tail ls)))

parseAction : Keys -> Action
parseAction {x, y} = 
    case (x, y) of
        (-1, 0) -> SlideLeft
        (1, 0) -> SlideRight
        (0, 1) -> SlideUp
        (0, -1) -> SlideDown
        (_, _) -> None

slideRight : GridRow -> (Bool, GridRow)
slideRight row = 
    let sliderFun : GridSquare -> (GridSquare -> Bool, List (Int, Int))
                -> (GridSquare -> Bool, List (Int, Int))
        sliderFun square (checkSlide, rowResult) =
            case square of
                Nothing ->
                    (\s -> case s of
                             Nothing -> checkSlide Nothing
                             Just sq -> True, rowResult)
                Just sq ->
                    case head rowResult of
                      Nothing -> (\s -> checkSlide square,
                                        (0, sq) :: rowResult)
                      Just lastHd ->
                          if (sq == snd lastHd) && (0 == fst lastHd)
                          then (\s -> True,
                                      (sq, sq) ::
                                        (withDefault [] (tail rowResult)))
                          else (\s -> checkSlide square,
                                      (0, sq) :: rowResult)
        (checkSlide, slidRow) = foldr sliderFun (\s -> False, []) row
        slidResult = map (\p -> Just ((fst p) + (snd p))) slidRow
    in (checkSlide (withDefault Nothing (head row)),
        padLeft (length row) Nothing slidResult)


padLeft : Int -> a -> List a -> List a
padLeft len val ls = if length ls >= len
                        then ls
                        else padLeft len val (val :: ls)

rotateCW : List GridRow -> List GridRow
rotateCW grid = 
    let hds = map (\gridRow -> case head gridRow of
                                Nothing -> Nothing
                                Just hd -> hd) grid
        tls = map (\gridRow -> case tail gridRow of
                                Nothing -> gridRow
                                Just tl -> tl) grid
    in if all isEmpty grid
        then []
        else (reverse hds) :: (rotateCW tls)

-- VIEW
view : Model -> Html
view model = div [] [
        makeGameOverHtml model.gameInProgress
        , table [align "center"] (gridToTableRows model.grid)
    ]

makeGameOverHtml : Bool -> Html
makeGameOverHtml gameInProgress = 
    if gameInProgress
    then Html.text ""
    else Html.text "Game Over!"

gridToTableRows : List GridRow -> List Html
gridToTableRows grid = 
    let cellView : GridSquare -> List Html
        cellView cell = 
            case cell of
              Nothing -> [fromElement (
                             collage 75 75 [
                               Graphics.Collage.text (styledText "   ")
                           , square 75 |> filled clearGrey ]
                          )]
              Just i -> [fromElement (
                            collage 75 75 [
                              square 75 |> filled (getNumberColor i)
                            , Graphics.Collage.text (styledText (toString i))
                         ])]
        
        cellViewData : List (List (List Html))
        cellViewData = map (map cellView) grid

        gridRows : List (List Html)
        gridRows = map (map (td [])) cellViewData
    in map (tr []) gridRows

styledText : String -> Text
styledText string = Text.style textStyle (fromString string)

clearGrey : Color
clearGrey =
  rgba 200 200 200 0.6


cssStyle : Html.Attribute
cssStyle = style [
    ("align", "center")
    ]

textStyle : Text.Style
textStyle = { 
    typeface = [ "Helvetica", "sans" ]
    , height   = Just 24
    , color    = black
    , bold     = True
    , italic   = False
    , line     = Nothing
    }

getNumberColor : Int -> Color
getNumberColor num = 
    let index = floor (logBase 2 (toFloat num)) - 1
        colorArray = [
        rgb 153 255 153, rgb 153 255 255, rgb 255 255 153, rgb 255 153 153
        , rgb 178 102 255, rgb 255 175 94, rgb 255 102 204, rgb 102 178 255
        , rgb 255 36 58, rgb 0 204 102, rgb 0 128 255, rgb 255 0 127
        , rgb 255 153 51, rgb 255 0 200, rgb 0 255 0]
    in withDefault clearGrey (get index colorArray)


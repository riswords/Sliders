module Sliders where

import Debug
import Signal
import Html exposing (..)
import List exposing (..)
import Keyboard
import Maybe exposing (withDefault)
import Random exposing (Seed, initialSeed)

main : Signal Html
main = Signal.map view (Signal.foldp update initTestModel Keyboard.arrows)

type alias Model = {
    grid : List GridRow
    , size : Int
    , seed : Seed
    }

type alias GridRow = List GridSquare
type alias GridSquare = Maybe Block
type alias Block = Int

type alias Keys = { 
    x : Int
    , y : Int
    }

init : Model 
init = {
    grid = initGrid 4
    , size = 4
    , seed = initialSeed 0
    }

initGrid : Int -> List GridRow
initGrid size = repeat size (repeat size Nothing)


initTestModel : Model
initTestModel = {
    grid = initTestGrid
    , size = 4
    , seed = initialSeed 0
    }


initTestGrid : List GridRow
initTestGrid = [  [Just 1, Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Just 2, Nothing]
                , [Nothing, Nothing, Nothing, Nothing]
                , [Just 1, Nothing, Nothing, Nothing]
                ]
                
-- UPDATE
type Action = 
    SlideRight
    | SlideLeft
    | SlideUp
    | SlideDown
    | None


update : Keys -> Model -> Model
update keys model = handleAction (parseAction keys) model

handleAction : Action -> Model -> Model
handleAction action model =
    case action of
        SlideRight -> 
            spawnSquare <| { model | 
                grid <- map slideRight model.grid
            }

        SlideLeft -> 
            spawnSquare <| { model | 
                grid <- 
                    rotateCW model.grid |>
                    rotateCW |>
                    map slideRight |>
                    rotateCW |>
                    rotateCW
            }

        SlideUp -> 
            spawnSquare <| { model | 
                grid <- 
                    rotateCW model.grid |>
                    map slideRight |>
                    rotateCW |>
                    rotateCW |> 
                    rotateCW
            }

        SlideDown ->
            spawnSquare <| { model | 
                grid <- 
                    rotateCW model.grid |>
                    rotateCW |>
                    rotateCW |>
                    map slideRight |>
                    rotateCW
            }
        
        None -> model


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
        (spawnIndex, seed') = Random.generate (Random.int 0 numEmptySquares) model.seed
        spawnLoc = withDefault Nothing (get spawnIndex emptyLocs)
        (spawnVal, seed'') = makeSpawnValue seed'
        temp = Debug.watch "" (emptyLocs, spawnLoc)
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
    let (coinFlip, seed') = Random.generate (Random.int 0 1) seed
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

slideRight : GridRow -> GridRow
slideRight row = 
    let sliderFun square rowResult = 
        case (square, head rowResult) of
            (Nothing, _) -> rowResult
            (Just sq, Nothing) -> (Just sq) :: rowResult
            (Just sq, Just lastHd) -> 
                case lastHd of
                    Nothing -> (Just sq) :: rowResult
                    Just lastNum -> 
                        if sq == lastNum
                            then Nothing :: ((Just (sq + sq)) :: (withDefault [] (tail rowResult)))
                            else (Just sq) :: rowResult
        rowLen = length row
    in 
        padLeft rowLen Nothing (foldr sliderFun [] row)

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
        table [] (gridToTableRows model.grid)
    ]

gridToTableRows : List GridRow -> List Html
gridToTableRows grid = 
    let cellView : GridSquare -> List Html
        cellView cell = case cell of
                            Nothing -> [text "_"]
                            Just i -> [text (toString i)]
        
        cellViewData : List (List (List Html))
        cellViewData = map (map cellView) grid

        gridRows : List (List Html)
        gridRows = map (map (td [])) cellViewData
    in map (tr []) gridRows
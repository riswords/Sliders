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
    let firstRotation = 
            case action of
                SlideRight -> model.grid
                SlideLeft -> rotateCW model.grid |>
                            rotateCW
                SlideUp -> rotateCW model.grid
                SlideDown -> rotateCW model.grid |>
                            rotateCW |>
                            rotateCW
                None -> model.grid
        slidGridResults = map slideRight firstRotation
        didSlide =
            case action of 
                None -> False
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
                None -> model.grid
        temp0 = Debug.watch "firstRotation" firstRotation
        temp1 = Debug.watch "slidGridResults" slidGridResults
    in if (Debug.watch "didSlide" didSlide)
        then spawnSquare { model | grid <- secondRotation }
        else model

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

slideRight : GridRow -> (Bool, GridRow)
slideRight row = 
    let sliderFun : GridSquare -> (GridSquare -> Bool, List (Int, Int)) -> (GridSquare -> Bool, List (Int, Int))
        sliderFun square (checkSlide, rowResult) =
            case square of
                Nothing -> (\s -> case s of 
                                    Nothing -> checkSlide Nothing
                                    Just sq -> True, rowResult)
                Just sq -> case head rowResult of
                                Nothing -> (\s -> checkSlide square, (0, sq) :: rowResult)
                                Just lastHd -> if (sq == snd lastHd) && (0 == fst lastHd)
                                                then (\s -> True, (sq, sq) :: (withDefault [] (tail rowResult)))
                                                else (\s -> checkSlide square, (0, sq) :: rowResult)
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
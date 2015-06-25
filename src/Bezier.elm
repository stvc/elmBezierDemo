module Bezier where

import Array
import Array exposing (Array)
import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element, show)
import Maybe
import MouseEvent
import Point exposing (Point)


{-- Constants --}

radius = 5


{-- Model --}

type alias Curve =
    { points : Array Point
    , selectedIndex : Maybe Int
    }


{-- Update --}

type Action
    = Select Point
    | MoveSelected Point
    | Deselect
    | NoOp


mouseActionToBezierAction : MouseEvent.MouseAction -> Action
mouseActionToBezierAction mEvent =
    case mEvent of
        MouseEvent.Click p ->
            Select p

        MouseEvent.Release _ ->
            Deselect

        MouseEvent.Drag p ->
            MoveSelected p

        _ ->
            NoOp


update : Action -> Curve -> Curve
update act curve =
    case act of
        NoOp ->
            curve

        Select point ->
            let isOnPoint i p =
                    if calculateDistance point p < radius
                        then (True, i)
                        else (False, i)
                getIndexOfPointNearClick =
                    Array.indexedMap isOnPoint
                        >> Array.filter fst
                        >> Array.map snd
                        >> Array.get 0
            in
                { curve | selectedIndex <- getIndexOfPointNearClick curve.points }

        MoveSelected point ->
            case curve.selectedIndex of
                Nothing ->
                    curve
                Just index ->
                    { curve | points <- Array.set index point curve.points }

        Deselect ->
            { curve | selectedIndex <- Nothing }


{-- View --}

render : (Int, Int) -> Curve -> Element
render (width, height) curve =
    collage width height
        [ drawCubicCurve curve
        , drawControlLines curve
        , drawControlPoints curve
        ]


drawControlPoints : Curve -> Form
drawControlPoints curve =
    let outlinedFilledCircle = group
            [ circle 5.0 |> filled Color.white
            , circle 5.0 |> outlined (solid Color.blue)
            ]
        renderPoint p = outlinedFilledCircle |> move p
    in
        group <| Array.toList <| Array.map renderPoint curve.points


drawControlLines : Curve -> Form
drawControlLines =
    traced (dotted Color.blue)
        << Array.toList
        << .points


drawCubicCurve : Curve -> Form
drawCubicCurve curve =
    let tVals = List.map (\ t -> t * 0.01 ) [1..100]
        curvePoints = List.map (getCubicCurvePoint curve) tVals
        firstPoint = Maybe.withDefault (0, 0) <| Array.get 0 curve.points
        allPoints = firstPoint :: curvePoints
    in
        traced (solid Color.black)
            <| allPoints


{-- Default Curve --}

defaultCurve  : Curve
defaultCurve  =
    let ps = Array.fromList
            [ (-200,  -50)
            , (-100,  150)
            , ( 100, -150)
            , ( 200,   50)
            ]
    in
        { points = ps, selectedIndex = Nothing }


{-- Helper Functions --}

{-| Calculate the distance between two points -}
calculateDistance : Point -> Point -> Float
calculateDistance (x1, y1) (x2, y2) =
    let lenX = x1 - x2
        lenY = y1 - y2
    in
        sqrt <| lenX ^ 2 + lenY ^ 2


{-| Calculate a point on the curve -}
getCubicCurvePoint : Curve -> Float -> Point
getCubicCurvePoint curve t =
    let getPoint i = Maybe.withDefault (0, 0) <| Array.get i curve.points
        p0 = getPoint 0
        p1 = getPoint 1
        p2 = getPoint 2
        p3 = getPoint 3
        subp0 = getSubPoint t p0 p1
        subp1 = getSubPoint t p1 p2
        subp2 = getSubPoint t p2 p3
        subsubp0 = getSubPoint t subp0 subp1
        subsubp1 = getSubPoint t subp1 subp2
    in
        getSubPoint t subsubp0 subsubp1

{-| Calculate a subpoint on a line -}
getSubPoint : Float -> Point -> Point -> Point
getSubPoint t (p1x, p1y) (p2x, p2y) =
    let px = (1 - t) * p1x + t * p2x
        py = (1 - t) * p1y + t * p2y
    in (px, py)


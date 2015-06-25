module MouseEvent
    ( MouseAction (..)
    , mouseAction
    ) where

import Mouse
import Signal exposing ((<~), (~))
import Point exposing (Point)
import Window


type alias MouseState =
    { position : Point
    , buttonState : Bool
    , action : MouseAction
    }


type MouseAction
    = Click Point
    | Release Point
    | Move Point
    | Drag Point
    | NoOp


initialMouseState =
    { position = (0, 0)
    , buttonState = False
    , action = NoOp
    }

updateMouseState : (Point, Bool) -> MouseState -> MouseState
updateMouseState (point, clicked) state =
    let act = case clicked of
            True ->
                if clicked == state.buttonState
                    then Drag point
                    else Click point

            False ->
                if clicked == state.buttonState
                    then Move point
                    else Click point
    in
        { state
            | position <- point
            , buttonState <- clicked
            , action <- act }


relativeMouse : (Int, Int) -> (Int, Int) -> (Float, Float)
relativeMouse (width, height) (x, y) =
    let rx = (toFloat x) - (toFloat width) / 2
        ry = -1 * (toFloat y) + (toFloat height) / 2
    in
        (rx, ry)


{-- Signals --}

relativeMouseSignal : Signal Point
relativeMouseSignal =
    relativeMouse <~ Window.dimensions ~ Mouse.position


positionAndClickSignal : Signal (Point, Bool)
positionAndClickSignal = (,) <~ relativeMouseSignal ~ Mouse.isDown


mouseState : Signal MouseState
mouseState = Signal.foldp updateMouseState initialMouseState positionAndClickSignal


{-| This is the signal you want to use -}
mouseAction : Signal MouseAction
mouseAction = Signal.map .action mouseState


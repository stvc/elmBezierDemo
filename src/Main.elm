module Main where

import Bezier
import Graphics.Element exposing (..)
import MouseEvent
import Signal exposing ((<~), (~))
import Window


bezierActions : Signal Bezier.Action
bezierActions = Bezier.mouseActionToBezierAction <~ MouseEvent.mouseAction


bezierCurve : Signal Bezier.Curve
bezierCurve = Signal.foldp Bezier.update Bezier.defaultCurve bezierActions


main : Signal Element
main = Bezier.render <~ Window.dimensions ~ bezierCurve


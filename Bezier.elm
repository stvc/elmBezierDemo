import Window
import Mouse

type Point = (Float, Float)
type MoveablePoint = { pos : Point, selected : Bool }
type Curve = [ MoveablePoint ]

makeMoveablePoint : Point -> MoveablePoint
makeMoveablePoint p = { pos = p, selected = False }

outlinedFilledCircle : Form
outlinedFilledCircle = group [ circle 5.0 |> filled white
                             , circle 5.0 |> outlined (solid blue)
                             ]

-- get a sub-point on a line
getSubPoint : Float -> Point -> Point -> Point
getSubPoint t (p1x, p1y) (p2x, p2y) =
    let px = (1-t) * p1x + t * p2x
        py = (1-t) * p1y + t * p2y
    in (px, py)

-- gets a point along the curve
getCurvePoint : Curve -> Float -> Point
getCurvePoint [p0,p1,p2,p3] t =
    let subp1 = getSubPoint t p0.pos p1.pos
        subp2 = getSubPoint t p1.pos p2.pos
        subp3 = getSubPoint t p2.pos p3.pos
        subsubp1 = getSubPoint t subp1 subp2
        subsubp2 = getSubPoint t subp2 subp3
    in getSubPoint t subsubp1 subsubp2

drawControlPoints : Curve -> [Form]
drawControlPoints = map (\ p -> outlinedFilledCircle |> move p.pos)

drawControlLines : Curve -> Form
drawControlLines ps = map .pos ps |> traced (dotted blue)

drawBezierCurve : Curve -> Form
drawBezierCurve c =
    let tIntervals= map (\ t -> t * 0.01) [1..100]
    in (head c |> .pos) :: map (getCurvePoint c) tIntervals |> traced (solid black)

-- combine all the components of the curve together
drawCurve : [MoveablePoint] -> [Form]
drawCurve ps = drawControlLines ps :: drawBezierCurve ps :: drawControlPoints ps

-- updates whether or not any MoveablePoints are currently selected based on
-- mouse clicks and releases
updatePointStatus : (Bool, (Float, Float)) -> [MoveablePoint] -> [MoveablePoint]
updatePointStatus (pressed, (x,y)) ps =
    case pressed of
        False   -> map (\ p -> {p | selected <- False } ) ps
        True    -> map (\ p -> if ((fst p.pos - 5) < x && (fst p.pos + 5) > x && (snd p.pos - 5) < y && (snd p.pos + 5) > y)
                                    then {p | selected <- True}
                                    else p
                       ) ps

-- custom signals
dragSignal = keepWhen Mouse.isDown (0,0) mousePosition  -- should only update when mouse button is down
mouseBtnToggle = dropRepeats Mouse.isDown               -- should only update when mouse button changes state
mousePosition = lift2 (\(mx,my) (w,h) ->                -- convert mouse coordinates to canvas coordinates
                        let mxf = toFloat mx
                            myf = toFloat my
                            wf  = toFloat w
                            hf  = toFloat h
                        in (mxf - (wf / 2), -1 * (myf - (hf / 2)))) Mouse.position Window.dimensions

updatePointPosition : (Float, Float) -> [MoveablePoint] -> [MoveablePoint]
updatePointPosition (mx, my) ps =
    let updatePointPosition' p =
        case p.selected of
            False -> p
            True  -> {p | pos <- (mx,my)}
    in map updatePointPosition' ps

-- a simple starter curve
curve1 : Curve
curve1 = [ makeMoveablePoint (-200, -150)
         , makeMoveablePoint (-100,    0)
         , makeMoveablePoint ( 100,    0)
         , makeMoveablePoint ( 200, -150)
         ]

-- bring it all together
update (sigA, sigB) = updatePointPosition sigA . updatePointStatus sigB
updatedPoints = foldp update curve1 (lift2 (,) dragSignal (lift2 (,) mouseBtnToggle (sampleOn mouseBtnToggle mousePosition)))

render (w,h) ps = collage w h <| drawCurve ps
main = lift2 render Window.dimensions <| updatedPoints

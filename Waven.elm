import Debug
import Html
import Graphics.Collage as Collage
import Graphics.Element as Element
import Text
import Color
import List
import Signal
import Time

-- CONFIG
framerate = 40
framelength = 1000 / framerate

stringWidth : Int
stringWidth = 450

numParts = 150

partWidth = toFloat (stringWidth // numParts)
partRadius = partWidth / 2


-- UTIL
iterate : (a -> a) -> a -> Int -> List a
iterate f b n =
  case n of
    0 -> []
    1 -> [b]
    n -> b :: iterate f (f b) (n - 1)
      
replaceHead new l =
  case l of
    x::xs -> new::xs
    [] -> []

replaceLast new l =
  case l of
    [] -> []
    [_] -> [new]
    x::xs -> x :: replaceLast new xs

-- ZIPPER
type ContextZipper e =
    LeftEnd (e, e) (List e) -- (focus, rightside), rest
  | MidElem (List e) (e, e, e) (List e) -- many, (leftside, focus, rightside), rest
  | RightEnd (List e) (e, e) -- many, (leftside, focus)

mkZipper : List x -> ContextZipper x
mkZipper l =
  case l of
    x :: y :: zs -> LeftEnd (x, y) zs
    _ -> Debug.crash "Tried to make a zipper from an empty list"

zToList : ContextZipper x -> List x
zToList cz =
  case cz of
    LeftEnd (f, r) rest -> f :: r :: rest
    MidElem prev (l, f, r) rest -> prev ++ (l :: f :: r :: rest)
    RightEnd prev (l, f) -> prev ++ [l, f]

zNext : ContextZipper a -> ContextZipper a
zNext cz =
  case cz of
    LeftEnd (f, r) (x::xs) ->
      MidElem [] (f, r, x) xs

    LeftEnd (f, r) [] ->
      RightEnd [] (f, r)

    MidElem xs (l, f, r) (y::ys) ->
      MidElem (xs ++ [l]) (f, r, y) ys

    MidElem xs (l, f, r) [] ->
      RightEnd (xs ++ [l]) (f, r)

    RightEnd xs (l, f) ->
      Debug.crash "Tried to get zNext from RightEnd"

-- OBJECTS
type alias Part = { y : Float, vy : Float }

type alias Spring = { k : Float, parts: List Part, t : Time.Time }

initialPart : Part
initialPart = { y=0, vy=0 }

initialParts : List Part
initialParts = {y=0, vy=5} :: List.repeat (numParts - 1) initialPart

initialSpring : Spring
initialSpring = { k=1, parts=initialParts, t=0 }

kE : Part -> Float
kE {vy} = 0.5 * vy * vy

totalKE : Spring -> Float
totalKE {parts} = List.sum (List.map kE parts)

totalPE : Spring -> Float
totalPE spr =
  case spr.parts of
    x::y::zs ->
      0.5 * (y.y - x.y) ^ 2 + totalPE { spr | parts = y::zs }
    _ -> 0
    

-- UPDATE

inertiaOne : Part -> Part
inertiaOne ({y, vy} as r) = { r | y = y + vy }

inertia : Spring -> Spring
inertia spr = { spr | parts = List.map inertiaOne spr.parts }

type alias Pulse =
  { start : Float
  , duration : Float
  , hFunction : Float -> Float -- takes time elapsed, returns y
  }

-- Get the height of a pulse at a given time
runP : Pulse -> Float -> Float
runP pulse t =
  if t >= pulse.start && t < pulse.start + pulse.duration
    then pulse.hFunction (t - pulse.start)
    else 0

mergePulses : Pulse -> Pulse -> Pulse
mergePulses p0 p1 =
  let start = min p0.start p1.start
      end = max (p0.start + p0.duration) (p1.start + p1.duration)
      duration = end - start
      p0RelStart = p0.start - start
      p1RelStart = p1.start - start
      hFunction t =
        let fromP0 = runP p0 (start + t)
            fromP1 = runP p1 (start + t)
        in fromP0 + fromP1
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

cosPulse : Float -> Float -> Float -> Float -> Pulse
cosPulse numWavelengths duration amp start =
  let hFunction t = amp * cos (t / duration * numWavelengths * 2 * pi + pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

infinity : Float
infinity = 1 / 0

cosWave : Float -> Float -> Float -> Float -> Pulse
cosWave amp period duration start =
  let hFunction t = amp * cos (t / period * 2 * pi + pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

demo_twoPulsesWideEConserved =
  let cosPulseA = cosPulse 0.5 2000 20 200
      cosPulseB = cosPulse 0.5 2000 20 2500
  in mergePulses cosPulseA cosPulseB

demo_twoPulsesQuickEConserved =
  let cosPulseA = cosPulse 0.5 600 20 200
      cosPulseB = cosPulse 0.5 600 20 2500
  in mergePulses cosPulseA cosPulseB

demo_fewPulsesReflection = cosWave 10 400 1000 200

demo_oneWaveReflection = cosWave 10 400 infinity 200

demo_pulseRight = demo_twoPulsesWideEConserved

demo = demo_twoPulsesQuickEConserved

tension : Spring -> Spring
tension string =
  let leftPart = {y = runP demo string.t, vy = 0}
      rightPart = {y = runP demo string.t, vy = 0}

      leftGetForce : (Part, Part) -> Float
      leftGetForce (f, r) = -f.y

      midGetForce : (Part, Part, Part) -> Float
      midGetForce (l, f, r) = string.k * (l.y - f.y) + string.k * (r.y - f.y)
      
      rightGetForce : (Part, Part) -> Float
      rightGetForce =
        let rightEndClosed (l, f) = -f.y
            rightEndOpen (l, f) = l.y - f.y
        in rightEndClosed
      
      applyForce : Float -> Part -> Part
      applyForce f {y, vy} = {y = y, vy = vy + f}

      handleTensions : List Part -> List Part
      handleTensions parts =
        let forcesZp : List Float -> ContextZipper Part -> List Float
            forcesZp fs pz =
              case pz of
                (LeftEnd fr rest) as cell ->
                  forcesZp
                    (fs ++ [leftGetForce fr])
                    (zNext cell)
                (MidElem left lfr rest) as cell ->
                  forcesZp
                    (fs ++ [midGetForce lfr])
                    (zNext cell)
                RightEnd left lf ->
                  fs ++ [rightGetForce lf]
        in List.map2 applyForce (forcesZp [] (mkZipper parts)) parts

  in { string | parts = handleTensions (replaceLast rightPart (replaceHead leftPart string.parts)) }

stepSpring : Float -> Spring -> Spring
stepSpring dt spr =
  let spr' = inertia (tension spr)
  in { spr' | t = spr.t + dt }

-- VIEW

viewPart : Float -> Part -> Collage.Form
viewPart x {y} = Collage.move (x, y) (Collage.filled Color.black (Collage.circle partRadius))

viewKE : Spring -> Collage.Form
viewKE spring =
  let ke = round (totalKE spring)
      info = "KE = " ++ toString ke
      keShown = Collage.toForm (Element.rightAligned (Text.fromString info))
  in Collage.move (100, 140) keShown

viewPE : Spring -> Collage.Form
viewPE spring =
  let pe = round (totalPE spring)
      info = "PE = " ++ toString pe
      peShown = Collage.toForm (Element.rightAligned (Text.fromString info))
  in Collage.move (100, 120) peShown

viewME : Spring -> Collage.Form
viewME spring =
  let me = round (totalKE spring + totalPE spring)
      info = "ME = " ++ toString me
      meShown = Collage.toForm (Element.rightAligned (Text.fromString info))
  in Collage.move (100, 100) meShown

viewString : Spring -> Collage.Form
viewString spr =
  let rightmost = toFloat (List.length spr.parts) / 2 * partWidth
      renderWithX part acc =
        let x = rightmost - partWidth * toFloat (List.length acc)
        in viewPart x part :: acc
      infos = [viewKE spr, viewPE spr, viewME spr]
  in 
  Collage.group (infos ++ (List.foldr renderWithX [] spr.parts))

placeSpring : Collage.Form -> Element.Element
placeSpring parts =
  let (w, h) = (500, 500)
  in Collage.collage w h
       [ parts
       , Collage.outlined
           (Collage.solid Color.black)
           (Collage.rect w h)
       ]

main : Signal Element.Element
main =
  Signal.map (viewString >> placeSpring)
    (Signal.foldp
      stepSpring
      initialSpring
      (Time.fps framerate))



-- TODO:
--  - modules
--  - make generalized Spring-state type, including
--     Pulses active from left and right, and other
--     customizations
--  - integrate with JS, HTML, incude a UI for customization
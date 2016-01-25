import Debug
import Graphics.Collage as Collage
import Graphics.Element as Element
import Text
import Color
import List
import Signal
import Time

port leftPulserSpecs : Signal (List PulserSpec)
port flat : Signal Bool

leftPulser = Signal.map pulserFromSpecs leftPulserSpecs

--withLeftPulser r leftPulser = { r | leftPulser = leftPulser }
--springSignal spring = Signal.map (withLeftPulser r) leftPulser
--theSpring = springSignal initialSpring

-- CONFIG
framerate = 50
framelength = 1000 / framerate

stringWidth : Int
stringWidth = 450

numParts = 120

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

type alias Spring =
  { k : Float -- spring constant. A particle is 1kg, a pixel is 1m. Used for vertical stretching of spring
  , parts: List Part
  , leftPulser : Pulser
  , rightPulser : Maybe Pulser
  , t : Time.Time
  , rightEndClosed : Bool
  }

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

type alias Pulser =
  { start : Float
  , duration : Float
  , hFunction : Float -> Float -- takes time elapsed, returns y
  }

-- Get the height of a pulser at a given time
runP : Pulser -> Float -> Float
runP pulser t =
  if t >= pulser.start && t < pulser.start + pulser.duration
    then pulser.hFunction (t - pulser.start)
    else 0

type alias PulserSpec =
  { start : Float
  , duration : Float
  , amplitude : Float
  , phaseShift : Float
  , period : Float
  , timeZero : Float
  }

pulserFromSpec : PulserSpec -> Pulser
pulserFromSpec ps =
  { start = ps.start + ps.timeZero + 100
  , duration = ps.duration
  , hFunction =
      \t -> ps.amplitude * cos (2 * pi * t / ps.period + pi / 2 + ps.phaseShift)
  }

zeroPulser : Pulser
zeroPulser =
  { start = 0
  , duration = 0
  , hFunction = \t -> 0
  }

pulserFromSpecs : List PulserSpec -> Pulser
pulserFromSpecs pss =
  case pss of
    [] -> zeroPulser
    [x] -> pulserFromSpec x
    x::xs -> mergePulsers (pulserFromSpec x) (pulserFromSpecs xs)

mergePulsers : Pulser -> Pulser -> Pulser
mergePulsers p0 p1 =
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

cosPulser : Float -> Float -> Float -> Float -> Pulser
cosPulser numWavelengths duration amp start =
  let hFunction t = amp * cos (t / duration * numWavelengths * 2 * pi + pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

cosPulserB : Float -> Float -> Float -> Float -> Pulser
cosPulserB numWavelengths duration amp start =
  let hFunction t = amp * cos (t / duration * numWavelengths * 2 * pi - pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

infinity : Float
infinity = 1 / 0

cosWave : Float -> Float -> Float -> Float -> Pulser
cosWave amp period duration start =
  let hFunction t = amp * cos (t / period * 2 * pi + pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

cosWaveB : Float -> Float -> Float -> Float -> Pulser
cosWaveB amp period duration start =
  let hFunction t = amp * cos (t / period * 2 * pi - pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

demo_onePulserWide = cosPulser 0.5 2000 20 200
demo_onePulserWideB = cosPulser 0.5 2000 -20 200

demo_twoPulsersWideEConserved =
  let a = cosPulser 0.5 2000 20 200
      b = cosPulser 0.5 2000 20 2500
  in mergePulsers a b

demo_twoPulsersQuickEConserved =
  let a = cosPulser 0.5 600 20 200
      b = cosPulser 0.5 600 20 2500
  in mergePulsers a b

demo_fewPulsersReflection = cosWave 10 400 400 200

demo_fewPulsersReflectionB = cosWave -10 400 400 2000

demo_dbl = mergePulsers demo_fewPulsersReflection demo_fewPulsersReflectionB

demo_oneWaveReflection = cosWave 10 400 infinity 200

demo_oneWaveReflectionB = cosWaveB 10 400 infinity 200

demo_pulseRight = demo_twoPulsersWideEConserved

demo = demo_twoPulsersQuickEConserved

initialPart : Part
initialPart = { y=0, vy=0 }

initialParts : List Part
initialParts = {y=0, vy=5} :: List.repeat (numParts - 1) initialPart

initialSpring : Spring
initialSpring =
  { k = 1
  , parts = initialParts
  , leftPulser = demo_dbl
  , rightPulser = Nothing
  , rightEndClosed = True
  , t = 0
  }

tension : Spring -> Spring
tension string =
  let leftPart = {y = runP string.leftPulser string.t, vy = 0}
      rightPart = {y = runP string.leftPulser string.t, vy = 0}

      leftGetForce : (Part, Part) -> Float
      leftGetForce (f, r) = -f.y

      midGetForce : (Part, Part, Part) -> Float
      midGetForce (l, f, r) = string.k * (l.y - f.y) + string.k * (r.y - f.y)
      
      rightGetForce : (Part, Part) -> Float
      rightGetForce =
        let closed (l, f) = -f.y
            open (l, f) = l.y - f.y
        in if string.rightEndClosed then closed else open

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

      newParts =
        case string.rightPulser of
          Just pulser -> replaceLast {y = runP pulser string.t, vy = 0} string.parts
          Nothing -> string.parts
  in { string | parts = handleTensions (replaceHead leftPart newParts) }

stepSpring : (Pulser, Bool, Float) -> Spring -> Spring
stepSpring (lp, flatten, dt) spr =
  if flatten
    then { initialSpring | t = spr.t + dt }
    else
      let spr' = inertia (tension { spr | leftPulser = lp})
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
      (Signal.map3 (,,) leftPulser flat (Time.fps framerate)))



-- TODO:
--  - modules
--  - integrate with JS, HTML, incude a UI for customization
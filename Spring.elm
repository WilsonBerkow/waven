module Spring where

import Time
import List
import Graphics.Element as Element
import Graphics.Collage as Collage
import Text

import Part
import Part exposing (Part)

import Pulser
import Pulser exposing (Pulser)

import Demos

import ContextZipper
import ContextZipper exposing (ContextZipper(LeftEnd, MidElem, RightEnd))

import ListUtil

import Config


-- MODEL

type alias Spring =
  { k : Float
    -- k is the spring constant. A particle is 1kg, a pixel is 1m, a
    -- frame is 1sec. Used for vertical stretching of spring
  , parts: List Part
  , leftPulser : Pulser
  , rightPulser : Maybe Pulser
  , t : Time.Time
  , rightEndClosed : Bool
  }

initialParts : List Part
initialParts = {y=0, vy=5} :: List.repeat (Config.numParts - 1) Part.initialPart

initialSpring : Spring
initialSpring =
  { k = 1
  , parts = initialParts
  , leftPulser = Demos.dbl
  , rightPulser = Nothing
  , rightEndClosed = True
  , t = 0
  }


-- UPDATE

kE : Spring -> Float
kE {parts} = List.sum (List.map Part.kE parts)

pE : Spring -> Float
pE spr =
  case spr.parts of
    x::y::zs ->
      0.5 * (y.y - x.y) ^ 2 + pE { spr | parts = y::zs }
    _ -> 0

inertia : Spring -> Spring
inertia spr = { spr | parts = List.map Part.inertia spr.parts }

tension : Spring -> Spring
tension string =
  let leftPart = {y = Pulser.run string.leftPulser string.t, vy = 0}
      rightPart = {y = Pulser.run string.leftPulser string.t, vy = 0}

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
                    (ContextZipper.next cell)
                (MidElem left lfr rest) as cell ->
                  forcesZp
                    (fs ++ [midGetForce lfr])
                    (ContextZipper.next cell)
                RightEnd left lf ->
                  fs ++ [rightGetForce lf]
        in List.map2 applyForce (forcesZp [] (ContextZipper.mk parts)) parts

      newParts =
        case string.rightPulser of
          Just pulser -> ListUtil.replaceLast {y = Pulser.run pulser string.t, vy = 0} string.parts
          Nothing -> string.parts
  in { string | parts = handleTensions (ListUtil.replaceHead leftPart newParts) }

stepSpring : (Pulser, Maybe Pulser, Bool, Float) -> Spring -> Spring
stepSpring (lp, mrp, flatten, dt) spr =
  if flatten
    then { initialSpring | t = spr.t + dt }
    else
      let spr' = inertia (tension { spr | leftPulser = lp, rightPulser = mrp })
      in { spr' | t = spr.t + dt }



-- VIEW

viewKE : Spring -> Collage.Form
viewKE spring =
  let ke = round (kE spring)
      info = "KE = " ++ toString ke
      keShown = Collage.toForm (Element.rightAligned (Text.fromString info))
  in Collage.move (100, 140) keShown

viewPE : Spring -> Collage.Form
viewPE spring =
  let pe = round (pE spring)
      info = "PE = " ++ toString pe
      peShown = Collage.toForm (Element.rightAligned (Text.fromString info))
  in Collage.move (100, 120) peShown

viewME : Spring -> Collage.Form
viewME spring =
  let me = round (kE spring + pE spring)
      info = "ME = " ++ toString me
      meShown = Collage.toForm (Element.rightAligned (Text.fromString info))
  in Collage.move (100, 100) meShown

viewSpring : Spring -> Collage.Form
viewSpring spr =
  let rightmost = toFloat (List.length spr.parts) / 2 * Config.partWidth
      renderWithX part acc =
        let x = rightmost - Config.partWidth * toFloat (List.length acc)
        in Part.viewPart x part :: acc
      infos = [viewKE spr, viewPE spr, viewME spr]
  in
  Collage.group (infos ++ (List.foldr renderWithX [] spr.parts))
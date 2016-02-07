module Part where

import Graphics.Collage as Collage
import Color

import Config


-- MODEL

type alias Part = { y : Float, vy : Float }


-- UPDATE

kE : Part -> Float
kE {vy} = 0.5 * vy * vy

inertia : Part -> Part
inertia ({y, vy} as r) = { r | y = y + vy }

initialPart : Part
initialPart = { y=0, vy=0 }


-- VIEW

viewPart : Float -> Part -> Collage.Form
viewPart x {y} =
  Collage.move
    (x, y)
    (Collage.filled Color.black (Collage.circle Config.partRadius))
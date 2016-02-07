import Graphics.Collage as Collage
import Graphics.Element as Element
import Color
import Signal
import Time

import ListUtil

import Spring
import Spring exposing (Spring)

import Pulser
import Pulser exposing (Pulser)

import PulserSpec
import PulserSpec exposing (PulserSpec)

import Config


-- Handle ports

port leftPulserSpecs : Signal (List PulserSpec)
port rightPulserSpecs : Signal (Maybe (List PulserSpec))
port flat : Signal Bool

leftPulser : Signal Pulser
leftPulser = Signal.map PulserSpec.pulserFromSpecs leftPulserSpecs

rightPulser : Signal (Maybe Pulser)
rightPulser = Signal.map (Maybe.map PulserSpec.pulserFromSpecs) rightPulserSpecs


-- Put together with views

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
  Signal.map (Spring.viewSpring >> placeSpring)
    (Signal.foldp
      Spring.stepSpring
      Spring.initialSpring
      (Signal.map4 (,,,)
        leftPulser
        rightPulser
        flat
        (Time.fps Config.framerate)))

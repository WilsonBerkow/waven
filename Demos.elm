module Demos where

import Pulser
import Pulser exposing (Pulser)

infinity : Float
infinity = 1 / 0

onePulserWide = Pulser.cosPulser 0.5 2000 20 200
onePulserWideB = Pulser.cosPulser 0.5 2000 -20 200

twoPulsersWideEConserved =
  let a = Pulser.cosPulser 0.5 2000 20 200
      b = Pulser.cosPulser 0.5 2000 20 2500
  in Pulser.mergePulsers a b

twoPulsersQuickEConserved =
  let a = Pulser.cosPulser 0.5 600 20 200
      b = Pulser.cosPulser 0.5 600 20 2500
  in Pulser.mergePulsers a b

fewPulsersReflection = Pulser.cosWave 10 400 400 200

fewPulsersReflectionB = Pulser.cosWave -10 400 400 2000

dbl = Pulser.mergePulsers fewPulsersReflection fewPulsersReflectionB

oneWaveReflection = Pulser.cosWave 10 400 infinity 200

oneWaveReflectionB = Pulser.cosWaveShifted 10 400 infinity 200

pulseRight = twoPulsersWideEConserved
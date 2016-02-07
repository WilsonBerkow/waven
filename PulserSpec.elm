module PulserSpec where

import Pulser
import Pulser exposing (Pulser)

import Config

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
      \t -> ps.amplitude * cos (2 * pi * t / ps.period - pi / 2 + ps.phaseShift)
  }

pulserFromSpecs : List PulserSpec -> Pulser
pulserFromSpecs pss =
  case pss of
    [] -> Pulser.zeroPulser
    [x] -> pulserFromSpec x
    x::xs -> Pulser.mergePulsers (pulserFromSpec x) (pulserFromSpecs xs)
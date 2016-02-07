module Pulser where

type alias Pulser =
  { start : Float
  , duration : Float
  , hFunction : Float -> Float -- takes time elapsed, returns y
  }

-- Get the height of a pulser at a given time
run : Pulser -> Float -> Float
run pulser t =
  if t >= pulser.start && t < pulser.start + pulser.duration
    then pulser.hFunction (t - pulser.start)
    else 0

mergePulsers : Pulser -> Pulser -> Pulser
mergePulsers p0 p1 =
  let start = min p0.start p1.start
      end = max (p0.start + p0.duration) (p1.start + p1.duration)
      duration = end - start
      p0RelStart = p0.start - start
      p1RelStart = p1.start - start
      hFunction t =
        let fromP0 = run p0 (start + t)
            fromP1 = run p1 (start + t)
        in fromP0 + fromP1
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

zeroPulser : Pulser
zeroPulser =
  { start = 0
  , duration = 0
  , hFunction = \t -> 0
  }

cosPulser : Float -> Float -> Float -> Float -> Pulser
cosPulser numWavelengths duration amp start =
  let hFunction t = amp * cos (t / duration * numWavelengths * 2 * pi + pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

cosPulserShifted : Float -> Float -> Float -> Float -> Pulser
cosPulserShifted numWavelengths duration amp start =
  let hFunction t = amp * cos (t / duration * numWavelengths * 2 * pi - pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

cosWave : Float -> Float -> Float -> Float -> Pulser
cosWave amp period duration start =
  let hFunction t = amp * cos (t / period * 2 * pi + pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }

cosWaveShifted : Float -> Float -> Float -> Float -> Pulser
cosWaveShifted amp period duration start =
  let hFunction t = amp * cos (t / period * 2 * pi - pi / 2)
  in
    { start = start
    , duration = duration
    , hFunction = hFunction
    }


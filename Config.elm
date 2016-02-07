module Config where

framerate = 50

framelength = 1000 / framerate

springWidth : Int
springWidth = 450

numParts = 120

partWidth = toFloat (springWidth // numParts)

partRadius = partWidth / 2

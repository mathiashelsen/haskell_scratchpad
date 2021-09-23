module Geometry.Cuboid
(volume
, area
) where

volume a b c = a*b*c

area a b c = a*b*2 + a*c*2 + b*c*2

module Geometry.Cube
(volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume a = Cuboid.volume a a a 

area a = Cuboid.area a a a

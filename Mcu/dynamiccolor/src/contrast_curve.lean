import Mcu.utils.math_utils

structure contrast_curve where
  low : Float
  normal : Float
  medium : Float
  high : Float

namespace contrast_curve

def get (cc : contrast_curve) (contrastLevel : Float) : Float :=
  if contrastLevel <= -1.0 then
    cc.low
  else if contrastLevel < 0.0 then
    utils.math_utils.lerp cc.low cc.normal (contrastLevel + 1.0)
  else if contrastLevel < 0.5 then
    utils.math_utils.lerp cc.normal cc.medium (contrastLevel * 2.0)
  else if contrastLevel < 1.0 then
    utils.math_utils.lerp cc.medium cc.high ((contrastLevel - 0.5) * 2.0)
  else
    cc.high

end contrast_curve

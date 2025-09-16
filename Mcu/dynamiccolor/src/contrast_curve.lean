import Mcu.utils.math_utils

structure ContrastCurve where
  low : Float
  normal : Float
  medium : Float
  high : Float

namespace ContrastCurve

def get (cc : ContrastCurve) (contrastLevel : Float) : Float :=
  if contrastLevel <= -1.0 then
    cc.low
  else if contrastLevel < 0.0 then
    MathUtils.lerp cc.low cc.normal (contrastLevel + 1.0)
  else if contrastLevel < 0.5 then
    MathUtils.lerp cc.normal cc.medium (contrastLevel * 2.0)
  else if contrastLevel < 1.0 then
    MathUtils.lerp cc.medium cc.high ((contrastLevel - 0.5) * 2.0)
  else
    cc.high

end ContrastCurve

import Mcu.utils.color_utils
import Mcu.utils.math_utils

/-- 
  the white_point, drgbInverse, rgbD are Vector Float 3 now
  but in implementation of dart, they are List<double>
  I don't know if it is true now, but if there is some problem,
  I will change them to List<double> at that time
-/
structure ViewingConditions where
  white_point : Vector Float 3
  adaptingLuminance : Float
  backgroundLstar : Float
  surround : Float
  discountingIlluminant : Bool

  backgroundYTowhitePointY : Float
  aw : Float
  nbb : Float
  ncb : Float
  c : Float
  nC : Float
  drgbInverse : Vector Float 3
  rgbD : Vector Float 3
  fl : Float
  fLRoot : Float
  z : Float

namespace ViewingConditions

def make (whitePoint : Vector Float 3 := ColorUtils.whitePointD65 ())
  (adaptingLuminance : Float := -1.0)
  (backgroundLstar : Float := 50.0)
  (surround : Float := 2.0)
  (discountingIlluminant : Bool := false) : ViewingConditions :=
  let adaptingLuminance := if adaptingLuminance > 0.0 then adaptingLuminance
    else 200.0 / MathUtils.pi * (ColorUtils.yFromLstar 50) / 100
  let backgroundLstar := max 0.1 backgroundLstar
  let xyz := whitePoint
  let rW := xyz[0] * 0.401288 + xyz[1] * 0.650173 + xyz[2] * -0.051461
  let gW := xyz[0] * -0.250268 + xyz[1] * 1.204414 + xyz[2] * 0.045854
  let bW := xyz[0] * -0.002079 + xyz[1] * 0.048952 + xyz[2] * 0.953127
  let f := 0.8 + surround / 10.0
  let c := if f >= 0.9 then MathUtils.lerp 0.59 0.69 (f - 0.9) * 10.0
    else MathUtils.lerp 0.525 0.59 (f - 0.8) * 10.0
  let d := if discountingIlluminant then 1.0
    else f * (1.0 - ((1.0 / 3.6) * Float.exp ((-adaptingLuminance - 42) / 92.0)))
  let d:= if d > 1.0 then 1.0 else if d < 0.0 then 0.0 else d
  let nc := f
  let rgbD := #v[ d * (100.0 / rW) + 1.0 - d,
    d * (100.0 / gW) + 1.0 - d,
    d * (100.0 / bW) + 1.0 - d ]
  let k := 1.0 / (5.0 * adaptingLuminance + 1.0)
  let k4 := k ^ 4
  let k4F := 1.0 - k4
  let fl :=
    (k4 * adaptingLuminance) +
    (0.1 * (k4F ^ 2) * ((5.0 * adaptingLuminance) ^ (1.0 / 3.0)))
  let n := ColorUtils.yFromLstar backgroundLstar / whitePoint[1]
  let z := 1.48 + Float.sqrt n
  let nbb := 0.725 / (n ^ 0.2)
  let ncb := nbb
  let rgbAFactors := #v[(fl * rgbD[0] * rW / 100.0) ^ 0.42,
    (fl * rgbD[1] * gW / 100.0) ^ 0.42,
    (fl * rgbD[2] * bW / 100.0) ^ 0.42]
  let rgbA := #v[(400.0 * rgbAFactors[0]) / (rgbAFactors[0] + 27.13),
    (400.0 * rgbAFactors[1]) / (rgbAFactors[1] + 27.13),
    (400.0 * rgbAFactors[2]) / (rgbAFactors[2] + 27.13)]
  let aw := (40.0 * rgbA[0] + 20.0 * rgbA[1] + rgbA[2]) / 20.0 * nbb
  {white_point := whitePoint,
   adaptingLuminance := adaptingLuminance,
   backgroundLstar := backgroundLstar,
   surround := surround,
   discountingIlluminant := discountingIlluminant,
   backgroundYTowhitePointY := n,
   aw := aw,
   nbb := nbb,
   ncb := ncb,
   c := c,
   nC := nc,
   drgbInverse := #v[0.0, 0.0, 0.0],
   rgbD := rgbD,
   fl := fl,
   fLRoot := fl ^ 0.25,
   z := z }

def sRgb : ViewingConditions := make
def standard : ViewingConditions := sRgb

end ViewingConditions

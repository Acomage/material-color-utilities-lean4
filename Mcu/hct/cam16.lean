import Mcu.utils.color_utils
import Mcu.utils.math_utils
import Mcu.hct.viewing_conditions

structure cam16 where
  hue : Float
  chroma : Float
  j : Float
  q : Float
  m : Float
  s : Float
  jstar : Float
  astar : Float
  bstar : Float

namespace cam16

def distance (self other : cam16) : Float :=
  let dJ := self.jstar - other.jstar
  let dA := self.astar - other.astar
  let dB := self.bstar - other.bstar
  let dEPrime := (dJ^2 + dA^2 + dB^2).sqrt
  1.41 * (dEPrime ^ 0.63)

def fromXyzInViewingConditions (x y z : Float) (viewingconditions : viewing_conditions) : cam16 :=
  let rC := 0.401288 * x + 0.650173 * y - 0.051461 * z
  let gC := -0.250268 * x + 1.204414 * y + 0.045854 * z
  let bC := -0.002079 * x + 0.048952 * y + 0.953127 * z

  let rD := viewingconditions.rgbD[0] * rC
  let gD := viewingconditions.rgbD[1] * gC
  let bD := viewingconditions.rgbD[2] * bC

  let rAF : Float := (viewingconditions.fl * rD.abs / 100.0) ^ 0.42
  let gAF : Float := (viewingconditions.fl * gD.abs / 100.0) ^ 0.42
  let bAF : Float := (viewingconditions.fl * bD.abs / 100.0) ^ 0.42

  let rA := Float.ofInt (utils.math_utils.signum rD) * 400.0 * rAF / (rAF + 27.13)
  let gA := Float.ofInt (utils.math_utils.signum gD) * 400.0 * gAF / (gAF + 27.13)
  let bA := Float.ofInt (utils.math_utils.signum bD) * 400.0 * bAF / (bAF + 27.13)

  let a := (11.0 * rA - 12.0 * gA + bA) / 11.0

  let b := (rA + gA - 2.0 * bA) / 9.0

  let u := (20.0 * rA + 20.0 * gA + 21.0 * bA) / 20.0
  let p2 := (40.0 * rA + 20.0 * gA + bA) / 20.0

  let atan2 := Float.atan2 b a
  let atanDegrees := atan2 * 180.0 / utils.math_utils.pi
  let hue := if atanDegrees < 0.0
    then atanDegrees + 360.0
    else if atanDegrees >= 360.0
      then atanDegrees - 360.0
      else atanDegrees
  let hueRadians := hue * utils.math_utils.pi / 180.0

  let ac := p2 * viewingconditions.nbb

  let J := 100.0 * (ac / viewingconditions.aw) ^ (viewingconditions.c * viewingconditions.z)
  let Q := (4.0 / viewingconditions.c) * (J / 100.0).sqrt * (viewingconditions.aw + 4.0) * viewingconditions.fLRoot

  let huePrime := if hue < 20.14 then hue + 360.0 else hue
  let eHue := 0.25 * (Float.cos (huePrime * utils.math_utils.pi / 180.0 + 2.0) + 3.8)
  let p1 := (50000.0 / 13.0) * eHue * viewingconditions.nC * viewingconditions.ncb
  let t := p1 * (a^2 + b^2).sqrt / (u + 0.305)
  let alpha := t^0.9 * (1.64 - 0.29^viewingconditions.backgroundYTowhitePointY)^0.73
  let C := alpha * (J / 100.0).sqrt
  let M := C * viewingconditions.fLRoot
  let s := 50.0 * ((alpha * viewingconditions.c)/(viewingconditions.aw + 4.0)).sqrt

  let jstar := (1.0 + 100.0 * 0.007) * J / (1.0 + 0.007 * J)
  let mstar := Float.log (1.0 + 0.0228 * M) / 0.0228
  let astar := mstar * Float.cos (hueRadians)
  let bstar := mstar * Float.sin (hueRadians)

  { hue := hue,
    chroma := C,
    j := J,
    q := Q,
    m := M,
    s := s,
    jstar := jstar,
    astar := astar,
    bstar := bstar }

def fromIntInViewingConditions (argb : Int) (viewingconditions : viewing_conditions) : cam16 :=
  let xyz := utils.color_utils.xyzFromArgb argb
  let x := xyz[0]
  let y := xyz[1]
  let z := xyz[2]
  fromXyzInViewingConditions x y z viewingconditions

def fromInt (argb : Int) : cam16 :=
  fromIntInViewingConditions argb (viewing_conditions.sRgb)

def fromJchInViewingConditions (J C h : Float) (viewingconditions : viewing_conditions) : cam16 :=
  let Q := (4.0 / viewingconditions.c) * (J / 100.0).sqrt * (viewingconditions.aw + 4.0) * viewingconditions.fLRoot
  let M := C * viewingconditions.fLRoot
  let alpha := C / (J / 100.0).sqrt
  let s := 50.0 * ((alpha * viewingconditions.c)/(viewingconditions.aw + 4.0)).sqrt

  let hueRadians := h * utils.math_utils.pi / 180.0
  let jstar := (1.0 + 100.0 * 0.007) * J / (1.0 + 0.007 * J)
  let mstar := (1.0 / 0.0228) * Float.log (1.0 + 0.0228 * M)
  let astar := mstar * Float.cos (hueRadians)
  let bstar := mstar * Float.sin (hueRadians)

  { hue := h,
    chroma := C,
    j := J,
    q := Q,
    m := M,
    s := s,
    jstar := jstar,
    astar := astar,
    bstar := bstar }

def fromJch (j c h : Float) : cam16 :=
  fromJchInViewingConditions j c h (viewing_conditions.sRgb)

def fromUcsInViewingConditions (jstar astar bstar : Float) (viewingconditions : viewing_conditions) : cam16 :=
  let a := astar
  let b := bstar
  let m := (a^2 + b^2).sqrt
  let M := (Float.exp (m * 0.0228) - 1.0) / 0.0228
  let c := M / viewingconditions.fLRoot
  let h := Float.atan2 b a * (180.0 / utils.math_utils.pi)
  let h := if h < 0.0 then h + 360.0 else h
  let j := jstar / (1.0 - (jstar - 100.0) * 0.007)
  fromJchInViewingConditions j c h viewingconditions

def fromUcs (jstar astar bstar : Float) : cam16 :=
  fromUcsInViewingConditions jstar astar bstar (viewing_conditions.standard)

def xyzInViewingConditions (self : cam16) (viewingconditions : viewing_conditions) : Vector Float 3 :=
  let alpha := if self.chroma == 0.0 || self.j == 0.0 then 0.0
    else self.chroma / (self.j / 100.0).sqrt

  let t := (alpha / ((1.64 - 0.29^viewingconditions.backgroundYTowhitePointY)^0.73))^(1.0 / 0.9)
  let hRad := self.hue * utils.math_utils.pi / 180.0

  let eHue := 0.25 * (Float.cos (self.hue + 2.0) + 3.8)
  let ac := viewingconditions.aw * (self.j / 100.0)^(1.0 / viewingconditions.c / viewingconditions.z)
  let p1 := eHue * (50000.0 / 13.0) * viewingconditions.nC * viewingconditions.ncb

  let p2 := ac / viewingconditions.nbb

  let hSin := Float.sin hRad
  let hCos := Float.cos hRad

  let gamma := (23.0 * (p2 + 0.305) * t) / (23.0 * p1 + 11.0 * hCos + 108.0 * hSin)
  let a := gamma * hCos
  let b := gamma * hSin
  let rA := (460.0 * p2 + 451.0 * a + 288.0 * b) / 1403.0
  let gA := (460.0 * p2 - 891.0 * a - 261.0 * b) / 1403.0
  let bA := (460.0 * p2 - 220.0 * a - 6300.0 * b) / 1403.0

  let rCBase := utils.math_utils.max 0.0 (27.13 * rA.abs / (400.0 - rA.abs))
  let rC := Float.ofInt (utils.math_utils.signum rA) * (100.0 / viewingconditions.fl) * (rCBase^(1.0 / 0.42))
  let gCBase := utils.math_utils.max 0.0 (27.13 * gA.abs / (400.0 - gA.abs))
  let gC := Float.ofInt (utils.math_utils.signum gA) * (100.0 / viewingconditions.fl) * (gCBase^(1.0 / 0.42))
  let bCBase := utils.math_utils.max 0.0 (27.13 * bA.abs / (400.0 - bA.abs))
  let bC := Float.ofInt (utils.math_utils.signum bA) * (100.0 / viewingconditions.fl) * (bCBase^(1.0 / 0.42))
  let rF := rC / viewingconditions.rgbD[0]
  let gF := gC / viewingconditions.rgbD[1]
  let bF := bC / viewingconditions.rgbD[2]

  let x := 1.86206786 * rF - 1.01125463 * gF + 0.14918677 * bF
  let y := 0.38752654 * rF + 0.62144744 * gF - 0.00897398 * bF
  let z := -0.01584150 * rF - 0.03412294 * gF + 1.04996444 * bF

  #v[x, y, z]

def viewed (self : cam16) (viewingconditions : viewing_conditions) : Int :=
  let xyz := xyzInViewingConditions self viewingconditions
  utils.color_utils.argbFromXyz xyz[0] xyz[1] xyz[2]

def toInt (self : cam16): Unit -> Int :=
  fun () => viewed self viewing_conditions.sRgb

end cam16

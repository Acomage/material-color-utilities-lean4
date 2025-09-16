import Mcu.hct.cam16
import Mcu.hct.hct
import Mcu.utils.color_utils
import Mcu.utils.math_utils

namespace Blend

def harmonize (designColor sourceColor : Int) : Int :=
  let fromHct := Hct.fromInt designColor
  let toHct := Hct.fromInt sourceColor
  let differenceDegrees := MathUtils.differenceDegrees fromHct.hue toHct.hue
  let rotationDegrees := MathUtils.min (differenceDegrees * 0.5) 15.0
  let outputHue := MathUtils.sanitizeDegreesDouble (fromHct.hue + rotationDegrees * Float.ofInt (MathUtils.rotationDirection fromHct.hue toHct.hue))
  (Hct.from_ outputHue fromHct.chroma fromHct.tone).toInt

def Cam16Ucs (from_ to : Int) (amount : Float) : Int :=
  let fromCam := Cam16.fromInt from_
  let toCam := Cam16.fromInt to
  let fromJ := fromCam.jstar
  let fromA := fromCam.astar
  let fromB := fromCam.bstar
  let toJ := toCam.jstar
  let toA := toCam.astar
  let toB := toCam.bstar
  let jstar := fromJ + (toJ - fromJ) * amount
  let astar := fromA + (toA - fromA) * amount
  let bstar := fromB + (toB - fromB) * amount
  (Cam16.fromUcs jstar astar bstar).toInt ()

def HctHue (from_ to : Int) (amount : Float) : Int :=
  let ucs := Cam16Ucs from_ to amount
  let ucsCam := Cam16.fromInt ucs
  let fromCam := Cam16.fromInt from_
  let blended := Hct.from_ ucsCam.hue fromCam.chroma (ColorUtils.lstarFromArgb from_)
  blended.toInt

end Blend

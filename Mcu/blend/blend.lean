import Mcu.hct.cam16
import Mcu.hct.hct
import Mcu.utils.color_utils
import Mcu.utils.math_utils

namespace blend

def harmonize (designColor sourceColor : Int) : Int :=
  let fromHct := hct.fromInt designColor
  let toHct := hct.fromInt sourceColor
  let differenceDegrees := utils.math_utils.differenceDegrees fromHct.hue toHct.hue
  let rotationDegrees := utils.math_utils.min (differenceDegrees * 0.5) 15.0
  let outputHue := utils.math_utils.sanitizeDegreesDouble (fromHct.hue + rotationDegrees * Float.ofInt (utils.math_utils.rotationDirection fromHct.hue toHct.hue))
  (hct.from_ outputHue fromHct.chroma fromHct.tone).toInt

def cam16Ucs (from_ to : Int) (amount : Float) : Int :=
  let fromCam := cam16.fromInt from_
  let toCam := cam16.fromInt to
  let fromJ := fromCam.jstar
  let fromA := fromCam.astar
  let fromB := fromCam.bstar
  let toJ := toCam.jstar
  let toA := toCam.astar
  let toB := toCam.bstar
  let jstar := fromJ + (toJ - fromJ) * amount
  let astar := fromA + (toA - fromA) * amount
  let bstar := fromB + (toB - fromB) * amount
  (cam16.fromUcs jstar astar bstar).toInt ()

def hctHue (from_ to : Int) (amount : Float) : Int :=
  let ucs := cam16Ucs from_ to amount
  let ucsCam := cam16.fromInt ucs
  let fromCam := cam16.fromInt from_
  let blended := hct.from_ ucsCam.hue fromCam.chroma (utils.color_utils.lstarFromArgb from_)
  blended.toInt

end blend

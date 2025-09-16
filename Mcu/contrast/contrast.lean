import Mcu.utils.color_utils
import Mcu.utils.math_utils

namespace Contrast

def _ratioOfYs (y1 y2 : Float) : Float :=
  let lighter := MathUtils.max y1 y2
  let darker := MathUtils.min y1 y2
  (lighter + 5.0) / (darker + 5.0)

def ratioOfTones (toneA toneB : Float) : Float :=
  let toneA := MathUtils.clampDouble 0.0 100.0 toneA
  let toneB := MathUtils.clampDouble 0.0 100.0 toneB
  _ratioOfYs (ColorUtils.yFromLstar toneA) (ColorUtils.yFromLstar toneB)

def lighter (tone ratio : Float) : Float :=
  if tone < 0.0 || tone > 100.0 then
    -1.0
  else
    let darkY := ColorUtils.yFromLstar tone
    let lightY := ratio * (darkY + 5.0) - 5.0
    let realContrast := _ratioOfYs lightY darkY
    let delta := (realContrast - ratio).abs
    if realContrast < ratio && delta > 0.04 then
      -1.0
    else
      let returnValue := ColorUtils.lstarFromY lightY + 0.4
      if returnValue < 0 || returnValue > 100 then
        -1.0
      else returnValue

def darker (tone ratio : Float) : Float :=
  if tone < 0.0 || tone > 100.0 then
    -1.0
  else
    let lightY := ColorUtils.yFromLstar tone
    let darkY := ((lightY + 5.0) / ratio) - 5.0
    let realContrast := _ratioOfYs lightY darkY
    let delta := (realContrast - ratio).abs
    if realContrast < ratio && delta > 0.04 then
      -1.0
    else
      let returnValue := ColorUtils.lstarFromY darkY - 0.4
      if returnValue < 0 || returnValue > 100 then
        -1.0
      else returnValue

def lighterUnsafe (tone ratio : Float) : Float :=
  let lighterSafe := lighter tone ratio
  if lighterSafe < 0.0 then 100.0 else lighterSafe

def darkerUnsafe (tone ratio : Float) : Float :=
  let darkerSafe := darker tone ratio
  if darkerSafe < 0.0 then 0.0 else darkerSafe

end Contrast

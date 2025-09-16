import Mcu.utils.color_utils
import Mcu.utils.math_utils

namespace contrast

def _ratioOfYs (y1 y2 : Float) : Float :=
  let lighter := utils.math_utils.max y1 y2
  let darker := utils.math_utils.min y1 y2
  (lighter + 5.0) / (darker + 5.0)

def ratioOfTones (toneA toneB : Float) : Float :=
  let toneA := utils.math_utils.clampDouble 0.0 100.0 toneA
  let toneB := utils.math_utils.clampDouble 0.0 100.0 toneB
  _ratioOfYs (utils.color_utils.yFromLstar toneA) (utils.color_utils.yFromLstar toneB)

def lighter (tone ratio : Float) : Float :=
  if tone < 0.0 || tone > 100.0 then
    -1.0
  else
    let darkY := utils.color_utils.yFromLstar tone
    let lightY := ratio * (darkY + 5.0) - 5.0
    let realContrast := _ratioOfYs lightY darkY
    let delta := (realContrast - ratio).abs
    if realContrast < ratio && delta > 0.04 then
      -1.0
    else
      let returnValue := utils.color_utils.lstarFromY lightY + 0.4
      if returnValue < 0 || returnValue > 100 then
        -1.0
      else returnValue

def darker (tone ratio : Float) : Float :=
  if tone < 0.0 || tone > 100.0 then
    -1.0
  else
    let lightY := utils.color_utils.yFromLstar tone
    let darkY := ((lightY + 5.0) / ratio) - 5.0
    let realContrast := _ratioOfYs lightY darkY
    let delta := (realContrast - ratio).abs
    if realContrast < ratio && delta > 0.04 then
      -1.0
    else
      let returnValue := utils.color_utils.lstarFromY darkY - 0.4
      if returnValue < 0 || returnValue > 100 then
        -1.0
      else returnValue

def lighterUnsafe (tone ratio : Float) : Float :=
  let lighterSafe := lighter tone ratio
  if lighterSafe < 0.0 then 100.0 else lighterSafe

def darkerUnsafe (tone ratio : Float) : Float :=
  let darkerSafe := darker tone ratio
  if darkerSafe < 0.0 then 0.0 else darkerSafe

end contrast

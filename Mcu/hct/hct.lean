import Mcu.utils.color_utils
import Mcu.hct.cam16
import Mcu.hct.src.hct_solver
import Mcu.hct.viewing_conditions

structure hct where
  _hue : Float
  _chroma : Float
  _tone : Float
  _argb : Int

namespace hct

def from_argb (argb : Int) : hct :=
  let _argb := argb
  let cam := cam16.fromInt argb
  let _hue := cam.hue
  let _chroma := cam.chroma
  let _tone := utils.color_utils.lstarFromArgb argb
  { _hue := _hue, _chroma := _chroma, _tone := _tone, _argb := _argb }

def from_ (hue chroma tone : Float) : hct :=
  let argb := hct_solver.solveToInt hue chroma tone
  from_argb argb

instance : BEq hct where
  beq a b := a._argb == b._argb

instance : Hashable hct where
  hash h := Hashable.hash h._argb

instance : ToString hct where
  toString h := s!"H{h._hue.round} C{h._chroma.round} T{h._tone.round}"

def fromInt (argb : Int) : hct :=
  from_argb argb

def toInt (h : hct) : Int :=
  h._argb

def hue (h : hct) : Float :=
  h._hue

def chroma (h : hct) : Float :=
  h._chroma

def tone (h : hct) : Float :=
  h._tone

def setHue (h : hct) (newHue : Float) : hct :=
  let _argb := hct_solver.solveToInt newHue h._chroma h._tone
  let cam := cam16.fromInt _argb
  let _hue := cam.hue
  let _chroma := cam.chroma
  let _tone := utils.color_utils.lstarFromArgb _argb
  {h with _hue := _hue, _chroma := _chroma, _tone := _tone, _argb := _argb}

def setChroma (h : hct) (newChroma : Float) : hct :=
  let _argb := hct_solver.solveToInt h._hue newChroma h._tone
  let cam := cam16.fromInt _argb
  let _hue := cam.hue
  let _chroma := cam.chroma
  let _tone := utils.color_utils.lstarFromArgb _argb
  {h with _hue := _hue, _chroma := _chroma, _tone := _tone, _argb := _argb}

def setTone (h : hct) (newTone : Float) : hct :=
  let _argb := hct_solver.solveToInt h._hue h._chroma newTone
  let cam := cam16.fromInt _argb
  let _hue := cam.hue
  let _chroma := cam.chroma
  let _tone := utils.color_utils.lstarFromArgb _argb
  {h with _hue := _hue, _chroma := _chroma, _tone := _tone, _argb := _argb}

def inViewingConditions (h : hct) (vc : viewing_conditions) : hct :=
  let cam := cam16.fromInt (toInt h)
  let viewedInVc := cam.xyzInViewingConditions vc

  let recastInVc := cam16.fromXyzInViewingConditions viewedInVc[0] viewedInVc[1] viewedInVc[2] viewing_conditions.make

  let recastHct := from_ recastInVc.hue recastInVc.chroma (utils.color_utils.lstarFromY (viewedInVc[1]))
  recastHct

end hct

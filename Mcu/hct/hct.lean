import Mcu.utils.color_utils
import Mcu.hct.cam16
import Mcu.hct.src.hct_solver
import Mcu.hct.viewing_conditions

structure Hct where
  _hue : Float
  _chroma : Float
  _tone : Float
  _argb : Int

namespace Hct

def from_argb (argb : Int) : Hct :=
  let _argb := argb
  let cam := Cam16.fromInt argb
  let _hue := cam.hue
  let _chroma := cam.chroma
  let _tone := ColorUtils.lstarFromArgb argb
  { _hue := _hue, _chroma := _chroma, _tone := _tone, _argb := _argb }

def from_ (hue chroma tone : Float) : Hct :=
  let argb := HctSolver.solveToInt hue chroma tone
  from_argb argb

instance : BEq Hct where
  beq a b := a._argb == b._argb

instance : Hashable Hct where
  hash h := Hashable.hash h._argb

instance : ToString Hct where
  toString h := s!"H{h._hue.round} C{h._chroma.round} T{h._tone.round}"

def fromInt (argb : Int) : Hct :=
  from_argb argb

def toInt (h : Hct) : Int :=
  h._argb

def hue (h : Hct) : Float :=
  h._hue

def chroma (h : Hct) : Float :=
  h._chroma

def tone (h : Hct) : Float :=
  h._tone

def setHue (h : Hct) (newHue : Float) : Hct :=
  let _argb := HctSolver.solveToInt newHue h._chroma h._tone
  let cam := Cam16.fromInt _argb
  let _hue := cam.hue
  let _chroma := cam.chroma
  let _tone := ColorUtils.lstarFromArgb _argb
  {h with _hue := _hue, _chroma := _chroma, _tone := _tone, _argb := _argb}

def setChroma (h : Hct) (newChroma : Float) : Hct :=
  let _argb := HctSolver.solveToInt h._hue newChroma h._tone
  let cam := Cam16.fromInt _argb
  let _hue := cam.hue
  let _chroma := cam.chroma
  let _tone := ColorUtils.lstarFromArgb _argb
  {h with _hue := _hue, _chroma := _chroma, _tone := _tone, _argb := _argb}

def setTone (h : Hct) (newTone : Float) : Hct :=
  let _argb := HctSolver.solveToInt h._hue h._chroma newTone
  let cam := Cam16.fromInt _argb
  let _hue := cam.hue
  let _chroma := cam.chroma
  let _tone := ColorUtils.lstarFromArgb _argb
  {h with _hue := _hue, _chroma := _chroma, _tone := _tone, _argb := _argb}

def inViewingConditions (h : Hct) (vc : ViewingConditions) : Hct :=
  let cam := Cam16.fromInt (toInt h)
  let viewedInVc := cam.xyzInViewingConditions vc

  let recastInVc := Cam16.fromXyzInViewingConditions viewedInVc[0] viewedInVc[1] viewedInVc[2] ViewingConditions.make

  let recastHct := from_ recastInVc.hue recastInVc.chroma (ColorUtils.lstarFromY (viewedInVc[1]))
  recastHct

end Hct

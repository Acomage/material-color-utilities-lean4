import Mcu.utils.math_utils

namespace utils.color_utils

def _srgbToXyz : Vector (Vector Float 3) 3 :=
  #v[#v[0.41233895, 0.35762064, 0.18051042],
    #v[0.2126, 0.7152, 0.0722],
    #v[0.01932141, 0.11916382, 0.95034478]]

def _xyzToSrgb : Vector (Vector Float 3) 3 :=
  #v[#v[3.2413774792388685, -1.5376652402851851, -0.49885366846268053],
    #v[-0.9691452513005321, 1.8758853451067872, 0.04156585616912061],
    #v[0.05562093689691305, -0.20395524564742123, 1.0571799111220335]]

def _whitePointD65 : Vector Float 3 := #v[95.047, 100.0, 108.883]

def argbFromRgb (red green blue : Int) : Int :=
    (255 <<< 24 ||| ((red &&& 255#8) <<< 16) ||| ((green &&& 255#8) <<< 8) ||| (blue &&& 255)).toInt

def linearized (rgbComponent : Int) : Float :=
  let normalized := Float.ofInt rgbComponent / 255
  if normalized <= 0.040449936 then
    normalized / 12.92 * 100.0
  else
    ((normalized + 0.055) / 1.055) ^ 2.4 * 100.0

def delinearized (rgbComponent : Float) : Int :=
  let normalized := rgbComponent / 100.0
  let delinearizedValue :=
    if normalized <= 0.0031308 then
      normalized * 12.92
    else
      1.055 * (normalized ^ (1 / 2.4)) - 0.055
  utils.math_utils.clampInt 0 255 (delinearizedValue * 255.0).toInt64.toInt

def argbFromLinrgb (linrgb : Vector Float 3) : Int :=
  let r := delinearized linrgb[0]
  let g := delinearized linrgb[1]
  let b := delinearized linrgb[2]
  argbFromRgb r g b

def alphaFromArgb (argb : Int) : Int :=
  ((argb >>> 24) &&& 255#8).toInt

def redFromArgb (argb : Int) : Int :=
  ((argb >>> 16) &&& 255#8).toInt

def greenFromArgb (argb : Int) : Int :=
  ((argb >>> 8) &&& 255#8).toInt

def blueFromArgb (argb : Int) : Int :=
  (argb &&& 255#8).toInt

def isOpaque (argb : Int) : Bool :=
  alphaFromArgb argb >= 255

def argbFromXyz (x y z : Float) : Int :=
  let matrix := _xyzToSrgb
  let linearR := matrix[0][0] * x + matrix[0][1] * y + matrix[0][2] * z
  let linearG := matrix[1][0] * x + matrix[1][1] * y + matrix[1][2] * z
  let linearB := matrix[2][0] * x + matrix[2][1] * y + matrix[2][2] * z
  let r := delinearized linearR
  let g := delinearized linearG
  let b := delinearized linearB
  argbFromRgb r g b

def xyzFromArgb (argb : Int) : Vector Float 3 :=
  let r := linearized (redFromArgb argb)
  let g := linearized (greenFromArgb argb)
  let b := linearized (blueFromArgb argb)
  utils.math_utils.matrixMultiply #v[r, g, b] _srgbToXyz

def _labF (t : Float) : Float :=
  let e := 216.0 / 24389.0
  let kappa := 24389.0 / 27.0
  if t > e then
    t ^ (1.0 / 3.0)
  else
    (kappa * t + 16.0) / 116.0

def _labInvf (ft : Float) : Float :=
  let e := 216.0 / 24389.0
  let kappa := 24389.0 / 27.0
  let ft3 := ft ^ 3
  if ft3 > e then
    ft3
  else
    (116.0 * ft - 16.0) / kappa

def argbFromLab (l a b : Float) : Int :=
  let whitePoint := _whitePointD65
  let fy := (l + 16) / 116
  let fx := a / 500 + fy
  let fz := fy - b / 200
  let xNormalized := _labInvf fx
  let yNormalized := _labInvf fy
  let zNormalized := _labInvf fz
  let x := xNormalized * whitePoint[0]
  let y := yNormalized * whitePoint[1]
  let z := zNormalized * whitePoint[2]
  argbFromXyz x y z

def labFromArgb (argb : Int) : Vector Float 3 :=
  let linearR := linearized (redFromArgb argb)
  let linearG := linearized (greenFromArgb argb)
  let linearB := linearized (blueFromArgb argb)
  let matrix := _srgbToXyz
  let x := matrix[0][0] * linearR + matrix[0][1] * linearG + matrix[0][2] * linearB
  let y := matrix[1][0] * linearR + matrix[1][1] * linearG + matrix[1][2] * linearB
  let z := matrix[2][0] * linearR + matrix[2][1] * linearG + matrix[2][2] * linearB
  let whitePoint := _whitePointD65
  let xNormalized := x / whitePoint[0]
  let yNormalized := y / whitePoint[1]
  let zNormalized := z / whitePoint[2]
  let fx := _labF xNormalized
  let fy := _labF yNormalized
  let fz := _labF zNormalized
  let l := 116 * fy - 16
  let a := 500 * (fx - fy)
  let b := 200 * (fy - fz)
  #v[l, a, b]

def yFromLstar (lstar : Float) : Float :=
  100 * _labInvf ((lstar + 16) / 116)

def lstarFromY (y : Float) : Float :=
  _labF (y / 100) * 116 - 16

def argbFromLstar (lstar : Float) : Int :=
  let y := yFromLstar lstar
  let component := delinearized y
  argbFromRgb component component component

def lstarFromArgb (argb : Int) : Float :=
  let y := (xyzFromArgb argb)[1]
  116 * _labF (y / 100) - 16

def whitePointD65 : Unit → Vector Float 3 :=
  fun () => _whitePointD65

end utils.color_utils

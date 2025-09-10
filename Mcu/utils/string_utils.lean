import Mcu.utils.color_utils

namespace utils.string_utils

def toHex (value : Int) : String :=
  (BitVec.ofInt 8 value).toHex.toUpper

def hexFromArgb (argb : Int) (leadingHashSign : Bool := true) : String :=
  let red := utils.color_utils.redFromArgb argb
  let green := utils.color_utils.greenFromArgb argb
  let blue := utils.color_utils.blueFromArgb argb
  s!"{if leadingHashSign then "#" else ""}{toHex red}{toHex green}{toHex blue}"

def toHexNat? (s : String) : Option Nat :=
  if s.all (fun c => c.isDigit || ('a' ≤ c ∧ c ≤ 'f') || ('A' ≤ c ∧ c ≤ 'F')) then
    some <| s.foldl (fun n c =>
      let v :=
        if c.isDigit then c.toNat - '0'.toNat
        else if 'a' ≤ c ∧ c ≤ 'f' then 10 + (c.toNat - 'a'.toNat)
        else 10 + (c.toNat - 'A'.toNat)
      n*16 + v
    ) 0
  else
    none

def argbFromHex? (hex : String) : Option Int :=
  let temp := hex.replace "#" ""
  if temp.length = 6 then
    match toHexNat? temp with
    | some rgb => some (Int.ofNat (0xFF000000 ||| rgb))
    | none => none
  else
    none

end utils.string_utils

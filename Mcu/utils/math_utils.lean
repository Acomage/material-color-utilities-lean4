namespace utils.math_utils

def pi : Float := 2 * Float.acos 0

def max (a b : Float) : Float := if a > b then a else b

def min (a b : Float) : Float := if a < b then a else b

def signum (num : Float) : Int :=
  if num < 0 then -1 else if num == 0 then 0 else 1

def lerp (start stop amount : Float) : Float :=
  (1 - amount) * start + amount * stop

def clampInt (min max input : Int) : Int :=
  if input < min then min else if input > max then max else input

def clampDouble (min max input : Float) : Float :=
  if input < min then min else if input > max then max else input

def sanitizeDegreesInt (degrees : Int) : Int := degrees % 360

instance : Mod Float where
  mod a b := a - b * Float.floor (a / b)

def sanitizeDegreesDouble (degrees : Float) : Float := degrees % 360

def rotationDirection (start to : Float) : Int :=
  if sanitizeDegreesDouble (to - start) <= 180 then 1 else -1

def differenceDegrees (a b : Float) : Float :=
  180 - ((a - b).abs - 180).abs

def matrixMultiply (row : Vector Float 3) (matrix : Vector (Vector Float 3) 3) : Vector Float 3 :=
  let a := row[0] * matrix[0][0] + row[1] * matrix[0][1] + row[2] * matrix[0][2]
  let b := row[0] * matrix[1][0] + row[1] * matrix[1][1] + row[2] * matrix[1][2]
  let c := row[0] * matrix[2][0] + row[1] * matrix[2][1] + row[2] * matrix[2][2]
  #v[a, b, c]

end utils.math_utils

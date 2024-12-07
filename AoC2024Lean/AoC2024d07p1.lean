import AoC2024Lean.LittleHelpers

open Std.Internal Parsec Lean.Parser
open Std.Internal.Parsec.String

structure Calibration where
  value : Nat
  numbers : List Nat

partial def Calibration.valid (clbr : Calibration) : Nat :=
  match clbr.numbers with
  | [] => 0
  | n₁ :: ns => match ns with
    | [] => if (n₁ == clbr.value) then 1 else 0
    | n₂ :: ns' =>
      let withAdd : Calibration := {value := clbr.value, numbers := (n₁ + n₂) :: ns'}
      let withMul : Calibration := {value := clbr.value, numbers := (n₁ * n₂) :: ns'}
      withAdd.valid + withMul.valid

def totalCalibration (calibs : List Calibration) : Nat :=
  List.sum <| calibs.map fun c ↦ if c.valid > 0 then c.value else 0

def parseCalibration : Parser Calibration := do
  let val ← digits
  _ ← pstring ": "
  let ns ← digits.separated (pchar ' ')
  return {value := val, numbers := ns}

def parseInput : Parser (List Calibration) := do
  let cs ← many ((attempt $ parseCalibration) <* (many $ pchar '\n'))
  return cs.toList

def main : IO Unit := do
  let input ← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d07p1.txt")
  let calibs := optList (parseInput.run input).toOption
  IO.println <| s!"Total calibration : {totalCalibration calibs}"

--#eval main -- Total calibration : 3749

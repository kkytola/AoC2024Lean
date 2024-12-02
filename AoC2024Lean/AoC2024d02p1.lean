import AoC2024Lean.AoC2024helpers

def safetyCheck (levels : List Int) : Bool :=
  let diffs := (fun p ↦ p.2 - p.1) <$> (levels.drop 1).zip (levels.dropLast)
  let decr := diffs.all (· ≤ 0)
  let incr := diffs.all (0 ≤ ·)
  let somegaps := diffs.all (1 ≤ Int.natAbs ·)
  let smallgaps := diffs.all (Int.natAbs · ≤ 3)
  (decr ∨ incr) ∧ (somegaps ∧ smallgaps)

def parseLineToList (line : String) : List Int :=
  listOpt <| String.toInt? <$> line.splitOn " "

def parseInput (input : String) : List (List Int) :=
  parseLineToList <$> (input.splitOn "\n").filter (· ≠ "")

def main : IO Unit := do
  let input ← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d02p1.txt")
  IO.println s!"Safe reports : {(safetyCheck <$> parseInput input).count true}"

--#eval main -- Safe reports : 2

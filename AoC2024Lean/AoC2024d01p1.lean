import AoC2024Lean.AoC2024helpers

def totalDist (pairs : List (Int × Int)) : Nat :=
  -- Golfs to:
  -- `((fun p ↦ Int.natAbs (p.2 - p.1)) <$> ((((·.1) <$> pairs).mergeSort (· < ·)).zip (((·.2) <$> pairs).mergeSort (· < ·)))).sum`
  let firsts := (pairs.map (·.1)).mergeSort (· < ·)
  let seconds := (pairs.map (·.2)).mergeSort (· < ·)
  let dists := (firsts.zip seconds).map fun p ↦ Int.natAbs (p.2 - p.1)
  dists.sum

def parseLineToPair (line : String) : Option (Int × Int) := do
  let parts := line.splitOn "   "
  some (←(←parts.get? 0).toNat?, ←(←parts.get? 1).toNat?)

def parseInput (input : String) : List (Int × Int) :=
  listOpt <| (input.splitOn "\n").map fun line ↦ parseLineToPair line

def main : IO Unit := do
  let input ← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d01p1.txt")
  IO.println s!"Total distance : {totalDist <| parseInput input}"

--#eval main -- Total distance : 11

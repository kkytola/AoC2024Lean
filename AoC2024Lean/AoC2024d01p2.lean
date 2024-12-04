import AoC2024Lean.LittleHelpers

def similarityScore (pairs : List (Int × Int)) : Int :=
  -- Golfs to:
  -- `((pairs.map (·.1)).map fun x ↦ x * (pairs.map (·.2)).count x).sum`
  let firsts := pairs.map (·.1)
  let seconds := pairs.map (·.2)
  let similarities := firsts.map fun x ↦ x * seconds.count x
  similarities.sum

def parseLineToPair (line : String) : Option (Int × Int) := do
  let parts := line.splitOn "   "
  some (←(←parts.get? 0).toNat?, ←(←parts.get? 1).toNat?)

def parseInput (input : String) : List (Int × Int) :=
  listOpt <| (input.splitOn "\n").map fun line ↦ parseLineToPair line

def main : IO Unit := do
  let input ← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d01p1.txt")
  IO.println s!"Similarity score : {similarityScore <| parseInput input}"

--#eval main -- Similarity score : 31

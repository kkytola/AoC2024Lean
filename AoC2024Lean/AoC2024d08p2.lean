import AoC2024Lean.LittleHelpers

structure Antenna where
  x : Int
  y : Int
  freq : Char

def antennaPairs (la : List Antenna) : List (Antenna × Antenna) :=
  let freqs := (la.map (·.freq)).union []
  (freqs.map fun f ↦ (la.filter (·.freq == f)).pairs).flatten

def antinodes' (a b : Antenna) (maxDispl : Nat) : List (Int × Int) :=
  if a.freq == b.freq then
    let xdiff := b.x - a.x
    let ydiff := b.y - a.y
    List.flatten <| (List.Icc (-maxDispl) maxDispl).map fun m ↦
      [(a.x + m * xdiff, a.y + m * ydiff)]
  else []

structure City where
  height : Nat
  width : Nat
  antennas : List Antenna

def City.validCoord (c : City) (xy : Int × Int) : Bool :=
  0 ≤ xy.1 && 0 ≤ xy.2 && xy.1 < c.width && xy.2 < c.height

def City.countAntinodes' (c : City) : Nat :=
  let apairs := antennaPairs c.antennas
  let anodes := (apairs.map fun ⟨a, b⟩ ↦ antinodes' a b (max c.height c.width)).flatten
  let anodesOnMap := anodes.filter c.validCoord
  (anodesOnMap.union []).length

def readInput (input : String) : City :=
  let lines := (input.trim.splitOn "\n").reverse
  let places : List (List (Char × (Int × Int))) :=
    lines.augmentWithEnum.map fun (line, y) ↦
    line.toList.augmentWithEnum.map fun (⟨c, x⟩ : Char × Nat) ↦ (c, (x, y))
  { height := places.length,
    width := (optList <| places.get? 0).length,
    antennas := List.flatten <| places.map fun row ↦ row.filterMap fun (c, xy) ↦
      if c ≠ '.' then some {x := xy.1, y := xy.2, freq := c} else none }

def main : IO Unit := do
  let input ← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d08p1.txt")
  let city := readInput input
  IO.println <| s!"Unique antinodes on map : {city.countAntinodes'}"

--#eval main -- Unique antinodes on map : 34

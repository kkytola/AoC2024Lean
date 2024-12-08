import AoC2024Lean.LittleHelpers

def cutoff := 1000000

structure Place where
  x : Nat
  y : Nat
  obstacle : Bool

inductive Dir where
  | N | E | S | W
deriving DecidableEq

def Dir.turnRight : Dir → Dir
  | .N => .E | .E => .S | .S => .W | .W => .N

def Dir.toPair : Dir → Int × Int
  | .N => (0, 1) | .E => (1, 0) | .S => (0, -1) | .W => (-1, 0)

structure Guard where
  xy : Int × Int
  facing : Dir
deriving DecidableEq

def Guard.ahead (g : Guard) : Int × Int := g.xy + g.facing.toPair
def Guard.moveAhead (g : Guard) : Guard := {g with xy := g.ahead}
def Guard.turnRight (g : Guard) : Guard := {g with facing := g.facing.turnRight}

abbrev Map := ReaderM (List (List Place))

def Map.height : Map Nat := do pure (← read).length
def Map.width : Map Nat := do pure (optList <| (←read).get? 0).length

def Map.getPlace? (xy : Int × Int) : Map (Option Place) := do
  let map ← read
  pure $ if 0 ≤ xy.1 && 0 ≤ xy.2
    then do some (←(←map.get? xy.2.toNat).get? xy.1.toNat)
    else none

def Map.validCoord (xy : Int × Int) : Map Bool := do
  pure (0 ≤ xy.1 && 0 ≤ xy.2 && (← getPlace? xy).isSome)

def Map.obstacle (xy : Int × Int) : Map Bool := do
  pure $ match ← getPlace? xy with
    | some p => p.obstacle
    | none => false

-- Note that the state has changed from part 1 - keeps track of also past directions.
abbrev Gallivant := StateT (Guard × List Guard) Map

def Gallivant.past : Gallivant (List Guard) := do pure (←get).2
def Gallivant.visited : Gallivant (List (Int × Int)) := do pure <| (←past).map (·.xy)
def Gallivant.guard : Gallivant Guard := do pure (←get).1

def Gallivant.move : Gallivant Unit := do
  let (g, p) ← get
  let forward := g.ahead
  let blocked ← liftM <| Map.obstacle forward
  if blocked then
    let newguard := g.turnRight
    _ ← set (newguard, newguard :: p) -- note that we now record the state even when turning
  else
    let newguard := g.moveAhead
    if ← liftM <| Map.validCoord forward
      then _ ← set (newguard, newguard :: p)
      else _ ← set (newguard, p)
  pure ()

def Gallivant.go : Nat → Gallivant Bool
  | 0 => pure false
  | n + 1 => do
    _ ← Gallivant.move
    let (g, p) ← get
    if (p.drop 1).contains g then pure true
    else if ← liftM <| Map.validCoord g.xy
      then pure <| ← Gallivant.go n
      else pure false

def Gallivant.wouldHaveBlocked : Gallivant (List (Int × Int)) := do
  let map : List (List Place) := ←read
  pure <| ((←past).map fun g ↦ g.ahead).filter fun xy ↦ (Map.validCoord xy).run map

def readInput (input : String) : List (List Place) × Guard :=
  let lines := (input.splitOn "\n").reverse.augmentWithEnum
  let places : List (List (Nat × Nat × Char)) :=
    lines.map fun ⟨line, y⟩ ↦ line.toList.augmentWithEnum.map fun ⟨c, x⟩ ↦ (x, y, c)
  let g := (listOpt <| places.map fun row ↦ row.find? fun ⟨_, _, c⟩ ↦ c == '^').get? 0
  let gx : Nat := optNum <| do pure (←g).1
  let gy : Nat := optNum <| do pure (←g).2.1
  ((·.map fun ⟨x, y, c⟩ ↦ {x := x, y := y, obstacle := c == '#'}) <$> places,
    Guard.mk (gx, gy) Dir.N)

def addBlock (map : List (List Place)) (xy : Int × Int) : List (List Place) :=
  let row := optList <| map.get? xy.2.toNat
  let row' := row.set xy.1.toNat {x := xy.1.toNat, y := xy.2.toNat, obstacle := true}
  map.set xy.2.toNat row'

def main : IO Unit := do
  IO.println ""
  let input ← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d06p1.txt")
  let (map, g) := readInput input.trim
  let possibleBlocks := ((((Gallivant.go cutoff >>= fun _ ↦
    Gallivant.wouldHaveBlocked).run (g, [g])).run map).1.union []).removeAll [g.xy]
  let blockedMaps := possibleBlocks.map fun xy ↦ (addBlock map xy, xy)
  let plans := blockedMaps.map fun bmap ↦ (((Gallivant.go cutoff).run (g, [g])).run bmap.1).1
  IO.println <| s!"Ways to create a loop : {(plans.filter (·)).length}"

--#eval main -- Ways to create a loop : 6

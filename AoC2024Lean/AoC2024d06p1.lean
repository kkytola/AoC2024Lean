import AoC2024Lean.LittleHelpers

def cutoff := 1000000

structure Place where
  x : Nat
  y : Nat
  obstacle : Bool

inductive Dir where
  | N | E | S | W

def Dir.turnRight : Dir → Dir
  | .N => .E | .E => .S | .S => .W | .W => .N

def Dir.toPair : Dir → Int × Int
  | .N => (0, 1) | .E => (1, 0) | .S => (0, -1) | .W => (-1, 0)

structure Guard where
  xy : Int × Int
  facing : Dir

def Guard.ahead (g : Guard) : Int × Int := g.xy + g.facing.toPair
def Guard.moveAhead (g : Guard) : Guard := {g with xy := g.ahead}
def Guard.turnRight (g : Guard) : Guard := {g with facing := g.facing.turnRight}

abbrev Map := ReaderM <| List (List Place)

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

abbrev Gallivant := StateT (Guard × List (Int × Int)) Map

def Gallivant.visited : Gallivant (List (Int × Int)) := do pure (←get).2
def Gallivant.guard : Gallivant Guard := do pure (←get).1

def Gallivant.move : Gallivant Unit := do
  let (g, v) ← get
  let forward := g.ahead
  let blocked ← liftM <| Map.obstacle forward
  if blocked then
    let newguard := g.turnRight
    _ ← set (newguard, v)
  else
    let newguard := g.moveAhead
    if ← liftM <| Map.validCoord forward
      then _ ← set (newguard, forward :: v)
      else _ ← set (newguard, v)
  pure ()

def Gallivant.go : Nat → Gallivant Unit
  | 0 => pure ()
  | n + 1 => do
    _ ← Gallivant.move
    let (g, _) ← get
    if ← liftM <| Map.validCoord g.xy
      then _ ← Gallivant.go n
      else pure ()

def readInput (input : String) : List (List Place) × Guard :=
  let lines := (input.splitOn "\n").reverse.augmentWithEnum
  let places : List (List (Nat × Nat × Char)) :=
    lines.map fun ⟨line, y⟩ ↦ line.toList.augmentWithEnum.map fun ⟨c, x⟩ ↦ (x, y, c)
  let g := (listOpt <| places.map fun row ↦ row.find? fun ⟨_, _, c⟩ ↦ c == '^').get? 0
  let gx : Nat := optNum <| do pure (←g).1
  let gy : Nat := optNum <| do pure (←g).2.1
  ((·.map fun ⟨x, y, c⟩ ↦ {x := x, y := y, obstacle := c == '#'}) <$> places,
    Guard.mk (gx, gy) Dir.N)

def main : IO Unit := do
  let input ← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d06p1.txt")
  let (map, g) := readInput input.trim
  let res := ((Gallivant.go cutoff).run (g, [g.xy])).run map
  IO.println <| s!"Visited positions : {(res.2.2.union []).length}"

--#eval main -- Visited positions : 41

import AoC2024Lean.LittleHelpers

open Std.Internal Parsec Lean.Parser
open Std.Internal.Parsec.String

structure OrderRule where
  before : Nat
  after : Nat

def OrderRule.test (r : OrderRule) (l : List Nat) : Bool :=
  let befores := l.findIdxs (· == r.before)
  let afters := l.findIdxs (· == r.after)
  match (do pure $ (←befores.max?) < (←afters.min?) : Option Bool) with
  | some b => b
  | none => true

def Update := List Nat

def Update.mk (l : List Nat) : Update := l

def Update.middle (u : Update) : Nat := optNum (u.get? (u.length / 2))

def Update.validate (u : Update) (rs : List OrderRule) : Bool :=
  (rs.map (·.test u)).all id

def parseOrderRule : Parser OrderRule := do
  let b ← digits
  _ ← pchar '|'
  let a ← digits
  return {before := b, after := a}

def parseUpdate : Parser Update := do
  return Update.mk (←digits.separated (pchar ','))

def parseInput : Parser $ (List OrderRule) × (List Update) := do
  let ors ← many ((attempt $ parseOrderRule) <* pchar '\n')
  _ ← many $ pchar '\n'
  let us ← many ((attempt $ parseUpdate) <* (many $ pchar '\n'))
  return (ors.toList, us.toList)

def processInput (input : String) : (List OrderRule) × (List Update) :=
  match parseInput.run input with
  | .ok x => x
  | .error _ => ([], [])

def main : IO Unit := do
  let input ← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d05p1.txt")
  let (rules, updates) := processInput input
  let result := (updates.map fun u ↦ if u.validate rules then u.middle else 0).sum
  IO.println <| s!"Sum of valid middle page numbers : {result}"

--#eval main -- Sum of valid middle page numbers : 143

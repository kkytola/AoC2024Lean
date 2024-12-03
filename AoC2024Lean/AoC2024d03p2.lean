import AoC2024Lean.AoC2024helpers

def String.splitOnFirst? (s : String) (c : Char) : Option (String × String) := do
  let fst := s.takeWhile (· != c) -- find the splitting character
  if fst.length == s.length then
    none -- ∄ splitting character
  else
    let snd := s.drop (fst.length + 1) -- the part after the split
    some (fst, snd) -- parts before and after the split

def String.splitOnFirstSubstring? (s : String) (sep : String) : Option (String × String) := do
  let splits : List (String × String) := -- all splits of s to head and tail segments
    (List.range s.length).map fun i ↦ ⟨s.take i, s.drop i⟩
  List.get? (i := 0) <| -- the first occurrence of...
    listOpt <| splits.map fun ⟨head, tail⟩ ↦
      if tail.startsWith sep then -- ...a tail starting with `sep`
        some ⟨head, tail.drop sep.length⟩
      else none -- ignore if the tail does not start with `sep`

def String.openWith? (s : String) (c : Char) : Option String := do
  let split ← s.splitOnFirst? c -- split at the opening character
  if split.1 == "" -- require that the opening character is at the beginning
  then some split.2 -- return the part after the desired opening character
  else none -- not the desired opening character

def String.until? (s : String) (c : Char) : Option String := do
  (← s.splitOnFirst? c).1 -- find the closing character and keep the part until it

def String.toSmallNat? (s : String) : Option Nat := do
  if (s.length ≥ 1 ∧ s.length ≤ 3) -- small means 1-3 digits
  then ← s.toNat? -- try to parse as a natural number
  else none -- the string does not represent a "small" natural number

def String.parseSmallNatPair? (s : String) : Option (Nat × Nat) := do
  let p ← (← (← s.openWith? '(').until? ')').splitOnFirst? ',' -- between parens, comma separated
  some (← p.1.toSmallNat?, ← p.2.toSmallNat?) -- pair of "small" natural numbers

def parseInput (input : String) : List (Nat × Nat) :=
  let offseg := input.splitOn "don't()" -- find positions to turn off
  let onseg := (offseg.take 1) -- initially on, and then...
    ++ (listOpt <| (fun s ↦ ((·.2) <$> s.splitOnFirstSubstring? "do()")) <$> offseg.drop 1)
  List.flatten <| onseg.map fun s ↦
    listOpt <| ((s.splitOn "mul").drop 1).map (·.parseSmallNatPair?)

def addProducts (l : List (Nat × Nat)) : Nat :=
  (l.map fun p ↦ p.1 * p.2).sum

def main : IO Unit := do
  let input ← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d03p2.txt")
  IO.println s!"Added multiplications : {(addProducts <| parseInput input)}"

#eval main -- Added multiplications : 48
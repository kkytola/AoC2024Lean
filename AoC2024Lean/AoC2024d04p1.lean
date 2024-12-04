import AoC2024Lean.AoC2024helpers

def parseInput (input : String) : List (List Char) :=
  String.toList <$> (input.splitOn "\n").filter (·.length > 0)

def height {α : Type} (arr : List (List α)) : Nat :=
  arr.length

def width {α : Type} (arr : List (List α)) : Nat :=
  (optList <| arr.get? 0).length

def atPos' {α : Type} (i j : Int) (arr : List (List α)) : Option α := do
  if (i < 0 ∨ j < 0) then none
  else (optList <| arr.get? i.toNat).get? j.toNat

def atPos (i j : Int) (arr : List (List Char)) : Char :=
  optChar' <| atPos' i j arr

def xmas : String := "XMAS"

def String.tails4 (s : String) : List String :=
  ((· - 3) <$> (List.range s.length).drop 3).map fun i ↦ s.drop i

def xmasCount (s : String) : Nat :=
  ((s.tails4).map (·.startsWith xmas)).count true

def row (i : Int) (arr : List (List Char)) : List Char :=
  optList <| if i < 0 then none else arr.get? i.toNat

def col (j : Int) (arr : List (List Char)) : List Char :=
  (List.Ico 0 arr.length).map fun i ↦ atPos i j arr

def diag (d : Int) (arr : List (List Char)) : List Char :=
  (List.Ico 0 arr.length).map fun i ↦ atPos i (i + d) arr

def adiag (d : Int) (arr : List (List Char)) : List Char :=
  (List.Ico 0 arr.length).map fun i ↦ atPos i (d - i) arr

def rows (arr : List (List Char)) : List (List Char) :=
  (List.Ico 0 arr.length).map fun i ↦ row i arr

def cols (arr : List (List Char)) : List (List Char) :=
  (List.Ico 0 (width arr)).map fun j ↦ col j arr

def diags (arr : List (List Char)) : List (List Char) :=
  (List.Icc (-(Int.ofNat <| height arr)) (width arr)).map fun d ↦ diag d arr

def adiags (arr : List (List Char)) : List (List Char) :=
  (List.Icc 0 (height arr + width arr)).map fun d ↦ adiag d arr

def xmasCountArray (arr : List (List Char)) : Nat :=
  let dirs := ([rows arr, cols arr, diags arr, adiags arr].map
              fun dir ↦ [dir, List.reverse <$> dir].flatten).flatten
  (xmasCount <$> String.trim <$> String.mk <$> dirs).sum

def main : IO Unit := do
  let input := (← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d04p1.txt")).trim
  IO.println <| s!"XMASes : {xmasCountArray <| parseInput input}"

--#eval main -- XMASes : 18

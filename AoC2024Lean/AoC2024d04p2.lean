import AoC2024Lean.AoC2024helpers

def parseInput (input : String) : List (List Char) :=
  String.toList <$> (input.splitOn "\n").filter (·.length > 0)

def height {α : Type} (arr : List (List α)) : Nat :=
  arr.length

def width {α : Type} (arr : List (List α)) : Nat :=
  (optList <| arr.get? 0).length

def atPos' (i j : Int) (arr : List (List Char)) : Option Char := do
  if (i < 0 ∨ j < 0) then none
  else (optList <| arr.get? i.toNat).get? j.toNat

def atPos (i j : Int) (arr : List (List Char)) : Char :=
  optChar' <| atPos' i j arr

def mas : List Char := "MAS".toList

def acrossNWSE (i j : Int) (arr : List (List Char)) : List Char :=
  [-1, 0, 1].map fun t ↦ atPos (i + t) (j + t) arr

def acrossNESW (i j : Int) (arr : List (List Char)) : List Char :=
  [-1, 0, 1].map fun t ↦ atPos (i + t) (j - t) arr

def hasXmasAt (i j : Int) (arr : List (List Char)) : Bool :=
  let nesw := [acrossNESW i j arr, (acrossNESW i j arr).reverse]
  let nwse := [acrossNWSE i j arr, (acrossNWSE i j arr).reverse]
  ((nwse.prod nesw).map fun p ↦ (p.1 == mas && p.2 == mas)).or

def countXmas (arr : List (List Char)) : Nat :=
  ((List.Ico 0 (height arr)).map fun i ↦
    (List.Ico 0 (width arr)).map fun j ↦
      hasXmasAt i j arr).flatten.count true

def main : IO Unit := do
  let input := (← (IO.FS.readFile "./AoC2024Lean/input-AoC2024d04p1.txt")).trim
  IO.println <| s!"X-MASes : {countXmas <| parseInput input}"

--#eval main -- X-MASes : 9
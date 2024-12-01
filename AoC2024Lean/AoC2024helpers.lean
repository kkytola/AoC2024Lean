import Batteries.Data.List

section OptionHandling -- Dirty.

/-- Turn an `Option String` to `String` forcefully, defaulting to the empty string `""`. -/
def optString :  Option String → String
  | none => ""
  | some s => s

/-- Turn an `Option (List X)` to `List X` forcefully, defaulting to the empty list `[]`. -/
def optList {α : Type u} : Option (List α) → List α
  | none => []
  | some l => l

/-- Turn a `List (Option X)` to `List X` omitting `none`s. -/
def listOpt {α : Type u} : List (Option α) → List α
  | [] => []
  | oa :: oas => match oa with
    | none => listOpt oas
    | some a => a :: listOpt oas

/-- Turn an `Option D` to `D` forcefully, when `D` is inhabited, defaulting
to `(default : D)`. -/
def optInhabited {D : Type u} [Inhabited D] : Option D → D
  | none => default
  | some x => x

/-- Turn an `Option R` to `R` forcefully, defaulting to zero `(0 : R)`. -/
def optNum {R : Type u} [Zero R] : Option R → R
  | none => 0
  | some x => x

/-- Turn an `Option Char` to `String` forcefully, defaulting to empty string `""`. -/
def optChar (c : Option Char) : String := optString $ c.map Char.toString

/-- Turn an `Option α × Option β` to `Option (α × β)`, defaulting to `none`. -/
def optPair : Option α × Option β → Option (α × β)
  | (none, _)         => none
  | (_, none)         => none
  | (some a, some b)  => some (a, b)

/-- Turn an `Option α × Option β × Option γ` to `Option (α × β × γ)`, defaulting to `none`. -/
def optTriple (abc' : Option α × Option β × Option γ) : Option (α × β × γ) :=
  optPair $ (fun q => (q.1, optPair q.2)) abc'

end OptionHandling -- section


section List_helpers

/-- To a given list, add a second component to the elements for `Nat`-enumeration of the
elements. -/
def List.augmentWithEnum (l : List α) : List (α × Nat) :=
  l.zip (List.range l.length)

/-- To a given list, add a second component to the elements for positive integer enumeration
of the elements. -/
def List.augmentWithEnum₁ (l : List α) : List (α × Nat) :=
  l.zip ((· + 1) <$> List.range l.length)

/-- The list repeating a given list n times. -/
def List.repeat (l : List α) (n : Nat) : List α :=
  List.flatten $ ((fun _ => l) <$> List.range n)

/-- The list containing n times the same element a. -/
def listRepeat (n : Nat) (a : α) : List α :=
  (fun _ => a) <$> List.range n

/-- An integer interval list with a given starting and end value. -/
def List.Icc (a b : Int) : List Int := (· + a) <$> (Int.ofNat <$> List.range (b-a+1).toNat)

/-- The union of a list of lists. -/
def List.union' [DecidableEq α] : List (List α) → List α
  | [] => []
  | l :: ls => ls.union'.union l

/-- Applying a list of maps sequentially. -/
def applyAll {α : Type u} (funs : List (α → α)) (a : α) : α :=
  match funs with
  | [] => a
  | f :: fs => f (applyAll fs a)

/-- Index of first difference in two lists. -/
def List.firstDiff? {α : Type u} [DecidableEq α] (L₁ L₂ : List α) : Option Nat :=
  let rec helper (A B : List α) (agree : Nat) : Option Nat :=
    match A, B with
    | [], [] => none
    | [], _ :: _ => some (agree + 1)
    | _ :: _, [] => some (agree + 1)
    | a :: as, b :: bs =>
      if a == b
        then helper as bs (agree + 1)
        else some (agree + 1)
  helper L₁ L₂ 0

/-- Index of first occurrence of a pattern in a list. -/
def List.findFirstPattern? {α : Type u} [DecidableEq α] (L : List α) (pat : List α) :
    Option Nat :=
  let aux : List (Option Nat) :=
            (((List.range L.length).map
              fun i => (L.drop i, i)).map
              fun si => (si.fst.firstDiff? pat, si.snd)).map
              fun oi => match oi.fst with
              | none => pure oi.snd
              | some c => if c > pat.length then pure oi.snd else none
  (listOpt aux)[0]?

/-- Run all monadic actions in a list. -/
def runList {m : Type u → Type v} {α : Type u} [Monad m] (as' : List (m α)) :
    m (List α) := do
  match as' with
  | []          => pure []
  | a' :: rest  => do pure ((← a') :: (← runList rest))

end List_helpers -- section


section Nat_helpers

/-- Distances between natural numbers (`ℕ`-valued, computable). -/
def natDist (n m : Nat) := (n - m) + (m - n)

/-- "Print" a natural number in a single character. -/
def Nat.toChar (n : Nat) : Char :=
  if (n > 9) then '#'
    else match (toString n).toList[0]? with
    | none => '.'
    | some c => c

end Nat_helpers -- section


section String_helpers

/-- Remove any copy of a given substring in a string. -/
def String.remove (orig rem : String) : String :=
  orig.replace rem ""

/-- Remove any copy of any of given substrings in a string. -/
def String.removeAny (orig : String) (rems : List String) : String :=
  let rec helper : List String → String → String
    | [], s => s
    | r :: rs, s => helper rs (s.remove r)
  helper rems orig

end String_helpers -- section


section Char_helpers

def Char.isNum (c : Char) : Bool := c.isAlphanum ∧ ¬c.isAlpha

def Char.toDigit (c : Char) : Nat := optNum c.toString.toNat?

end Char_helpers -- section


section Position

/-- A position on the grid: `x` and `y` coordinates. -/
structure Position where
  x : Nat
  y : Nat
deriving BEq, DecidableEq

instance : ToString Position := ⟨fun p => s!"⟨{p.x},{p.y}⟩"⟩

def Position.mkXY (x y : Nat) : Position := {x := x, y := y}

def Position.leLex (p q : Position) : Bool := p.y < q.y ∨ (p.y == q.y ∧ p.x ≤ q.x)

end Position -- section


section Many

/-- A monad of lazily evaluated unordered bunches of things. -/
inductive Many (α : Type u) where
  | none
  | more : α → (Unit → Many α) → Many α

def Many.one (a : α) : Many α := .more a (fun _ => none)

def Many.both : Many α → Many α → Many α
  | none, bs => bs
  | more a as', bs => more a (fun _ => both (as' ()) bs)

def manyMany : Many (Many α) → Many α
  | Many.none         => Many.none
  | Many.more a' as'' => Many.both a' (manyMany (as'' ()))

def Many.bind {α β : Type u} (as : Many α) (f : α → Many β) : Many β :=
  match as with
  | none => none
  | more a as' => both (f a) (bind (as' ()) f)

instance : Monad Many where
  pure := Many.one
  bind := Many.bind

abbrev Many.map (as : Many α) (f : α → β) := f <$> as

def manyOfListMany (ml : List (Many α)) : Many α := match ml with
  | [] => .none
  | ma :: mas => .both ma (manyOfListMany mas)

def Many.ofList (l : List α) : Many α :=
  manyOfListMany (pure <$> l) -- Inefficient?

def Many.toList : Many α → List α
  | none => []
  | more a as' => a :: (as' ()).toList

instance [ToString α] : ToString (Many α) where
  toString as := s!"M{as.toList}"

def Many.ofOption : Option α → Many α
  | .some a => Many.one a
  | .none   => Many.none

def Many.isSingle : Many α → Bool
  | none => false
  | more _ as' => match as' () with
    | none => true
    | _ => false

def Many.find? (as : Many α) (p : α → Bool) : Option α :=
  match as with
  | none => Option.none
  | more a as' => if p a then some a else find? (as' ()) p

def Many.first? (as : Many α) : Option α := as.find? (fun _ => true)

def Many.first [Inhabited α] (as : Many α) : α := match as.first? with
  | some a => a
  | Option.none => default

def Many.count {α : Type u} : Many α → Nat
  | none => 0
  | more _ as' => (as' ()).count + 1

def Many.sum {α : Type u} [Zero α] [Add α] : Many α → α
  | none => 0
  | more a as' => (as' ()).sum + a

/-- Run all monadic actions in a Many. -/
def runMany {m : Type u → Type v} {α : Type u} [Monad m] (as' : Many (m α)) :
    m (Many α) := do
  match as' with
  | Many.none           => pure Many.none
  | Many.more a' rest'  => do
    let rest ← runMany $ rest' ()
    pure $ Many.more (← a') (fun _ => rest)

end Many -- section


section Algebra

instance [Add R] : Add (R × R) where
  add p q := (p.1 + q.1, p.2 + q.2)

end Algebra

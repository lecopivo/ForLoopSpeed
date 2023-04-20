@[unbox]
structure Idx (n : USize) where
  val : USize
  property : val < n
deriving DecidableEq

instance : ToString (Idx n) := ⟨λ i => toString i.1⟩

instance {n} : LT (Idx n) where
  lt a b := a.val < b.val

instance {n} : LE (Idx n) where
  le a b := a.val ≤ b.val

@[inline] instance Idx.decLt {n} (a b : Idx n) : Decidable (a < b) := USize.decLt ..
@[inline] instance Idx.decLe {n} (a b : Idx n) : Decidable (a ≤ b) := USize.decLe ..

def Range (α : Type u) := Option (α × α)
@[inline] def fullRange (n : USize) : Range (Idx n) :=
  if h : 0 < n then
    some (⟨0,h⟩, ⟨n-1,sorry⟩)
  else
    none

@[inline]
partial def Idx.forIn {m : Type → Type} [Monad m] {β : Type} (range : Range (Idx n)) (init : β) (f : Idx n → β → m (ForInStep β)) :=
    let rec @[specialize] forLoop (i : Idx n) (stop : Idx n) (b : β) : m β := do
      match (← f i b) with
      | ForInStep.done b  => pure b
      | ForInStep.yield b => 
        if i < stop then
          forLoop ⟨i.1 + 1, sorry⟩ stop b
        else
          pure b
    match range with
    | some (start, stop) => 
      if start ≤ stop then
        forLoop start stop init
      else
        pure init
    | none => pure init

instance : ForIn m (Range (Idx n)) (Idx n) where
  forIn := Idx.forIn

@[inline] 
partial def Idx.foldM {m : Type → Type} [Monad m] {β : Type} (range : Range (Idx n)) (f : β → Idx n → m β) (init : β) :=
    let rec @[specialize] foldLoop (i : Idx n) (stop : Idx n) (b : β) : m β := do
      let b' ← f b i
      if i < stop then
        foldLoop ⟨i.1+1, sorry⟩ stop b'
      else
        pure b'
    match range with
    | some (start,stop) => 
      if start ≤ stop then 
        foldLoop start stop init 
      else 
        pure init
    | none => pure init

@[inline] 
partial def Idx.fold {β : Type} (range : Range (Idx n)) (f : β → Idx n → β) (init : β) :=
    let rec @[specialize] foldLoop (i : Idx n) (stop : Idx n) (b : β) : β :=
      let b' := f b i
      if i < stop then
        foldLoop ⟨i.1+1, sorry⟩ stop b'
      else
        b'
    match range with
    | some (start,stop) => 
      if start ≤ stop then 
        foldLoop start stop init 
      else 
        init
    | none => init



-- taken from Aesop      
@[inline]
def time [Monad m] [MonadLiftT BaseIO m] (x : m α) : m (α × Nat) := do
  let start ← IO.monoNanosNow
  let a ← x
  let stop ← IO.monoNanosNow
  return (a, stop - start)



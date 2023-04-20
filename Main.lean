import ForLoopSpeed

def triangularNumberForIn (n : USize) : IO USize := do
  let mut sum : USize := 0
  for i in (fullRange n) do
    for _ in fullRange i.1 do
      sum := sum + 1
  pure sum

def triangularNumberFoldM (n : USize) : IO USize := do
  Idx.foldM (fullRange n) (init := 0) λ sum i => 
    Idx.foldM (fullRange i.1) (init := sum) λ sum' i => pure $ sum'+1

def triangularNumberFold (n : USize) : IO USize := pure $
  Idx.fold (fullRange n) (init := 0) λ sum i => 
    Idx.fold (fullRange i.1) (init := sum) λ sum' i => sum'+1

-- taken from Aesop      
@[inline]
def time [Monad m] [MonadLiftT BaseIO m] (x : m α) : m (α × Nat) := do
  let start ← IO.monoNanosNow
  let a ← x
  let stop ← IO.monoNanosNow
  return (a, stop - start)


def testFun (f : USize → IO USize) (n : USize) (msg : String) : IO Unit := do
  
  let (m, t) ← time (f n)

  IO.println s!"Computed {n}-th triangular number {m} with {msg} in: time {t.toFloat/1000000.0}ms"

def main (args : List String) : IO Unit := do
  
  let n := (args.head!.toNat?.getD 1000000).toUSize

  testFun triangularNumberForIn n "for loop"
  testFun triangularNumberFoldM n "foldM"
  testFun triangularNumberFold n "fold"

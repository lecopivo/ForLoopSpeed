import ForLoopSpeed


def triangularNumberNaive (n : USize) : IO USize := pure $ Id.run do
  let mut sum : USize := 0
  for i in [0:n.toNat] do
    for _ in [0:i] do
      sum := sum + 1
  pure sum

def triangularNumberForIn (n : USize) : IO USize := pure $ Id.run do
  let mut sum : USize := 0
  for i in (fullRange n) do
    for _ in fullRange i.1 do
      sum := sum + 1
  pure sum

def triangularNumberFoldM (n : USize) : IO USize := pure $ Id.run do
  Idx.foldM (fullRange n) (init := 0) λ sum i => 
    Idx.foldM (fullRange i.1) (init := sum) λ sum' i => sum'+1

def triangularNumberFold (n : USize) : IO USize := pure $ 
  Idx.fold (fullRange n) (init := 0) λ sum i => 
    Idx.fold (fullRange i.1) (init := sum) λ sum' i => sum'+1

def testFun (f : USize → IO USize) (n : USize) (msg : String) : IO Unit := do
  let (m, t) ← time (f n)
  IO.println s!"Computed {n}-th triangular number {m} with {msg} in: time {t.toFloat/1000000.0}ms"

def main (args : List String) : IO Unit := do
  
  let n := (args.head!.toNat?.getD 1000000).toUSize

  -- testFun triangularNumberNaive n "naive for loop"
  testFun triangularNumberForIn n "for loop"
  testFun triangularNumberFoldM n "foldM"
  testFun triangularNumberFold n "fold"





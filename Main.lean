import ForLoopSpeed

instance : Coe USize Float := ⟨λ n => n.toUInt64.toFloat⟩

def triangularNumberNaive (n : USize) : IO Float := pure $ Id.run do
  let mut sum : Float := 0
  for i in [0:n.toNat] do
    for j in [0:i] do
      sum := sum + Float.sin i.toFloat + Float.cos j.toFloat
  pure sum


def triangularNumberForIn (n : USize) : IO Float := pure $ Id.run do
  let mut sum : Float := 0
  for i in (fullRange n) do
    for j in fullRange i.1 do
      sum := sum + Float.sin i.1 + Float.cos j.1
  pure sum

def triangularNumberFoldM (n : USize) : IO Float := pure $ Id.run do
  Idx.foldM (fullRange n) (init := 0) λ sum i => 
    Idx.foldM (fullRange i.1) (init := sum) λ sum' j => sum'+Float.sin i.1 + Float.cos j.1

def triangularNumberFold (n : USize) : IO Float := pure $ 
  Idx.fold (fullRange n) (init := 0) λ sum i => 
    Idx.fold (fullRange i.1) (init := sum) λ sum' j => sum'+Float.sin i.1 + Float.cos j.1

def testFun (f : USize → IO Float) (n : USize) (msg : String) : IO Unit := do
  let (m, t) ← time (f n)
  IO.println s!"Computing for n := {n}, result := {m}, method := {msg}, time := {t.toFloat/1000000.0}ms"

def main (args : List String) : IO Unit := do
  
  let n := (args.head!.toNat?.getD 1000).toUSize

  -- testFun triangularNumberNaive n "naive for loop"
  testFun triangularNumberForIn n "for loop"
  testFun triangularNumberFoldM n "foldM"
  testFun triangularNumberFold n "fold"





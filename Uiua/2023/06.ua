&i "../lib.ua" "Load"

Solve ← (
  ⍘⊟⇌
  -⊓(×4|ⁿ2.)
  ⊃∘(⌊√)
  =ⁿ2,
  +⊃-(◿2+++1)
)

⊜(⊜parse≠@ .↘+1⊗@:.)≠@\n.
⊃(Solve ≡/(parse⊂∩⍘parse)) (/×≡Solve⍉)

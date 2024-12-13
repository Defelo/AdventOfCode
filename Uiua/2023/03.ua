&fras ⊣ &args

Nums ← (
  ∊+@0⇡10
  ×¬⬚0↻0_¯1..
  ×⍜♭\+
)
Symbols   ← ×⊃(≠@.)(=0Nums)
Gears     ← =@*
Neighbors ← ≡(×⬚0↻)♭₂-1⇡3_3∩¤
A ← (
  Neighbors ⊃Symbols Nums
  ⊏◴♭/↥
)
B ← (
  Neighbors ⊃Nums Gears
  ♭₂⍉
  ⬚0≡(▽=2⧻.▽≠0.◴)
  ≡/×≡⊏⊙¤
)

⊃(⊜∘≠@\n.)(⊂0≡⋕♭ regex "\\d+")
∩/+ ⊃B A

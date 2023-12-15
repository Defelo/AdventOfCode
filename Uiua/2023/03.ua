&fras ⊢⇌ &args

Nums ← (
  ∊:+@0⇡10
  ×¬⬚0↻0_¯1..
  ×⍜♭\+
)
Symbols ← ×⊃(≠@.)(=0Nums)
Gears ← =@*
Neighbors ← ≡(×⬚0↻)☇1-1⇡3_3∩¤
A ← (
  Neighbors ⊃Symbols Nums
  ⊏⊝♭/↥
)
B ← (
  Neighbors ⊃Nums Gears
  ☇1⍉
  ⬚0≡(▽=2⧻.▽≠0.⊝)
  ≡/×≡⊏⊙¤
)

⊃(⊜∘≠@\n.)(⊂0≡⋕♭ regex "\\d+")
∩/+ ⊃B A

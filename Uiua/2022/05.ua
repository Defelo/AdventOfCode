&i "../lib.ua" "Load"

ParseStep ← (
  regex "move (\\d+) from (\\d+) to (\\d+)"
  -0_1_1 ≡⊐⋕↘1⊢
)
ExecStep! ← (
  ⊃(↘1⊙∘)⊢
  ⍜(°⊟⊏)(∩□ ⍜∩↙(:^1) ⊃⋅⋅∘⊙(0∘) ∩°□)
)

∩(≠@\n.°□) °⊟⊜□¬⌕"\n\n".
⊓(⊜∘|⊜ParseStep)
⍜⇌(↘1)
▽↯¤⧻,0_1_0_0⍉
:≡(□▽≠@ .)

⊃∧ExecStep!∘∧ExecStep!⇌
∩(&p≡⊢)

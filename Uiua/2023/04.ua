&i "../lib.ua" "Load"

ParseCard ← ∩⊔⍘⊟ ⊜(□⊜parse≠@ .)≠@|. ↘+2⊗@:.
Matches ← ⧻▽∊,
Points ← ⌊ⁿ⊙2-1
WinMatrix ← ⬚0≡(⊂∩↯⊙(0⊙1)) +1⇡⧻.

⊜(Matches ParseCard)≠@\n.
⊃(
  0 ↯⊙1⧻. WinMatrix
  ⊙(;;) ⍥⊃(+⊙/+)⋅(/+×,)⧻,
)(/+≡Points)

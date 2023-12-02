&i "../lib.ua" "Load"

Colors ← {"red" "green" "blue"}
ParseCount ← ×parse⊔⊃⊢(⬚0↯[3]⍘⊚⊗⊙Colors⊢⇌)⊜□≠@ .
ParseRound ← /+⊜ParseCount≠@,.
ParseGame ← ⊜ParseRound≠@;.

∩/+ ⊃/×(×+1⇡⧻./×≤12_13_14) ⍉⊜(/↥ParseGame↘+2⊗@:.)≠@\n.

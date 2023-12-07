&i "../lib.ua" "Load"

Nums ← {"one" "two" "three" "four" "five"
        "six" "seven" "eight" "nine"}
Digits ← +@1⇡9
Find ← /+ ×+1⇡9 ≡⊐⌕
Extract ← +×10 ∩⊢⊃∘⇌ ▽≠0.

°⊟/+ ⊜(⊟ ∩Extract ⊃↥∘ ∩Find Digits,Nums¤)≠@\n.

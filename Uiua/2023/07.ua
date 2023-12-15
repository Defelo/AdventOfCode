&fras ⊢⇌ &args

ParseCard ← +2⊗⊙"23456789TJQKA"
ParseHand ← ⊂⊓(ParseCard°□|⋕)°⊟ ⊜□≠@ .
CountCards ← ¤⬚0↯[4] ↘1 ⊏⍖. ⊕⧻⊛. ▽≠0.
HandType ← -⊙6⊢⊚ ≡≍[
  0_0_0_0 1_0_0_0
  2_0_0_0 1_1_0_0
  2_1_0_0 1_1_1_0
  1_1_1_1] CountCards
Sort ← ⊏⍏ ≡(⊂HandType.↘¯1) .
ReplaceJokers ← ⍜(↘¯1)(×≠11.)

⊜ParseHand≠@\n.
≡ReplaceJokers.
∩(/+× +1⇡⧻. ⊢⇌⍉ Sort)
